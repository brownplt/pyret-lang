#lang racket/base
(require parser-tools/yacc
         parser-tools/lex
         racket/list
         racket/match
         "rule-structs.rkt")

;; A parser for grammars.

(provide tokens
         token-LPAREN
         token-RPAREN
         token-LBRACKET
         token-RBRACKET
         token-PIPE
         token-REPEAT
         token-RULE_HEAD
         token-ID
         token-LIT
         token-EOF
         grammar-parser

         current-source
         current-parser-error-handler
         
         [struct-out rule]
         [struct-out lhs-id]
         [struct-out pattern]
         [struct-out pattern-id]
         [struct-out pattern-lit]
         [struct-out pattern-token]
         [struct-out pattern-choice]
         [struct-out pattern-repeat]
         [struct-out pattern-maybe]
         [struct-out pattern-seq])

(define-tokens tokens (LPAREN
                       RPAREN
                       LBRACKET
                       RBRACKET
                       PIPE
                       REPEAT
                       RULE_HEAD
                       ID
                       LIT
                       EOF))

;; grammar-parser: (-> token) -> (listof rule)
(define grammar-parser
  (parser
   (tokens tokens)
   (src-pos)
   (start rules)
   (end EOF)

   (grammar
    [rules
     [(rules*) $1]]

    [rules*
     [(rule rules*)
      (cons $1 $2)]
     [()
      '()]]

    ;; I have a separate token type for rule identifiers to avoid the
    ;; shift/reduce conflict that happens with the implicit sequencing
    ;; of top-level rules.  i.e. the parser can't currently tell, when
    ;; it sees an ID, if it should shift or reduce to a new rule.
    [rule
     [(RULE_HEAD pattern)
      (begin 
        (define trimmed (regexp-replace #px"\\s*:$" $1 ""))
        (rule (position->pos $1-start-pos)
              (position->pos $2-end-pos)
              (lhs-id (position->pos $1-start-pos)
                      (pos (+ (position-offset $1-start-pos)
                              (string-length trimmed))
                           (position-line $1-start-pos)
                           (position-col $1-start-pos))
                      trimmed)
              $2))]]

    [pattern
     [(implicit-pattern-sequence PIPE pattern)
      (if (pattern-choice? $3)
          (pattern-choice (position->pos $1-start-pos)
                          (position->pos $3-end-pos)
                          (cons $1 (pattern-choice-vals $3)))
          (pattern-choice (position->pos $1-start-pos)
                          (position->pos $3-end-pos)
                          (list $1 $3)))]
     [(implicit-pattern-sequence)
      $1]]

    [implicit-pattern-sequence
     [(repeatable-pattern implicit-pattern-sequence)
      (if (pattern-seq? $2)
          (pattern-seq (position->pos $1-start-pos)
                       (position->pos $2-end-pos)
                       (cons $1 (pattern-seq-vals $2)))
          (pattern-seq (position->pos $1-start-pos)
                       (position->pos $2-end-pos)
                       (list $1 $2)))]
     [(repeatable-pattern)
      $1]]

    [repeatable-pattern
     [(atomic-pattern REPEAT)
      (cond [(string=? $2 "*")
             (pattern-repeat (position->pos $1-start-pos)
                             (position->pos $2-end-pos)
                             0 $1)]
            [(string=? $2 "+")
             (pattern-repeat (position->pos $1-start-pos)
                             (position->pos $2-end-pos)
                             1 $1)]
            [else
             (error 'grammar-parse "unknown repetition operator ~e" $2)])]
     [(atomic-pattern)
      $1]]

    [atomic-pattern
     [(LIT)
      (pattern-lit (position->pos $1-start-pos)
                   (position->pos $1-end-pos)
                   (substring $1 1 (sub1 (string-length $1))))]
     
     [(ID)
      (if (token-id? $1)
          (pattern-token (position->pos $1-start-pos)
                         (position->pos $1-end-pos)
                         $1)
          (pattern-id (position->pos $1-start-pos)
                      (position->pos $1-end-pos)
                      $1))]

     [(LBRACKET pattern RBRACKET)
      (pattern-maybe (position->pos $1-start-pos)
                     (position->pos $3-end-pos)
                     $2)]
     
     [(LPAREN pattern RPAREN)
      (relocate-pattern $2 (position->pos $1-start-pos) (position->pos $3-end-pos))]])

   
   (error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
            ((current-parser-error-handler) tok-ok? tok-name tok-value (position->pos start-pos) (position->pos end-pos))))))


;; relocate-pattern: pattern -> pattern
;; Rewrites the pattern's start and end pos accordingly.
(define (relocate-pattern a-pat start-pos end-pos)
  (match a-pat
   [(pattern-id _ _ v)
    (pattern-id start-pos end-pos v)]
   [(pattern-token _ _ v)
    (pattern-token start-pos end-pos v)]
   [(pattern-lit _ _ v)
    (pattern-lit start-pos end-pos v)]
   [(pattern-choice _ _ vs)
    (pattern-choice start-pos end-pos vs)]
   [(pattern-repeat _ _ m v)
    (pattern-repeat start-pos end-pos m v)]
   [(pattern-maybe _ _ v)
    (pattern-maybe start-pos end-pos v)]
   [(pattern-seq _ _ vs)
    (pattern-seq start-pos end-pos vs)]
   [else
    (error 'relocate-pattern "Internal error when relocating ~s\n" a-pat)]))


; token-id: string -> boolean
;; Produces true if the id we see should be treated as the name of a token.
;; By convention, tokens are all upper-cased.
(define (token-id? id)
  (string=? (string-upcase id)
            id))



;; position->pos: position -> pos
;; Coerses position structures from parser-tools/lex to our own pos structures.
(define (position->pos a-pos)
  (pos (position-offset a-pos)
       (position-line a-pos)
       (position-col a-pos)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; During parsing, we should define the source of the input.
(define current-source (make-parameter #f))


;; When bad things happen, we need to emit errors with source location.
(struct exn:fail:parse-grammar exn:fail (srclocs)
        #:transparent
        #:property prop:exn:srclocs (lambda (instance)
                                      (exn:fail:parse-grammar-srclocs instance)))

(define current-parser-error-handler
  (make-parameter
   (lambda (tok-ok? tok-name tok-value start-pos end-pos)
     (raise (exn:fail:parse-grammar
             (format "Error while parsing grammar near: ~e [line=~a, column=~a, position=~a]"
                     tok-value
                     (pos-line start-pos)
                     (pos-col start-pos)
                     (pos-offset start-pos))
             (current-continuation-marks)
             (list (srcloc (current-source)
                           (pos-line start-pos)
                           (pos-col start-pos)
                           (pos-offset start-pos)
                           (if (and (number? (pos-offset end-pos))
                                    (number? (pos-offset start-pos)))
                               (- (pos-offset end-pos)
                                  (pos-offset start-pos))
                               #f))))))))
