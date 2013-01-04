#lang racket/base

(require (for-template racket/base)
         racket/list
         racket/set
         (prefix-in support: ragg/support)
         syntax/srcloc
         ragg/rules/stx-types
         "flatten.rkt"
         (prefix-in stxparse: syntax/parse))

(provide rules-codegen)



(define (rules-codegen stx #:parser-provider-module [parser-provider-module 'parser-tools/yacc])
  (syntax-case stx ()
    [(_  r ...)
     (begin
       ;; (listof stx)
       (define rules (syntax->list #'(r ...)))

       (when (empty? rules)
         (raise-syntax-error 'ragg
                             (format "The grammar does not appear to have any rules")
                             stx))


       ;; We flatten the rules so we can use the yacc-style ruleset that parser-tools
       ;; supports.
       (define flattened-rules (flatten-rules rules))

       (define generated-rule-codes (map flat-rule->yacc-rule flattened-rules))
       
       ;; The first rule, by default, is the start rule.
       (define rule-ids (for/list ([a-rule (in-list rules)])
                          (syntax-case a-rule (rule)
                            [(rule id pattern)
                             #'id])))
       (define start-id (first rule-ids))
       
       
       (define-values (implicit-tokens    ;; (listof string-stx)
                       explicit-tokens)   ;; (listof identifier-stx)
         (rules-collect-token-types rules))

       ;; (listof symbol)
       (define implicit-token-types
         (map string->symbol
              (set->list (list->set (map syntax-e implicit-tokens)))))

       ;; (listof symbol)
       (define explicit-token-types
         (set->list (list->set (map syntax-e explicit-tokens))))

       ;; (listof symbol)
       (define token-types
         (set->list (list->set (append (map (lambda (x) (string->symbol (syntax-e x)))
                                            implicit-tokens)
                                       (map syntax-e explicit-tokens)))))
       
       (with-syntax ([start-id start-id]

                     [(token-type ...) token-types]

                     [(token-type-constructor ...)
                      (map (lambda (x) (string->symbol (format "token-~a" x)))
                           token-types)]

                     [(explicit-token-types ...) explicit-token-types]
                     [(implicit-token-types ...) implicit-token-types]
                     [(implicit-token-types-str ...) (map symbol->string implicit-token-types)]
                     [(implicit-token-type-constructor ...)
                      (map (lambda (x) (string->symbol (format "token-~a" x)))
                           implicit-token-types)]
                     [generated-grammar #`(grammar #,@generated-rule-codes)]
                     [parser-module parser-provider-module])

         (quasisyntax/loc stx
           (begin             
             (require parser-tools/lex
                      parser-module
                      ragg/codegen/runtime
                      ragg/support
                      (for-syntax syntax/parse racket/base))
             
             (provide parse
                      make-rule-parser
                      ;; default-lex/1

                      all-tokens-hash

                      #;current-source
                      #;current-parser-error-handler
                      #;current-tokenizer-error-handler
                      #;[struct-out exn:fail:parsing]
                      )

             (define-tokens enumerated-tokens (EOF token-type ...))

             (define all-tokens-hash 
               (make-immutable-hash (list (cons 'EOF token-EOF)
                                          (cons 'token-type token-type-constructor) ...)))

             ;; For internal use by the permissive tokenizer only:
             (define all-tokens-hash/mutable
               (make-hash (list (cons 'EOF token-EOF)
                                ;; Note: we also allow the eof object here, to make
                                ;; the permissive tokenizer even nicer to work with.
                                (cons eof token-EOF) 
                                (cons 'token-type token-type-constructor) ...)))

             
             #;(define default-lex/1
                 (lexer-src-pos [implicit-token-types-str
                                 (token 'implicit-token-types lexeme)]
                                ...
                                [(eof) (token eof)]))

             (define-syntax (make-rule-parser stx-2)
               (syntax-parse stx-2
                 [(_ start-rule:id)
                  (begin
                    ;; HACK HACK HACK
                    ;; The cfg-parser depends on the start-rule provided in (start ...) to have the same
                    ;; context as the rest of this body, so I need to hack this.  I don't like this, but
                    ;; I don't know what else to do.  Hence recolored-start-rule.
                    (define recolored-start-rule (datum->syntax (syntax #,stx) (syntax-e #'start-rule)))
                    #`(let ([THE-GRAMMAR (parser (tokens enumerated-tokens)
                                                 (src-pos)
                                                 (start #,recolored-start-rule)
                                                 (end EOF)
                                                 (error THE-ERROR-HANDLER)
                                                 generated-grammar)])
                        (case-lambda [(tokenizer)
                                      (define next-token
                                        (make-permissive-tokenizer tokenizer all-tokens-hash/mutable))
                                      (THE-GRAMMAR next-token)]
                                     [(source tokenizer)
                                      (parameterize ([current-source source])
                                        (parse tokenizer))])))]))
             
             (define parse (make-rule-parser start-id))))))]))


;; Given a flattened rule, returns a syntax for the code that
;; preserves as much source location as possible.
;;
;; Each rule is defined to return a list with the following structure:
;;
;;     stx :== (name (U tokens rule-stx) ...)
;;
(define (flat-rule->yacc-rule a-flat-rule)
  (syntax-case a-flat-rule ()
    [(rule-type origin name clauses ...)
     (begin
       (define translated-clauses
         (map (lambda (clause) (translate-clause clause #'name #'origin))
              (syntax->list #'(clauses ...))))
       (with-syntax ([(translated-clause ...) translated-clauses])
         #`[name translated-clause ...]))]))

  

;; translates a single primitive rule clause.
;; A clause is a simple list of ids, lit, vals, and inferred-id elements.
;; The action taken depends on the pattern type.
(define (translate-clause a-clause rule-name/false origin)
  (define translated-patterns
    (let loop ([primitive-patterns (syntax->list a-clause)])
      (cond
       [(empty? primitive-patterns)
        '()]
       [else
        (cons (syntax-case (first primitive-patterns) (id lit token inferred-id)
                [(id val)
                 #'val]
                [(lit val)
                 (datum->syntax #f (string->symbol (syntax-e #'val)) #'val)]
                [(token val)
                 #'val]
                [(inferred-id val reason)
                 #'val])
              (loop (rest primitive-patterns)))])))
  
  (define translated-actions
    (for/list ([translated-pattern (in-list translated-patterns)]
               [primitive-pattern (syntax->list a-clause)]
               [pos (in-naturals 1)])
      (with-syntax ([$X (datum->syntax translated-pattern (string->symbol (format "$~a" pos)))]
                    [$X-start-pos (datum->syntax translated-pattern (string->symbol (format "$~a-start-pos" pos)))]
                    [$X-end-pos (datum->syntax translated-pattern (string->symbol (format "$~a-end-pos" pos)))])        
        (syntax-case primitive-pattern (id lit token inferred-id)
          ;; When a rule usage is inferred, the value of $X is a syntax object
          ;; whose head is the name of the inferred rule . We strip that out,
          ;; leaving the residue to be absorbed.
          [(inferred-id val reason)
           #'(syntax-case $X ()
               [(inferred-rule-name . rest)
                (syntax->list #'rest)])]
          [(id val)
           #`(list $X)]
          [(lit val)
           #`(list (d->s $X $X-start-pos $X-end-pos))]
          [(token val)
           #`(list (d->s $X $X-start-pos $X-end-pos))]))))

  (define whole-rule-loc
    (if (> (length translated-patterns) 0)
        (with-syntax ([$1-start-pos
                       (datum->syntax (first translated-patterns)
                                      (string->symbol "$1-start-pos"))]
                      [$n-end-pos
                       (datum->syntax (last translated-patterns)
                                      (string->symbol (format "$~a-end-pos"
                                                              (length translated-patterns))))])
          #`(positions->srcloc $1-start-pos $n-end-pos))
        #'(list (current-source) #f #f #f #f)))
  
  (with-syntax ([(translated-pattern ...) translated-patterns]
                [(translated-action ...) translated-actions])
    #`[(translated-pattern ...)
       (datum->syntax #f
                      (append (list (datum->syntax #f '#,rule-name/false #,whole-rule-loc))
                              translated-action ...)
                      #,whole-rule-loc)]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; collect-token-types: (listof rule-syntax) -> (values (listof identifier) (listof identifier))
;;
;; Given a rule, automatically derive the list of implicit and
;; explicit token types we need to generate.
;; 
(define (rules-collect-token-types rules)
  (define-values (implicit explicit)
    (for/fold ([implicit '()]
               [explicit '()])
        ([r (in-list rules)])
      (rule-collect-token-types r implicit explicit)))
  (values (reverse implicit) (reverse explicit)))
  
(define (rule-collect-token-types a-rule implicit explicit)
  (syntax-case a-rule (rule)
    [(rule id a-pattern)
     (pattern-collect-implicit-token-types #'a-pattern implicit explicit)]))

(define (pattern-collect-implicit-token-types a-pattern implicit explicit)
  (let loop ([a-pattern a-pattern]
             [implicit implicit]
             [explicit explicit])
    (syntax-case a-pattern (id lit token choice repeat maybe seq)
      [(id val)
       (values implicit explicit)]
      [(lit val)
       (values (cons #'val implicit) explicit)]
      [(token val)
       (values implicit (cons #'val explicit))]
      [(choice vals ...)
       (for/fold ([implicit implicit]
                  [explicit explicit])
                 ([v (in-list (syntax->list #'(vals ...)))])
         (loop v implicit explicit))]
      [(repeat min val)
       (loop #'val implicit explicit)]
      [(maybe val)
       (loop #'val implicit explicit)]
      [(seq vals ...)
       (for/fold ([implicit implicit]
                  [explicit explicit])
                 ([v (in-list (syntax->list #'(vals ...)))])
         (loop v implicit explicit))])))


