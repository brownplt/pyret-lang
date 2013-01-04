#lang racket/base

(require racket/match 
         racket/generator
         (prefix-in lex: parser-tools/lex) 
         ragg/support)


(provide (all-defined-out))



;; The level of indirection here is necessary since the yacc grammar wants a
;; function value for the error handler up front.  We want to delay that decision
;; till parse time.
(define (THE-ERROR-HANDLER tok-ok? tok-name tok-value start-pos end-pos)
  (match (positions->srcloc start-pos end-pos)
    [(list src line col offset span)
     ((current-parser-error-handler) tok-name 
                                     tok-value
                                     offset
                                     line
                                     col 
                                     span)]))




(define no-position (lex:position #f #f #f))
(define (no-position? p)
  (not 
   (or (lex:position-line p)
       (lex:position-col p)
       (lex:position-offset p))))


;; make-permissive-tokenizer: (U (sequenceof (U token token-struct eof void)) (-> (U token token-struct eof void))) hash -> (-> position-token)
;; Creates a tokenizer from the given value.
;; FIXME: clean up code.
(define (make-permissive-tokenizer tokenizer token-type-hash)
  (define tokenizer-thunk (cond
                           [(sequence? tokenizer)
                            (sequence->generator tokenizer)]
                           [(procedure? tokenizer)
                            tokenizer]))

  (define (permissive-tokenizer)
    (define next-token (tokenizer-thunk))
    (let loop ([next-token next-token])
    (match next-token
      [(or (? eof-object?) (? void?))
       (lex:position-token ((hash-ref token-type-hash 'EOF) eof)
                           no-position 
                           no-position)]

      [(? symbol?)
       (lex:position-token ((hash-ref token-type-hash next-token) next-token)
                           no-position
                           no-position)]

      [(? string?)
       (lex:position-token ((hash-ref token-type-hash
                                      (string->symbol next-token))
                            next-token)
                           no-position
                           no-position)]

      [(? char?)
       (lex:position-token ((hash-ref token-type-hash
                                      (string->symbol (string next-token)))
                            next-token)
                           no-position
                           no-position)]
      
      [(token-struct type val offset line column span whitespace?)
       (cond [whitespace?
              ;; skip whitespace, and just tokenize again.
              (permissive-tokenizer)]
             
             [(hash-has-key? token-type-hash type)
              (lex:position-token ((hash-ref token-type-hash type) val)
                                  (lex:position offset line column)
                                  ;; try to synthesize a consistent end position.
                                  (lex:position (if (and (number? offset) (number? span))
                                                    (+ offset span)
                                                    offset)
                                                line
                                                (if (and (number? column) (number? span))
                                                    (+ column span)
                                                    column)))]
             [else
              ;; We ran into a token of unrecognized type.  Let's raise an appropriate error.
              ((current-tokenizer-error-handler) type val 
               offset line column span)])]
      
      [(lex:position-token t s e)
       (define a-position-token (loop t))
       (lex:position-token (lex:position-token-token a-position-token)
                           (if (no-position? (lex:position-token-start-pos a-position-token))
                               s
                               (lex:position-token-start-pos a-position-token))
                           (if (no-position? (lex:position-token-end-pos a-position-token))
                               e
                               (lex:position-token-end-pos a-position-token)))]
      
      [else
       ;; Otherwise, we have no idea how to treat this as a token.
       ((current-tokenizer-error-handler) 'unknown-type (format "~a" next-token)
        #f #f #f #f)])))
  permissive-tokenizer)

                      

;; positions->srcloc: position position -> (list source line column offset span)
;; Given two positions, returns a srcloc-like structure, where srcloc is the value
;; consumed as the third argument to datum->syntax.
(define (positions->srcloc start-pos end-pos)
  (list (current-source)
        (lex:position-line start-pos)
        (lex:position-col start-pos)
        (lex:position-offset start-pos)
        (if (and (number? (lex:position-offset end-pos))
                 (number? (lex:position-offset start-pos)))
            (- (lex:position-offset end-pos)
               (lex:position-offset start-pos))
            #f)))

;; d->s: datum position position
;; Helper that does the ugly work in wrapping a datum into a syntax
;; with source location.
(define (d->s d start-pos end-pos)
  (datum->syntax #f d (positions->srcloc start-pos end-pos)))
