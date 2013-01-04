#lang s-exp syntax/module-reader
ragg/codegen/sexp-based-lang
#:read my-read
#:read-syntax my-read-syntax
#:info my-get-info
#:whole-body-readers? #t

(require ragg/rules/parser
         ragg/rules/lexer
         ragg/rules/stx
         ragg/rules/rule-structs)

(define (my-read in)
  (syntax->datum (my-read-syntax #f in)))

(define (my-read-syntax src in)
  (define-values (first-line first-column first-position) (port-next-location in))
  (define tokenizer (tokenize in))
  (define rules 
    (parameterize ([current-parser-error-handler
                    (lambda (tok-ok? tok-name tok-value start-pos end-pos)
                      (raise-syntax-error 
                       #f
                       (format "Error while parsing grammar near: ~a [line=~a, column=~a, position=~a]"
                               tok-value
                               (pos-line start-pos)
                               (pos-col start-pos)
                               (pos-offset start-pos))
                       (datum->syntax #f
                                      (string->symbol (format "~a" tok-value))
                                      (list src
                                            (pos-line start-pos)
                                            (pos-col start-pos)
                                            (pos-offset start-pos)
                                            (if (and (number? (pos-offset end-pos))
                                                     (number? (pos-offset start-pos)))
                                                (- (pos-offset end-pos)
                                                   (pos-offset start-pos))
                                                #f)))))])
      (grammar-parser tokenizer)))
  (define-values (last-line last-column last-position) (port-next-location in))
  (list (rules->stx src rules 
                    #:original-stx (datum->syntax #f 'original-stx
                                                  (list src 
                                                        first-line 
                                                        first-column 
                                                        first-position
                                                        (if (and (number? last-position)
                                                                 (number? first-position))
                                                            (- last-position first-position)
                                                            #f))))))


;; Extension: we'd like to cooperate with DrRacket and tell
;; it to use the default, textual lexer and color scheme when
;; editing bf programs.
;;
;; See: http://docs.racket-lang.org/guide/language-get-info.html
;; for more details, as well as the documentation in
;; syntax/module-reader.
(define (my-get-info key default default-filter)
  (case key
    [(color-lexer)
     (dynamic-require 'syntax-color/default-lexer
                      'default-lexer)]
    [else
     (default-filter key default)]))

