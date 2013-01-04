#lang racket/base
(require ragg/examples/simple-arithmetic-grammar
         ragg/support
         parser-tools/lex
         racket/list
         rackunit)

(define (tokenize ip)
  (port-count-lines! ip)
  (define lex/1
    (lexer-src-pos
     [(repetition 1 +inf.0 numeric)
      (token 'INT (string->number lexeme))]
     [whitespace
      (token 'WHITESPACE #:whitespace? #t)]
     ["+"
      (token '+ "+")]
     ["*"
      (token '* "*")]
     [(eof)
      (token eof)]))
  (lambda ()
    (lex/1 ip)))


;; expr : term ('+' term)*
;; term : factor (('*') factor)*
;; factor : INT

(check-equal? (syntax->datum (parse #f (tokenize (open-input-string "42"))))
              '(expr (term (factor 42))))
(check-equal? (syntax->datum (parse #f (tokenize (open-input-string "3+4"))))
              '(expr (term (factor 3))
                     "+"
                     (term (factor 4))))
(check-equal? (syntax->datum (parse #f (tokenize (open-input-string "3+4+5"))))
              '(expr (term (factor 3))
                     "+"
                     (term (factor 4))
                     "+"
                     (term (factor 5))))


(check-equal? (syntax->datum (parse #f (tokenize (open-input-string "3*4*5"))))
              '(expr (term (factor 3) "*" (factor 4) "*" (factor 5))))


(check-equal? (syntax->datum (parse #f (tokenize (open-input-string "3*4 + 5*6"))))
              '(expr (term (factor 3) "*" (factor 4))
                     "+" 
                     (term (factor 5) "*" (factor 6))))

(check-equal? (syntax->datum (parse #f (tokenize (open-input-string "4*5+6"))))
              '(expr (term (factor 4) "*" (factor 5))
                     "+"
                     (term (factor 6))))

(check-equal? (syntax->datum (parse #f (tokenize (open-input-string "4+5   *6"))))
              '(expr (term (factor 4))
                     "+"
                     (term (factor 5) "*" (factor 6))))


(check-exn exn:fail:parsing?
           (lambda () (parse #f (tokenize (open-input-string "7+")))))
(check-exn exn:fail:parsing?
           (lambda () (parse #f (tokenize (open-input-string "7+6+")))))
