#lang racket

(provide (rename-out [my-read read]
                     [my-read-syntax read-syntax]))

(require "../tokenizer.rkt")
(require syntax/strip-context racket/pretty racket/port)

(define (my-read in)
  (syntax->datum (my-read-syntax #f in)))

(define (my-read-syntax src in)
  (with-syntax ([str (get-syntax src in)])
    (strip-context
     #'(module anything "../src/lang/pyret.rkt"
         str))))
