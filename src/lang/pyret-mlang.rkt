#lang racket

(provide (rename-out [my-read read]
                     [my-read-syntax read-syntax]))

(require "parser.rkt" "compile-pyret.rkt")
(require (for-syntax "pyret.rkt"))
(require syntax/strip-context racket/pretty racket/port)

(dynamic-require "../src/lang/pyret.rkt" 0)
(define ns (module->namespace "../src/lang/pyret.rkt"))

(define (my-read in)
  (syntax->datum (my-read-syntax #f in)))

(define (my-read-syntax src in)
  (with-syntax ([stx (compile-pyret (eval (get-syntax src in) ns))])
    #'(module src racket
        (require "../src/lang/values.rkt")
        stx)))
        