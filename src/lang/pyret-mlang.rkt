#lang racket

(provide (rename-out [my-read read]
                     [my-read-syntax read-syntax]))

(require "parser.rkt" "compile-pyret.rkt")
(require racket/runtime-path)

(define-runtime-module-path pyret "pyret.rkt")
(define-runtime-module-path values "values.rkt")

(dynamic-require pyret 0)
(define ns (module->namespace (resolved-module-path-name pyret)))

(define (my-read in)
  (syntax->datum (my-read-syntax #f in)))

(define (my-read-syntax src in)
  (with-syntax ([stx (compile-pyret (eval (get-syntax src in) ns))]
                [values-stx (path->string (resolved-module-path-name values))])
    #'(module src racket
        (require (file values-stx))
        stx
        )))
