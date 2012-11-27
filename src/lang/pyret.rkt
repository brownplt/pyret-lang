#lang racket

(provide (rename-out [my-read read]
                     [my-read-syntax read-syntax]))

(require "tokenizer.rkt" "compile.rkt")
(require racket/runtime-path)

(define-runtime-module-path parser "parser.rkt")
(define-runtime-module-path runtime "runtime.rkt")

(dynamic-require parser 0)
(define ns (module->namespace (resolved-module-path-name parser)))

(define (my-read in)
  (syntax->datum (my-read-syntax #f in)))

(define (my-read-syntax src in)
  (with-syntax ([stx (compile-pyret (eval (get-syntax src in) ns))]
                [runtime-stx (path->string (resolved-module-path-name runtime))])
    #'(module src racket
        (require (file runtime-stx))
        stx
        )))
