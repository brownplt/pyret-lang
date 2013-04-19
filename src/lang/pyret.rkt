#lang racket

(provide
  bare-read-syntax
  (rename-out [my-read read]
              [my-read-syntax read-syntax]))

(require
  racket/runtime-path
  syntax/strip-context
  (only-in rnrs/io/ports-6 port-eof?)
  "tokenizer.rkt"
  "compile.rkt"
  "desugar.rkt"
  "typecheck.rkt"
  "eval.rkt")

(define-runtime-module-path parser "parser.rkt")
(define-runtime-module-path pyret-lang "pyret-lang.rkt")
(define-runtime-module-path full-eval "eval.rkt")


(define (my-read in)
  (syntax->datum (my-read-syntax #f in)))

(define (bare-read-syntax src in)
  (cond
    [(port-eof? in) eof]
    [else (strip-context (pyret->racket/libs src in))]))

(define (my-read-syntax src in)
  (cond
    [(port-eof? in) eof]
    [else
      (with-syntax
         ([pyret-lang-stx (path->string (resolved-module-path-name pyret-lang))]
          [full-eval-stx (path->string (resolved-module-path-name full-eval))])
            (strip-context
              #`(module src (file pyret-lang-stx)
                  ;(require (file full-eval-stx))
                  ;(current-read-interaction repl-eval-pyret)
                  ;(void (current-print (print-pyret (current-print))))
                  #,(bare-read-syntax src in))))]))

