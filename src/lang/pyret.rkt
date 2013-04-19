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

(define-runtime-module-path pyret-lang-racket "pyret-lang-racket.rkt")

(define (my-read in)
  (syntax->datum (my-read-syntax #f in)))

(define (bare-read-syntax src in)
  (cond
    [(port-eof? in) eof]
    [else (strip-context (pyret->racket src in #:libs #t))]))

(define (my-read-syntax src in)
  (cond
    [(port-eof? in) eof]
    [else
      (with-syntax
         ([pyret-lang-racket-stx
           (path->string (resolved-module-path-name pyret-lang-racket))])
            (strip-context
              #`(module src (file pyret-lang-racket-stx)
                  (r:require (r:only-in racket/base current-read-interaction current-print void))
                  (void (current-read-interaction repl-eval-pyret))
                  (void (current-print (print-pyret (current-print))))
                  #,(pyret->racket src in #:libs #t #:toplevel #t))))]))

