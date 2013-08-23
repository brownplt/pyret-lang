#lang racket/base

(provide
  bare-read-syntax
  (rename-out [my-read read]
              [my-read-syntax read-syntax]))

(require
  racket/runtime-path
  syntax/strip-context
  (only-in rnrs/io/ports-6 port-eof?)
  "../parameters.rkt"
  "type-env.rkt"
  "compile.rkt"
  "desugar.rkt"
  "typecheck.rkt"
  "eval.rkt")

(define-runtime-module-path pyret-lang-racket "pyret-lang-racket.rkt")

(define (my-read in)
  (syntax->datum (my-read-syntax #f in)))

; NOTE(joe): Only the first file gets compiled in check mode, so this helper
; returns true the first time, and sets the parameter to be false after
(define (test-check-mode)
  (define old-value (current-check-mode))
  (current-check-mode #f)
  old-value)

(define (bare-read-syntax src in #:check [check #f] #:type-env [env DEFAULT-ENV])
  (cond
    [(port-eof? in) eof]
    [else (strip-context (pyret->racket src in #:check check #:type-env env))]))

(define (my-read-syntax src in)
  (cond
    [(port-eof? in) eof]
    [else
      (with-syntax
         ([pyret-lang-racket-stx
           (path->string (resolved-module-path-name pyret-lang-racket))]
          [src-syntax (src->module-name src)])
            (strip-context
              #`(module src-syntax (file pyret-lang-racket-stx)
                  (r:require (r:only-in racket/base current-read-interaction current-print void))
                  (void (current-read-interaction repl-eval-pyret))
                  (void (current-print (print-pyret #,(current-check-mode))))
                  #,(pyret->racket src in #:toplevel #t #:check (test-check-mode)))))]))

