#lang racket/base

(provide
  bare-read-syntax
  get-info
  (rename-out [my-read read]
              [my-read-syntax read-syntax]))

(require
  racket/runtime-path
  syntax/strip-context
  (only-in rnrs/io/ports-6 port-eof?)
  "../../parameters.rkt"
  "../../lang/type-env.rkt"
  "../../lang/compile.rkt"
  "../../lang/desugar.rkt"
  "../../lang/typecheck.rkt"
  "../../lang/eval.rkt"
  "../../color-tokenizer.rkt")

(define (get-info in mod line col pos)
  (lambda (key default)
    (case key
      [(color-lexer) get-token]
      [else default])))

(define-runtime-module-path pyret-lang-racket "../../lang/pyret-lang-racket.rkt")

(define (my-read in)
  (syntax->datum (my-read-syntax #f in)))

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
                  (void (current-print (print-pyret #t)))
                  #,(pyret->racket src in #:type-env WHALESONG-ENV #:toplevel #t #:check #t))))]))

