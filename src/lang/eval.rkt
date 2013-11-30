#lang racket/base

(provide
  src->module-name
  repl-eval-pyret
  print-pyret
  pyret->racket
  stx->racket)
(require
  (only-in racket/bool false?)
  racket/match
  racket/pretty
  (only-in racket/base [print racket-print])
  racket/sandbox
  racket/runtime-path
  syntax/modresolve
  syntax/strip-context
  "ast.rkt"
  "../parameters.rkt"
  "type-env.rkt"
  "get-syntax.rkt"
  "desugar.rkt"
  "desugar-check.rkt"
  "types-compile.rkt"
  "typecheck.rkt"
  "well-formed.rkt"
  "indentation.rkt"
  "compile.rkt"
  "load.rkt"
  "pretty.rkt"
  "runtime.rkt")

(define (stx->racket
          pyret-stx
          #:toplevel [toplevel #f]
          #:check [check (current-check-mode)]
          #:indentation [indentation (current-indentation-mode)]
          #:type-env [type-env WHALESONG-ENV]
          #:print-desugared [print-desugared (current-print-desugared)]
          #:print-typed-core [print-typed-core (current-print-typed-core)])
  (define desugar
    (cond
      [check (lambda (e) (desugar-pyret (desugar-check e)))]
      [else desugar-pyret]))
  (define compile (if toplevel compile-pyret compile-expr))
  (define well-formed-stx (well-formed pyret-stx))
  (define indentation-stx (if indentation
                              (indentation-check well-formed-stx)
                              well-formed-stx))
  (define desugared (desugar indentation-stx))
  (when print-typed-core
      (printf "\n[pyret typed core]\n\n~a\n\n[rest follows]\n\n" (pretty desugared)))
  (define typeless (types-compile-pyret desugared))
  (define type-checked
    (if type-env
        (contract-check-pyret typeless type-env)
        typeless))
  (when print-desugared
      (printf "\n[pyret desugared]\n\n~a\n\n[code running follows]\n\n" (pretty type-checked)))
  (define compiled (compile type-checked))
  (strip-context compiled))

(define (pyret->racket
          src
          in
          #:toplevel [toplevel #f]
          #:check [check (current-check-mode)]
          #:indentation [indentation (current-indentation-mode)]
          #:type-env [type-env WHALESONG-ENV])
  (define parsed-stx (parse-pyret/port in src))
  (stx->racket
    parsed-stx
    #:toplevel toplevel
    #:check check
    #:indentation indentation
    #:type-env type-env))

(define (repl-eval-pyret src in)
  ;; the parameterize is stolen from
  ;; http://docs.racket-lang.org/reference/eval.html?(def._((quote._~23~25kernel)._current-read-interaction))
  (parameterize ([read-accept-reader #t]
                 [read-accept-lang #f]
                 [current-compile-lift-constants #f])
    (if (not (byte-ready? in))
        eof
        (pyret->racket src in #:toplevel #t #:type-env #f #:check #f))))

(define ((print-pyret check-mode) val)
  (when (not (equal? val nothing))
   (match val
     [(p:p-opaque v) (racket-print v) (newline)]
     [(? p:p-base?)
      (cond
        [check-mode
         (cond
          [(p:has-field? val "format")
           ((p:p-base-method (p:get-raw-field p:dummy-loc val "format")) val)]
          [else (printf "~a\n" (p:to-repr val))])]
        [else
         (printf "~a\n" (p:to-repr val))])]
     [_ (void)])))
