#lang racket/base

(provide
  src->module-name
  repl-eval-pyret
  pyret-to-printable
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
          #:print-desugared [print-desugared (current-print-desugared)])
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
  (define type-checked
    (if type-env
        (contract-check-pyret desugared type-env)
        desugared))
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
          #:type-env [type-env DEFAULT-ENV])
  (define pyret-stx (get-syntax src in))
  (define parsed-stx (parse-eval pyret-stx))
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

(define (simplify-pyret val)
  (match val
    [(? (λ (v) (eq? v nothing))) nothing]
    [(p:p-num _ _ _ _ n) n]
    [(p:p-str _ _ _ _ s) s]
    [(p:p-bool _ _ _ _ b) b]
    [(p:p-object _ d _ _)
     (make-hash (hash-map d (lambda (s v) (cons s (simplify-pyret v)))))]
    [(? p:p-base?) val]
    [_ (void)]))

(define (pyret-to-printable val)
  (when (not (equal? val nothing))
    (match val
      [(p:p-opaque v) v]
      [(? p:p-base?) (p:to-string val)]
      [_ (void)])))


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
          [else (void)])]
        [else
         (printf "~a\n" (p:to-repr val))])]
     [_ (void)])))
