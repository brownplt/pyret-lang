#lang racket/base

(provide
  src->module-name
  repl-eval-pyret
  pyret-to-printable
  print-pyret
  pyret->racket)
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
  "get-syntax.rkt"
  "desugar.rkt"
  "desugar-check.rkt"
  "typecheck.rkt"
  "compile.rkt"
  "load.rkt"
  "runtime.rkt")

(define (stx->racket stx desugar)
  (strip-context
   (compile-expr
    (contract-check-pyret
     (desugar
      (parse-eval stx))))))

(define (pyret->racket src in #:toplevel [toplevel #f] #:check [check #f])
  (define desugar
    (cond
      [check (lambda (e) (desugar-pyret (desugar-check e)))]
      [else desugar-pyret]))
  (define compile (if toplevel compile-pyret compile-expr))
  (define pyret-stx (get-syntax src in))
  (define parsed-stx (parse-eval pyret-stx))
  (define desugared (desugar parsed-stx))
  (define compiled (compile (contract-check-pyret desugared)))
  (strip-context compiled))

(define (repl-eval-pyret src in)
  ;; the parameterize is stolen from 
  ;; http://docs.racket-lang.org/reference/eval.html?(def._((quote._~23~25kernel)._current-read-interaction))
  (parameterize ([read-accept-reader #t]
                 [read-accept-lang #f])
    (if (not (byte-ready? in))
        eof
        (pyret->racket src in #:toplevel #t))))

(define (simplify-pyret val)
  (match val
    [(? (λ (v) (eq? v nothing))) nothing]
    [(p:p-num _ _ _ n) n]
    [(p:p-str _ _ _ s) s]
    [(p:p-bool _ _ _ b) b]
    [(p:p-object _ d _)
     (make-hash (hash-map d (lambda (s v) (cons s (simplify-pyret v)))))]
    [(? p:p-base?) val]
    [_ (void)]))

(define (pyret-to-printable val)
  (when (not (equal? val nothing))
    (match val
      [(p:p-opaque v) v]
      [(? p:p-base?) (p:to-string val)]
      [_ (void)])))


(define (print-pyret val)
  (when (not (equal? val nothing))
    (match val
      [(p:p-opaque v) (racket-print v) (newline)]
      [(? p:p-base?) (printf "~a\n" (p:to-string val))]
      [_ (void)])))

