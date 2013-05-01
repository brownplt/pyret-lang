#lang racket

(provide
  repl-eval-pyret
  print-pyret
  pyret->racket
  pyret->racket/libs)
(require
  (only-in racket/base [print racket-print])
  racket/sandbox
  racket/runtime-path
  syntax/modresolve
  syntax/strip-context
  "tokenizer.rkt"
  "desugar.rkt"
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

(define (pyret->racket/libs src in)
  (stx->racket (get-syntax src in) desugar-pyret/libs))

(define (pyret->racket src in #:libs [libs #f] #:toplevel [toplevel #f])
  (define desugar (if libs desugar-pyret/libs desugar-pyret))
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
    (if (eof-object? (peek-char in))
        eof
        (pyret->racket src in #:toplevel #t))))

(define (simplify-pyret val)
  (match val
    [(? (λ (v) (eq? v nothing))) nothing]
    [(p:p-num _ _ _ n) n]
    [(p:p-str _ _ _ s) s]
    [(p:p-bool _ _ _ b) b]
    [(p:p-object (p:none) _ d)
     (make-hash (hash-map d (lambda (s v) (cons s (simplify-pyret v)))))]
    [(p:p-object (? set? s) _ d)
     (make-hash (set-map s (lambda (s) (cons s (simplify-pyret (hash-ref d s))))))]
    [(? p:p-base?) val]
    [_ (void)]))

(define (print-pyret val)
  (when (not (equal? val nothing))
    (match val
      [(p:p-opaque v) (racket-print v) (newline)]
      [(? p:p-base?) (pretty-write (p:to-string val))]
      [_ (void)])))

