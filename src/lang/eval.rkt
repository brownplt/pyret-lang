#lang racket

(provide
  repl-eval-pyret
  print-pyret
  pyret->racket
  pyret->racket/libs
  get-py-eval)
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

(define-runtime-path parser "parser.rkt")
(define-runtime-path ast "ast.rkt")
(define-runtime-path runtime "runtime.rkt")
(define-runtime-module-path-index pyret-lang-ix "pyret-lang-racket.rkt")
;(dynamic-require pyret-lang #f)
(define-runtime-path read-root (simplify-path "/"))

(define (get-py-eval) pyret-eval)
(define (pyret-eval stx)
  (define pyret-lang (resolve-module-path-index pyret-lang-ix #f))
  (define make-fresh-namespace (eval
                              '(lambda ()
                                 (variable-reference->empty-namespace
                                  (#%variable-reference)))
                              (make-base-namespace)))
  (define ns (make-fresh-namespace))
  (parameterize ([current-namespace ns])
    (namespace-require pyret-lang))
  (eval stx ns))


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

(define ((print-pyret printer) val)
  (when (not (equal? val nothing))
    (match val
      [(p:p-opaque v) (racket-print v) (newline)]
      [(? p:p-base?) (pretty-write (p:to-string val))]
      [_ (void)])))

