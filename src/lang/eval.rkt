#lang racket

(provide
  repl-eval-pyret
  print-pyret
  pyret->racket
  pyret->racket/libs)
(require
  racket/sandbox
  racket/runtime-path
  syntax/strip-context
  "tokenizer.rkt"
  "desugar.rkt"
  "typecheck.rkt"
  "compile.rkt"
  "runtime.rkt")

(define-runtime-path parser "parser.rkt")
(define-runtime-path ast "ast.rkt")
(define-runtime-path pyret-base-path (simplify-path (build-path "." 'up 'up)))

(define py-eval
  (let ([specs (sandbox-namespace-specs)])
    (parameterize [(sandbox-namespace-specs (cons make-base-namespace
                                                  (list ast)))
                   (sandbox-path-permissions `((exists ,pyret-base-path)))]
      (make-evaluator 'racket/base #:requires (list ast parser)))))

(define (stx->racket stx desugar)
  (strip-context
   (compile-pyret
    (contract-check-pyret
     (desugar
      (py-eval stx))))))

(define (pyret->racket src in)
  (stx->racket (get-syntax src in) desugar-pyret))

(define (pyret->racket/libs src in)
  (stx->racket (get-syntax src in) desugar-pyret/libs))

(define (repl-eval-pyret src in)
  ;; the parameterize is stolen from 
  ;; http://docs.racket-lang.org/reference/eval.html?(def._((quote._~23~25kernel)._current-read-interaction))
  (parameterize ([read-accept-reader #t]
                 [read-accept-lang #f])
    (if (eof-object? (peek-char in))
        eof
        (pyret->racket src in))))

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
    (pretty-write (simplify-pyret val))))
