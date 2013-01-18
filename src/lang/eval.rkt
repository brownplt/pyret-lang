#lang racket

(provide
  repl-eval-pyret
  print-pyret
  pyret->racket)
(require
  racket/runtime-path
  syntax/strip-context
  "tokenizer.rkt"
  "desugar.rkt"
  "typecheck.rkt"
  "compile.rkt"
  "runtime.rkt")

(define-runtime-module-path parser "parser.rkt")
(dynamic-require parser 0)
(define ns (module->namespace (resolved-module-path-name parser)))

(define (stx->racket stx)
  (strip-context
   (compile-pyret
    (contract-check-pyret
     (desugar-pyret
      (eval stx ns))))))

(define (pyret->racket src in)
  (stx->racket (get-syntax src in)))

;; for the repl, we parse statement by statement
(define (pyret->racket-repl src in)
  (stx->racket (get-stmt-syntax src in)))

(define (repl-eval-pyret src in)
  ;; the parameterize is stolen from 
  ;; http://docs.racket-lang.org/reference/eval.html?q=current-read&q=%23%3Atop-interactoin#(def._((quote._~23~25kernel)._current-read-interaction))
  (parameterize ([read-accept-reader #t]
                 [read-accept-lang #f])
    (if (eof-object? (peek-char in))
        eof
        (pyret->racket-repl src in))))

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
