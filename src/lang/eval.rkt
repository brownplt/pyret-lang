#lang racket

(provide repl-eval-pyret print-pyret pyret->racket)
(require "tokenizer.rkt" "desugar.rkt" "typecheck.rkt" "compile.rkt" "runtime.rkt")
(require racket/runtime-path)

(define-runtime-module-path parser "parser.rkt")
(dynamic-require parser 0)
(define ns (module->namespace (resolved-module-path-name parser)))

(define (pyret->racket src in)
  (compile-pyret
    (typecheck-pyret
      (desugar-pyret
        (eval (get-syntax src in) ns)))))

(define (repl-eval-pyret src in)
  ;; the parameterize is stolen from 
  ;; http://docs.racket-lang.org/reference/eval.html?q=current-read&q=%23%3Atop-interactoin#(def._((quote._~23~25kernel)._current-read-interaction))
  (parameterize ([read-accept-reader #t]
                 [read-accept-lang #f])
    (if (eof-object? (peek-char in))
        eof
        (pyret->racket src in))))

(define (simplify-pyret val)
  (match val
    [(p-num _ _ _ _ n) n]
    [(p-str _ _ _ _ s) s]
    [(p-bool _ _ _ _ b) b]
    [(p-object (none) _ _ d)
     (make-hash (hash-map d (lambda (s v) (cons s (simplify-pyret v)))))]
    [(p-object (? set? s) _ _ d)
     (make-hash (set-map s (lambda (s) (cons s(simplify-pyret (hash-ref d s))))))]
    [(? p-base?) val]
    [_ (void)]))

(define (print-pyret val)
  (pretty-write (simplify-pyret val)))
