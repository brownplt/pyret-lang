#lang racket

(provide eval-pyret print-pyret)
(require "tokenizer.rkt" "compile.rkt" "runtime.rkt")
(require racket/runtime-path)

(define-runtime-module-path parser "parser.rkt")
(dynamic-require parser 0)
(define ns (module->namespace (resolved-module-path-name parser)))

(define (eval-pyret src in)
  ;; the parameterize is stolen from 
  ;; http://docs.racket-lang.org/reference/eval.html?q=current-read&q=%23%3Atop-interactoin#(def._((quote._~23~25kernel)._current-read-interaction))
  (parameterize ([read-accept-reader #t]
                 [read-accept-lang #f])
    (if (eof-object? (peek-char in))
        eof
        (compile-pyret (eval (get-syntax src in) ns)))))

(define (print-pyret val)
  (match val
    [(p-num _ _ _ _ n) (pretty-write n)]
    [(p-str _ _ _ _ s) (pretty-write s)]
    [(p-bool _ _ _ _ b) (pretty-write b)]
    [_ (pretty-write val)]))

