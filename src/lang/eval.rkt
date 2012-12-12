
#lang racket

(provide
  repl-eval-pyret
  print-pyret
  pyret->racket)
(require
  racket/runtime-path
  "tokenizer.rkt"
  "desugar.rkt"
  "typecheck.rkt"
  "compile.rkt"
  "runtime.rkt")

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



(define (print-pyret val)
  (pretty-write (p:simplify-pyret val)))
