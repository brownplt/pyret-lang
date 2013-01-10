#lang racket

(provide parse-pyret)

(require
  "tokenizer.rkt"
  racket/runtime-path)

(define-runtime-module-path parser "parser.rkt")
(dynamic-require parser 0)
(define ns (module->namespace (resolved-module-path-name parser)))

;; note - using eval-syntax below misses an important "enrichment" step:
;; http://docs.racket-lang.org/reference/eval.html?q=eval-syntax&q=eval-syntax&q=%23%25datum#(def._((quote._~23~25kernel)._eval-syntax))
;;
;; NB(joe):  I have no idea what that means
(define (parse-pyret str (name "unnamed-pyret-file"))
  (eval
   (get-syntax name (open-input-string str))
   ns))
