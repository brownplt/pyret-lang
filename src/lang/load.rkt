#lang racket

(provide parse-pyret)

(require
  "tokenizer.rkt"
  racket/runtime-path
  racket/sandbox)

(define-runtime-path parser "parser.rkt")
(define-runtime-path ast "ast.rkt")

(define py-eval
  (let ([specs (sandbox-namespace-specs)])
    (parameterize [(sandbox-namespace-specs (cons make-base-namespace
                                                  (list ast)))
                   (sandbox-path-permissions `((exists "./")))]
      (make-evaluator 'racket/base #:requires (list ast parser)))))


;; note - using eval-syntax below misses an important "enrichment" step:
;; http://docs.racket-lang.org/reference/eval.html?q=eval-syntax&q=eval-syntax&q=%23%25datum#(def._((quote._~23~25kernel)._eval-syntax))
;;
;; NB(joe):  I have no idea what that means
(define (parse-pyret str (name "unnamed-pyret-file"))
  (py-eval (get-syntax name (open-input-string str))))

