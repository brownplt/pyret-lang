#lang racket

(provide parse-pyret)

(require
  "tokenizer.rkt"
  racket/runtime-path
  racket/sandbox)

(define-runtime-path parser "parser.rkt")
(define-runtime-path ast "ast.rkt")
(define-runtime-path pyret-base-path (simplify-path (build-path "." 'up 'up)))

(define py-eval
  (let ([specs (sandbox-namespace-specs)])
    (parameterize [(sandbox-namespace-specs (cons make-base-namespace
                                                  (list ast)))
                   (sandbox-path-permissions `((exists ,pyret-base-path)))]
      (make-evaluator 'racket/base #:requires (list ast parser)))))


(define (parse-pyret str (name "unnamed-pyret-file"))
  (py-eval (get-syntax name (open-input-string str))))

