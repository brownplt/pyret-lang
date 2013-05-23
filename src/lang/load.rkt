#lang racket

(provide parse-pyret (rename-out [py-eval parse-eval]))

(require
  "get-syntax.rkt"
  "parser2.rkt"
  racket/runtime-path)

#;(module test-shell racket/base
  (define-namespace-anchor test-shell-anchor)
  (provide test-shell-anchor))
#;(require (submod "." test-shell))

#;(define-runtime-path parser "parser.rkt")
#;(define-runtime-path ast "parser.rkt")

#;(define parse-namespace (namespace-anchor->empty-namespace test-shell-anchor))
#;(parameterize ([current-namespace parse-namespace])
  (namespace-require parser)
  (namespace-require ast))

(define (py-eval stx)
  (parse-program stx))
;  (eval stx parse-namespace))

#;(define (parse-pyret str (name "unnamed-pyret-file"))
  (py-eval (get-syntax name (open-input-string str))))

(define (parse-pyret str (name "unnamed-pyret-file"))
  (parse-program (get-syntax name (open-input-string str))))

