#lang racket/base

(require
  racket/match
  racket/list
  "../runtime.rkt"
  "../ffi-helpers.rkt")

(provide (rename-out [Racket %PYRET-PROVIDE]))

(define (apply-racket-fun package-name package-member args)
  (define package (string->symbol package-name))
  (define fun (dynamic-require package (string->symbol package-member)))
  (define result (apply fun (map get-val args)))
  (wrap-racket-value result))

;; mk-racket-fun : String -> Value
(define (mk-racket-fun f)
  (p:mk-fun-nodoc
    (λ args
      (match (cons f (first args))
        [(cons (p:p-str _ _ f) (p:p-str _ _ s))
         (p:wrap (apply-racket-fun f s (map p:unwrap (rest args))))]
        [else
         (error (format "Racket: expected string as first argument, got ~a" (first args)))]))))

(define Racket (p:mk-fun mk-racket-fun "Racket ffi"))

