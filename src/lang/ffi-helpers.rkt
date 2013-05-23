#lang racket/base

(require "runtime.rkt")
(provide (all-defined-out))

(define (allowed-prim? v)
  (or (number? v)
      (string? v)
      (boolean? v)))

(define (wrap-racket-value val)
  (cond
   [(allowed-prim? val)  val]
   [(list? val) (map wrap-racket-value val)]
   [else (p:p-opaque val)]))

(define (wrap-racket-fun f)
  (p:mk-fun-nodoc (λ args (p:wrap (wrap-racket-value (apply f (map get-val (map p:unwrap args))))))))

(define (get-val arg)
  (cond
    [(p:p-opaque? arg) (p:p-opaque-val arg)]
    [(allowed-prim? arg) arg]
    [else (error (format "apply-racket-fun: Bad argument ~a." arg))]))

