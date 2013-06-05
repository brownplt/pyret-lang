#lang racket/base

(require racket/match "runtime.rkt")
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

(define (ffi-wrap val)
  (cond
    [(procedure? val)
     (p:mk-fun-nodoc
      (lambda args
        (ffi-wrap (apply val (map ffi-unwrap args)))))]
    [(number? val) (p:mk-num val)]
    [(string? val) (p:mk-str val)]
    [(boolean? val) (p:mk-bool val)]
    [else (p:p-opaque val)]))

(define (ffi-unwrap val)
  (match val
    [(p:p-fun _ _ f)
     (lambda args (ffi-unwrap ((f p:dummy-loc) (map ffi-wrap args))))]
    [(p:p-opaque v) v]
    [(p:p-num _ _ n) n]
    [(p:p-str _ _ s) s]
    [(p:p-bool _ _ b) b]
    [else val]))

(define (wrap-racket-fun f)
  (p:mk-fun-nodoc (λ args (p:wrap (wrap-racket-value (apply f (map get-val (map p:unwrap args))))))))

(define (get-val arg)
  (cond
    [(p:p-opaque? arg) (p:p-opaque-val arg)]
    [(list? arg) (map get-val arg)]
    [(allowed-prim? arg) arg]
    [else (error (format "apply-racket-fun: Bad argument ~a." arg))]))

