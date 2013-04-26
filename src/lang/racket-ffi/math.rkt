#lang racket/base

(require
  "../runtime.rkt"
  (only-in math uniform-dist sample))

(define (allowed-prim? v)
  (or (number? v)
      (string? v)
      (boolean? v)))

(define (wrap-racket-value val)
  (cond
   [(allowed-prim? val)  val]
   [else (p:p-opaque val)]))
(define (get-val arg)
  (cond
    [(p:p-opaque? arg) (p:p-opaque-val arg)]
    [(allowed-prim? arg) arg]
    [else (error (format "apply-racket-fun: Bad argument ~a." arg))]))

(define (wrap-racket-fun f)
  (p:mk-fun-nodoc (λ args (p:wrap (wrap-racket-value (apply f (map get-val (map p:unwrap args))))))))

(define math-dict
  (make-immutable-hash
    (list
      (cons "uniform-dist" (wrap-racket-fun uniform-dist))
      (cons "sample" (wrap-racket-fun sample)))))

(define math-obj (p:mk-object math-dict))

(provide (rename-out %PYRET-PROVIDE math-obj))

