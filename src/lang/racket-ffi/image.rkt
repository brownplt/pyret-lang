#lang racket/base

(require
  "../runtime.rkt"
  (only-in 2htdp/image
    place-image
    circle
    rectangle
    star
    triangle
    isosceles-triangle
    rotate))

(provide (rename-out [image-obj %PYRET-PROVIDE]))

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

(define image-dict
  (make-immutable-hash
    (list
      (cons "place-image" (wrap-racket-fun place-image))
      (cons "circle" (wrap-racket-fun circle))
      (cons "star" (wrap-racket-fun star))
      (cons "isosceles-triangle" (wrap-racket-fun isosceles-triangle))
      (cons "triangle" (wrap-racket-fun triangle))
      (cons "rotate" (wrap-racket-fun rotate))
      (cons "rectangle" (wrap-racket-fun rectangle)))))

(define image-obj (p:mk-object image-dict))

