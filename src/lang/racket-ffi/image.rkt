#lang racket/base

(require
  "../runtime.rkt"
  "../ffi-helpers.rkt"
  (only-in 2htdp/image
    place-image
    circle
    rectangle
    star
    triangle
    isosceles-triangle
    rotate))

(provide (rename-out [image-obj %PYRET-PROVIDE]))

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

