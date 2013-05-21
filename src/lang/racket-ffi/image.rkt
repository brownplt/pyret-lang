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
    rotate
    bitmap/url
    bitmap/file
    image-width
    image?
    image->color-list
    color-list->bitmap
    color?
    make-color
    color-red
    color-blue
    color-green))

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
      (cons "rectangle" (wrap-racket-fun rectangle))
      (cons "bitmap-url" (wrap-racket-fun bitmap/url))
      (cons "bitmap-file" (wrap-racket-fun bitmap/file))
      (cons "Image" (wrap-racket-fun image?))
      (cons "image-width" (wrap-racket-fun image-width))
      (cons "image-to-color-list"
            (wrap-racket-fun image->color-list))
      (cons "color-list-to-bitmap"
            (wrap-racket-fun color-list->bitmap))
      (cons "Color" (wrap-racket-fun color?))
      (cons "color-red" (wrap-racket-fun color-red))
      (cons "color-green" (wrap-racket-fun color-green))
      (cons "color-blue" (wrap-racket-fun color-blue))
      (cons "rgb" (wrap-racket-fun make-color)))))

(define image-obj (p:mk-object image-dict))

