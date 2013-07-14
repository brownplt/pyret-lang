#lang racket/base

(require
  "../runtime.rkt"
  "../string-map.rkt"
  "../ffi-helpers.rkt"
  (only-in 2htdp/image
    text
    place-image
    circle
    rectangle
    star
    triangle
    isosceles-triangle
    rotate
    bitmap/url
    bitmap/file
    save-image
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
  (make-string-map
    (list
      (cons "place-image" (ffi-wrap place-image))
      (cons "circle" (ffi-wrap circle))
      (cons "star" (ffi-wrap star))
      (cons "isosceles-triangle" (ffi-wrap isosceles-triangle))
      (cons "triangle" (ffi-wrap triangle))
      (cons "rotate" (ffi-wrap rotate))
      (cons "rectangle" (ffi-wrap rectangle))
      (cons "bitmap-url" (ffi-wrap bitmap/url))
      (cons "bitmap-file" (ffi-wrap bitmap/file))
      (cons "save-file" (ffi-wrap save-image))
      (cons "Image" (ffi-wrap image?))
      (cons "image-width" (ffi-wrap image-width))
      (cons "image-to-color-list"
            (ffi-wrap image->color-list))
      (cons "color-list-to-bitmap"
            (ffi-wrap color-list->bitmap))
      (cons "Color" (ffi-wrap color?))
      (cons "color-red" (ffi-wrap color-red))
      (cons "color-green" (ffi-wrap color-green))
      (cons "color-blue" (ffi-wrap color-blue))
      (cons "text" (ffi-wrap text))
      (cons "rgb" (ffi-wrap make-color)))))

(define image-obj (p:mk-object image-dict))

