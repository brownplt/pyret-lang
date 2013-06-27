#lang whalesong

(require
  "../runtime.rkt"
  "../string-map.rkt"
  "../ffi-helpers.rkt"
  (prefix-in i: whalesong/image)
  (prefix-in w: whalesong/web-world))
(provide (all-defined-out))

(define circle (ffi-wrap i:circle))
(define rectangle (ffi-wrap i:rectangle))
(define place-image (ffi-wrap i:place-image))

(define pyret-random (ffi-wrap random))

(define (%animate init obj)
  (define the-dict (p:get-dict obj))
  (define handlers
    (string-map-map the-dict (lambda (key val)
      (define racket-val (ffi-unwrap val))
      (cond
        [(equal? key "on-tick") (w:on-tick/f racket-val)]
        [(equal? key "to-draw") (w:to-draw/f racket-val)]
        [(equal? key "stop-when") (w:stop-when/f racket-val)]))))
  (apply w:big-bang/f (cons init handlers)))

(define animate (ffi-wrap %animate))
