#lang whalesong

(require
  "../runtime.rkt"
  "../string-map.rkt"
  "../ffi-helpers.rkt"
  (prefix-in i: whalesong/image)
  (prefix-in w: whalesong/world))
(provide (all-defined-out))

(define rectangle (ffi-wrap i:rectangle))
(define circle (ffi-wrap i:circle))
(define triangle (ffi-wrap i:triangle))
(define ellipse (ffi-wrap i:ellipse))
(define star (ffi-wrap i:star))
(define text (ffi-wrap i:text))
(define scale (ffi-wrap i:scale))
(define rotate (ffi-wrap i:rotate))
(define place-image (ffi-wrap i:place-image))
(define put-image (ffi-wrap i:place-image))
(define bitmap-url (ffi-wrap i:bitmap/url))
(define Image (ffi-wrap i:image?))

(define pyret-random (ffi-wrap random))

(define (%animate init obj)
  (define the-dict (p:get-dict obj))
  (define handlers
    (string-map-map the-dict (lambda (key val)
      (define racket-val (ffi-unwrap val))
      (cond
        [(equal? key "on-key") (w:on-key racket-val)]
        [(equal? key "on-mouse") (w:on-mouse racket-val)]
        [(equal? key "on-tick") (w:on-tick racket-val)]
        [(equal? key "to-draw") (w:to-draw racket-val)]
        [(equal? key "stop-when") (w:stop-when racket-val)]))))
  (apply w:big-bang (cons init handlers)))

(define animate (ffi-wrap %animate))
