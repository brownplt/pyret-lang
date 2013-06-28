#lang whalesong

(require "../whalesong/js-map.rkt")

(provide (all-defined-out))

(define make-string-map make-js-map)
(define (string-map-ref obj-map str
          (default (lambda () (raise (format "Key not found: ~a" str)))))
  (cond
    [(js-map-has-key? obj-map str) (js-map-get obj-map str)]
    [else (default)]))
(define string-map-set js-map-set)
(define string-map-set* js-map-set*)
(define string-map-keys js-map-keys)
(define string-map-has-key? js-map-has-key?)

(define (string-map-map obj-map f)
  (map (lambda (key) (f key (js-map-get obj-map key)))
       (string-map-keys obj-map)))

