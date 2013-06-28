#lang whalesong

(provide (all-defined-out))

(define make-js-map make-immutable-hash)
(define js-map-set hash-set)
(define (js-map-set* jsmap args)
  (foldr (lambda (pair jsmap) (js-map-set jsmap (car pair) (cdr pair)))
         jsmap
         args))
(define js-map-get hash-ref)
(define js-map-keys hash-keys)
(define js-map-has-key? hash-has-key?)
