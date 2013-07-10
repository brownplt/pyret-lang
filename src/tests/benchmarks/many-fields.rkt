#lang racket/base

(define (make-a-hash)
  (for/fold ((the-hash (make-immutable-hash)))
            ((i (build-list 10000 (lambda (x) x))))
    (hash-set the-hash (number->string i) i)))
(equal? (make-a-hash) (make-a-hash))

