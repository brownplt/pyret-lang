#lang racket/base

(define (mklist)
  (map (lambda (i) (* i 7))
       (build-list 10000 (lambda (x) x))))
(equal? (mklist) (mklist))
