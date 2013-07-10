#lang racket/base

(define (build-some-lists n)
  (cond
    [(<= n 0) '()]
    [(> n 0)
     (cons (build-list 100 (lambda (x) (* x x)))
           (build-some-lists (- n 1)))]))

(void (build-some-lists 1000))

