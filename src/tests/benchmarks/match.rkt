#lang racket/base

(require racket/match)

(struct var1 (a b))
(struct var2 (a b c))
(struct var3 ())

(define (matcher v)
  (match v
    [(var1 a b) (+ a b)]
    [(var2 a b c) (* a b c)]
    [(var3) 42]))


(define sum (for/fold ((sum 0)) ((i (in-range 0 10000)))
  (define choice (modulo i 3))
  (+ sum
    (cond
      [(= choice 0) (matcher (var1 1 2))]
      [(= choice 1) (matcher (var2 1 2 3))]
      [(= choice 2) (matcher (var3))]))))

(printf "~a\n" sum)
