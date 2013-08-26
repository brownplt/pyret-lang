#lang racket

(provide
  current-indentation-mode
  current-check-mode
  current-whalesong-repl-print)

(define current-check-mode (make-parameter #f))
(define current-indentation-mode (make-parameter #t))
(define current-whalesong-repl-print (make-parameter #t))

