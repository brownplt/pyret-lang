#lang racket

(provide
  current-indentation-mode
  current-check-mode)

(define current-check-mode (make-parameter #f))
(define current-indentation-mode (make-parameter #t))

