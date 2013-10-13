#lang racket

(provide
  current-indentation-mode
  current-check-mode
  current-whalesong-repl-print
  current-allow-shadowed-vars
  current-mark-mode
  current-compile-lift-constants)

(define current-check-mode (make-parameter #f))
(define current-indentation-mode (make-parameter #t))
(define current-whalesong-repl-print (make-parameter #t))
(define current-allow-shadowed-vars (make-parameter #f))
(define current-mark-mode (make-parameter #t))
(define current-compile-lift-constants (make-parameter #t))
