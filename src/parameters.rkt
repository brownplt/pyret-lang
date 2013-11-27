#lang racket

(provide
  current-where-everywhere
  current-indentation-mode
  current-check-mode
  current-whalesong-repl-print
  current-allow-shadowed-vars
  current-mark-mode
  current-compile-lift-constants
  current-print-desugared
  current-print-typed-core
  current-print-hints
  command-line-arguments)

(define current-where-everywhere (make-parameter #f))
(define current-check-mode (make-parameter #f))
(define current-indentation-mode (make-parameter #t))
(define current-whalesong-repl-print (make-parameter #t))
(define current-allow-shadowed-vars (make-parameter #f))
(define current-mark-mode (make-parameter #t))
(define current-compile-lift-constants (make-parameter #t))
(define current-print-desugared (make-parameter #f))
(define current-print-typed-core (make-parameter #f))
(define current-print-hints (make-parameter #f))
(define command-line-arguments (make-parameter empty))
