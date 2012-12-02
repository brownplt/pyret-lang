#lang racket

(require
  rackunit
  redex
  "../tests/test-utils.rkt"
  "pyret-core.rkt"
  "redex-compile.rkt")

(define (compile-to-redex str)
  (redex-compile-pyret (parse-pyret str)))

(define (valid? e)
  (define t (apply-reduction-relation* eval-πret (term (() () ,e))))
  (when (> (length t) 1) (error (format "So many ambiguouses: ~a" t)))
  ((term-match πret [(σ Σ e) (first t)]) (first t)))

(valid? (compile-to-redex "{}"))
(compile-to-redex "{nested:{}}.nested")
(valid? (compile-to-redex "{nested:{}}.nested"))