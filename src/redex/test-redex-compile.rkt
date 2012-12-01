#lang racket

(require
  rackunit
  "../tests/test-utils.rkt"
  "pyret-core.rkt"
  "redex-compile.rkt")

(define (compile-to-redex str)
  (redex-compile-pyret (parse-pyret str)))

(compile-to-redex "{}")

