#lang racket

(require
  rackunit
  rackunit/text-ui
  "test-utils.rkt"
  "../lang/runtime.rkt")

(define all (test-suite "all"

(check-pyret-exn
 "true and not false"
 "well-formedness:")

(check-pyret
 "true and false and true"
 p:p-false)

(check-pyret-exn
 "true and false or true"
 "well-formedness:")

(check-pyret-exn
 "true and false and not true and false"
 "well-formedness:")

(check-pyret
 "1 + 2 + 3 + 4"
 (p:mk-num 10))

(check-pyret-exn
 "1 + 2 - 3"
 "well-formedness:")

(check-pyret-exn
 "1 + 2 + 3 * 4"
 "well-formedness:")

(check-pyret-exn
 "1 / 2 + 3 * 4 - 5"
 "well-formedness:")

))

(run-tests all)
