#lang racket

(require
  rackunit
  rackunit/text-ui
  "test-utils.rkt"
  "../lang/runtime.rkt")

(define all (test-suite "all"

(check-pyret-exn/indent
 "1 1"
 "indentation:")

(check-pyret-exn/indent
 "fun foo():
   1 2
  end"
 "indentation:")

(check-pyret/indent
 "1
  1"
 (p:mk-num 1))

(check-pyret-match/indent
 "fun foo(): 1 end
  foo"
 (p:p-fun _ _ _ _))

(check-pyret-exn/indent
 "1 + 1 2"
 "indentation:")

(check-pyret-exn/indent
 "when true: 1 1 end"
 "indentation:")

))

(run-tests all)
