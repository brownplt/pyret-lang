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
 "2
  1 1"
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

(check-pyret-exn/indent
  "fun f(a):
      2 * a
  end

  fun g(a):
      f (22)
      a + 2
  end"
  "Expected to find one statement per line")

(check-pyret-exn/indent
  "fun f(a):
      2 * a
  end

  fun g(a):
      a + 2
      f (22)
      b + 3
  end"
  "Expected to find one statement per line")


(check-pyret-exn/indent
  "fun g(a):
      a + 2
      (b + 3) 4
  end"
  "Expected to find one statement per line")

(check-pyret-exn/indent
  "fun g(a):
      a + 2
      b + 3 4
  end"
  "Expected to find one statement per line")

(check-pyret-exn/indent
  "fun g(a):
      g
      fun h(): end 5
  end"
  "Expected to find one statement per line")

(check-pyret-exn/indent
  "fun g(a):
      g
      print()
      do-other-thing()
      fun h(): end 5
  end"
  "Expected to find one statement per line")

(check-pyret-exn/indent
  "fun g(a):
      g (4)
      print()
      5 + 6
      77
  end"
  "Expected to find one statement per line")
))

(run-tests all)
