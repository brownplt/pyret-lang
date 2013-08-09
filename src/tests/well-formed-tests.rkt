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

(check-pyret-exn
 "method(): end"
 "well-formedness:")

(check-pyret-exn
 "{foo(): end}"
 "well-formedness:")

(check-pyret-exn
 "fun foo():
   x = 10
  end
  10"
 "well-formedness:")

(check-pyret-exn
 "fun foo():
   var x = 10
  end
  10"
 "well-formedness:")

(check-pyret
 "fun foo():
   var x = 10
   x
  end
  10"
 (p:mk-num 10))

(check-pyret-exn
 "fun foo():
   fun f(): end
  end
  10"
 "well-formedness:")

(check-pyret-exn
 "fun: x = 5 end"
 "Cannot end a block in a let-binding")

(check-pyret-exn
 "fun: var x = 5 end"
 "Cannot end a block in a var-binding")

(check-pyret-exn
 "fun: fun f(): end end"
 "Cannot end a block in a fun-binding")

(check-pyret-exn
 "fun: x = 5 fun f(): end end"
 "Cannot end a block in a fun-binding")

(check-pyret-exn
 "fun: var x = 5 y = 4 fun f(): end end"
 "Cannot end a block in a fun-binding")

(check-pyret-exn
 "fun: nothing check: 5 + 2 is 7 end"
 "well-formedness:")

;; NOTE(dbp 2013-08-09): The more "obvious" occurence of these two get
;; caught by typechecking / parsing.
(check-pyret-exn
 "check = 1 check"
 "Cannot use `check` as an identifier.")
(check-pyret-exn
 "where = 1 fun(): where end"
 "Cannot use `where` as an identifier.")

(check-pyret-exn
 "fun: 1 is 2 end"
 "Cannot use `is` outside of a check block. Try `==`.")

(check-pyret
 "fun: nothing where: 1 is 2 end 10"
 (p:mk-num 10))

))

(run-tests all)
