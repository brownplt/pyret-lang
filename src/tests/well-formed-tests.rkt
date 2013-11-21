#lang racket

(require
  rackunit
  rackunit/text-ui
  "test-utils.rkt"
  "../lang/runtime.rkt")

(define (wf-check type)
  (format "where: blocks only allowed on named function declarations and data, not on ~a" type))


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

;; returns and object because we're really just checking OK parse/wf,
;; and this is (void) otherwise
(check-pyret-match "fun f(): nothing where: 5 + 2 is 7 end {}" (p:p-object _ _ _ _))

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
 "Cannot use `is` outside of a `check` or `where` block.")

(check-pyret-exn
 "fun: 1 raises 2 end"
 "Cannot use a check-test form outside of a `check` or `where` block.")

(check-pyret
 "fun f(): nothing where: 1 is 2 end 10"
 (p:mk-num 10))

(check-pyret-exn
  "fun: 
    data D:
      | var1()
    end
   end"
  "Cannot end a block with a data definition")

(check-pyret-exn
  "fun: 
    y = 10
    x = 5
    fun f(): end
    data D:
      | var1()
    end
   end"
  "Cannot end a block with a data definition")

(check-pyret-exn
  "fun: 
    y = 10
    x = 5
    fun f(): end
    graph:
    z = 5
    end
   end"
  "Cannot end a block with a graph definition")

(check-pyret-exn
  "block:
    x = 5
    y = 10
   end"
  "Cannot end a block in a let-binding")

(check-pyret-exn
  "block:
    x = 5
    graph: y = 10 end
   end"
  "Cannot end a block with a graph definition")

(check-pyret-exn
  "if x < y:
    print('x less than y')
   end"
  "Cannot have an if with a single branch"
)

(check-pyret-exn
  "fun(): where: 5 end"
  (wf-check "anonymous functions"))
(check-pyret-exn
  "method(self): where: 5 end"
  (wf-check "methods"))
(check-pyret-exn
  "{m(self): where: 5 end}"
  (wf-check "methods"))

(check-pyret-exn
  "datatype Foo:
    | foo() with constructor(self): self end
    | foo with constructor(self): self end
   end"
  "Constructor name foo appeared more than once.")

(check-pyret-exn
  "datatype Foo:
    | foo() with constructor(self): self end
    | bar() with constructor(self): self end
    | baz() with constructor(self): self end
    | foo(a) with constructor(self): self end
   end"
  "Constructor name foo appeared more than once.")

(check-pyret-exn
  "datatype Foo:
    | bang with constructor(self): self end
    | bar() with constructor(self): self end
    | bang() with constructor(self): self end
    | foo() with constructor(self): self end
    | foo(a) with constructor(self): self end
   end"
  "Constructor name bang appeared more than once.")

(check-pyret-exn
  "cases(List) []:
    | empty => 1
    | empty => 2
   end"
  "Duplicate case for empty")

(check-pyret-exn
  "cases(List) []:
    | empty => 1
    | link(f, r) => 2
    | empty => 2
   end"
  "Duplicate case for empty")

(check-pyret-exn
  "cases(List) []:
    | empty => 1
    | empty => 2
    | else => 3
   end"
  "Duplicate case for empty")

(check-pyret-exn
  "cases(List) []:
    | link(f, r) => 2
    | bogus => 'bogus'
    | bogus2 => 'bogus'
    | empty => 1
    | bogus3 => 'bogus'
    | empty => 2
    | else => 3
   end"
  "Duplicate case for empty")

(check-pyret-exn
  "cases(List) []:
    | empty => 2
    | bogus => 'bogus'
    | bogus2 => 'bogus'
    | link(f, r) => 1
    | bogus3 => 'bogus'
    | link(_, _) => 2
   end"
  "Duplicate case for link")

))

(run-tests all)
