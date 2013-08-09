#lang racket

(require
  rackunit
  rackunit/text-ui
  "test-utils.rkt"
  "../lang/runtime.rkt")

(define six (p:mk-num 6))
(define ten (p:mk-num 10))
(define eight (p:mk-num 8))
(define forty8 (p:mk-num 48))

(define all (test-suite "all"

;; tests for type annotation runtime checks
(check-pyret-exn
 "var x :: Number = 'hello'"
 "runtime:")

(check-pyret
 "var x :: String = 'hello' x"
 (p:mk-str "hello"))

(check-pyret
 "var x :: list.List = [1]
  x.first"
 (p:mk-num 1))

(check-pyret
 "var x :: list.List<Foo> = [1]
  x.first"
 (p:mk-num 1))

(check-pyret-exn "var x :: Number = true" "runtime:")
(check-pyret "var x :: Number = 48 x" forty8)
(check-pyret-exn "var x :: Number = 37 x := 'not-a-num'" "runtime:")

(check-pyret "var x :: Number = 23 x := 6" six)
(check-pyret "fun f(x :: Number) -> String: var y :: String = 'daniel' y end f(42)" (p:mk-str "daniel"))
(check-pyret-exn "fun f(x :: Number) -> String: var y :: String = 'daniel' y := 6 end f(42)"
                 "runtime:")
(check-pyret "var x :: Number = 6 fun f(): var x :: String = 'daniel' x := 'bobby' end f() x"
             six)
(check-pyret-exn "var x :: Number = 6 fun f(): x := 'bobby' end f() x"
                 "runtime:")
(check-pyret-exn "fun g(h): h() end var x :: Number = 6 fun f(): x := 'bobby' end g(f) x"
                 "runtime:")

(check-pyret-exn
 "fun f(x :: Number) -> String: x.tostring() end
  var g :: (Number -> String) = f
  g := fun(x :: String) -> String: x end
  g('foo')"
 "runtime:")

(check-pyret-exn
 "fun f(x :: Number) -> String: x.tostring() end
  var g :: (String -> String) = f
  g := fun(x :: String) -> String: x end
  g(6)"
 "runtime:")

(check-pyret
 "fun f(g :: (Number -> String)): g(10) end
  f(fun(m): m.tostring() end)"
 (p:mk-str "10"))

(check-pyret-exn
 "fun f(g :: (Number -> String)): g('hello') end
  f(fun(m): m.tostring() end)"
 "expected Number")

(check-pyret-exn
 "fun f(g :: (Number -> String)): g(10) end f(fun(m): m end)"
 "expected String")

(check-pyret-exn
 "data HasAType:
    | variant(x :: Number)
  end
  variant(true)"
 "expected Number")

#;(check-pyret
 "data Maybe(a) | some: value :: a end
  fun f(x :: Maybe<Number>)) -> Number: x.value end
  f(some(10))"
 ten)

#;(check-pyret
 "data Maybe(a) | some: value :: a end
  fun f(x :: (Maybe<Number> -> Number)) -> Number: x(some(10)) end
  f(\\m: (m.value))"
 ten)

#;(check-pyret-exn
  "data Maybe(a) | some: value :: a end
  fun f(x :: Maybe<Number>) -> Number: x.value end
  f(some('hello'))"
  "runtime:")

#;(check-pyret
 "data Maybe(a) | some: value :: a end
  fun f(x :: (Maybe<Number> -> Number)) -> Number: x(some('string')) end
  f(\\m: (m.value))"
 "runtime:")

(check-pyret-exn
 "fun even(x):
    if x._equals(0): true
    else if x._equals(1) or (x._equals(-1)): false
    else: even(x._minus(2))
    end
  end
  x :: Number(even) = 5"
 "contract failure")

(check-pyret
 "fun even(x):
    if x._equals(0): true
    else if x._equals(1) or (x._equals(-1)): false
    else: even(x._minus(2))
    end
  end
  x :: Number(even) = 10 x"
 ten)

(check-pyret-exn
  "data D:
    | variant() with: meth(self, other :: Number): other end
   end
   variant().meth('not-a-num')"
   "expected Number")

(check-pyret
  "data D:
    | variant(x :: Number) with: meth(self :: D, other :: Number): self end
   end
   variant(10).meth(0).meth(1).meth(2).meth(3).x"
   ten)

(check-pyret
  "data D:
    | variant(x :: Number) with: meth(self :: D, other :: Number): self end
   end
   v = variant(5)
   v2 = variant(10)
   v:meth._fun()(v2,3).meth(1).meth(2).meth(3).x"
   ten)
 
))

(check-pyret-exn
  "x"
  "Unbound identifier: x")

(run-tests all)
