#lang racket

(require
  rackunit
  rackunit/text-ui
  "test-utils.rkt"
  "../lang/runtime.rkt"
  "../lang/string-map.rkt")

(verbose! #f)

(define six (p:mk-num 6))
(define ten (p:mk-num 10))
(define eight (p:mk-num 8))
(define forty8 (p:mk-num 48))

(define all (test-suite "all"

;; tests for type annotation runtime checks
(check-pyret-exn
 "var x :: Number = 'hello'"
 "expected Number")

(check-pyret
 "var x :: String = 'hello' x"
 (p:mk-str "hello"))

(check-pyret
 "var b :: Boolean = true b"
 (p:mk-bool #t))

(check-pyret-exn
 "var b :: Boolean = true b := 'foo'"
 "expected Boolean")

(check-pyret-exn
 "x :: String = {}"
 "expected String")

(check-pyret-exn
 "x :: {foo : String} = {}"
 "missing field foo")

(check-pyret-exn
 "x :: {foo : String} = {foo: 10}"
 "expected String")

(check-pyret-exn
 "x :: {foo : String} = 10"
 "missing field foo")

(check-pyret
 "x :: {foo : String} = {foo: 'some string', bar: 10} x.bar"
 (p:mk-num 10))

(check-pyret
 "x :: {} = {foo: 'some string'} x.foo"
 (p:mk-str "some string"))

(check-pyret
 "var x :: list.List = [1]
  x.first"
 (p:mk-num 1))

(check-pyret
 "var x :: list.List<Foo> = [1]
  x.first"
 (p:mk-num 1))

(check-pyret-exn "var x :: Number = true" "expected Number")
(check-pyret "var x :: Number = 48 x" forty8)
(check-pyret-exn "var x :: Number = 37 x := 'not-a-num'" "expected Number")

(check-pyret "var x :: Number = 23 x := 6" six)
(check-pyret "fun f(x :: Number) -> String: var y :: String = 'daniel' y end f(42)" (p:mk-str "daniel"))
(check-pyret-exn "fun f(x :: Number) -> String: var y :: String = 'daniel' y := 6 end f(42)"
                 "expected String")
(check-pyret "var x :: Number = 6 fun f(): var y :: String = 'daniel' y := 'bobby' end f() x"
             six)
(check-pyret-exn "var x :: Number = 6 fun f(): x := 'bobby' end f() x"
                 "expected Number")
(check-pyret-exn "fun g(h): h() end var x :: Number = 6 fun f(): x := 'bobby' end g(f) x"
                 "expected Number")

(check-pyret-exn
 "fun f(x :: Number) -> String: x.tostring() end
  var g :: (Number -> String) = f
  g := fun(x :: String) -> String: x end
  g('foo')"
 "expected Number")

(check-pyret-exn
 "fun f(x :: Number) -> String: x.tostring() end
  var g :: (String -> String) = f
  g := fun(x :: String) -> String: x end
  g(6)"
 "expected String")

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
  "expected Number")

#;(check-pyret
 "data Maybe(a) | some: value :: a end
  fun f(x :: (Maybe<Number> -> Number)) -> Number: x(some('string')) end
  f(\\m: (m.value))"
 "expected Number")

(check-pyret-exn
 "fun even(x):
    if x._equals(0): true
    else if x._equals(1) or (x._equals(-1)): false
    else: even(x._minus(2))
    end
  end
  y :: Number(even) = 5"
 "Contract failure")

(check-pyret
 "fun even(x):
    if x._equals(0): true
    else if x._equals(1) or (x._equals(-1)): false
    else: even(x._minus(2))
    end
  end
  y :: Number(even) = 10 y"
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

(check-pyret-exn
  "x = 5
   fun f():
     x = 3
     x
   end"
  "The name x cannot be used in two nested scopes.")
(check-pyret-exn
  "x = 5
   fun f(x):
     x
   end"
  "The name x cannot be used in two nested scopes.")
))

(check-pyret-exn
  "x"
  "Unbound identifier: x")

(check-pyret-exn
  "x :: (Number -> String) = 5"
  "expected (Number -> String)")

(check-pyret
  "var x :: (Number -> String) = fun(y): 5 end
   Function(x)"
  (p:mk-bool #t))

(check-pyret-exn
  "var f :: (Number -> String) = method(self): end"
  "expected (Number -> String)")

(check-pyret
  "cases(list.List<Number>) [1,2]:
     | empty => list.empty
     | link(f :: Number, r :: list.List<Number>) => f
   end"
  (p:mk-num 1))

(check-pyret-exn
  "block:
     x = 10
     x
   end
   x"
  "Unbound identifier: x")

(check-pyret-exn
  "block:
     var x = 10
     x
   end
   x"
  "Unbound identifier: x")

(check-pyret
  "data D:
    | v(a :: D(fun(v): not is-v(v) end))
    | v2(b)
   end
   v(v2(4)).a.b
   "
   (p:mk-num 4))

(run-tests all)
