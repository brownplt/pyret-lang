#lang racket

(require
  rackunit
  "test-utils.rkt"
  "../lang/runtime.rkt"
  "match-set.rkt")

(define six (p:mk-num 6))
(define ten (p:mk-num 10))
(define eight (p:mk-num 8))
(define forty8 (p:mk-num 48))

;; tests for type annotation runtime checks
(check-pyret-exn
 "var x :: Number: 'hello'"
 "runtime:")

(check-pyret
 "var x :: String: 'hello'"
 (p:mk-str "hello"))


(check-pyret-exn "var x :: Number: true" "runtime:")
(check-pyret "var x :: Number: 48 x" forty8)
(check-pyret-exn "var x :: Number: 37 x := 'not-a-num'" "runtime:")

(check-pyret "var x :: Number: 23 x := 6" six)
(check-pyret "fun f(x :: Number) -> String: (var x :: String: 'daniel' x) f(42)" (p:mk-str "daniel"))
(check-pyret-exn "fun f(x :: Number) -> String: (var x :: String: 'daniel' x := 6) f(42)"
                 "runtime:")
(check-pyret "var x :: Number: 6 fun f(): (var x :: String: 'daniel' x := 'bobby') f() x"
             six)
(check-pyret-exn "var x :: Number: 6 fun f(): (x := 'bobby') f() x"
                 "runtime:")
(check-pyret-exn "fun g(h): h() end var x :: Number: 6 fun f(): (x := 'bobby') g(f) x"
                 "runtime:")

(check-pyret-exn
 "fun f(x :: Number) -> String: x.tostring() end
  f := \\x :: String -> String: (x)
  f('foo')"
 "runtime:")
(check-pyret-exn
 "fun f(x :: Number) -> String: x.tostring() end
  f := \\x :: String -> String: (x)
  f(6)"
 "runtime:")

(check-pyret
 "fun f(g :: (Number -> String)): g(10) end
  f(\\m: (m.tostring()))"
 (p:mk-str "10"))

(check-pyret-exn
 "fun f(g :: (Number -> String)): g('hello') end
  f(\\m: (m.tostring()))"
 "expected Number")

(check-pyret-exn
 "fun f(g :: (Number -> String)): g(10) end f(\\m: (m))"
 "expected String")

(check-pyret
 "data Maybe(a) | some: value :: a end
  fun f(x :: Maybe<Number>)) -> Number: x.value end
  f(some(10))"
 ten)

(check-pyret
 "data Maybe(a) | some: value :: a end
  fun f(x :: (Maybe<Number> -> Number)) -> Number: x(some(10)) end
  f(\\m: (m.value))"
 ten)

(check-pyret-exn
  "data Maybe(a) | some: value :: a end
  fun f(x :: Maybe<Number>) -> Number: x.value end
  f(some('hello'))"
  "runtime:")

(check-pyret
 "data Maybe(a) | some: value :: a end
  fun f(x :: (Maybe<Number> -> Number)) -> Number: x(some('string')) end
  f(\\m: (m.value))"
 "runtime:")

(check-pyret-exn/libs
 "fun even(x):
    cond:
      | x.equals(0) => true
      | x.equals(1).or(x.equals(-1)) => false
      | else => even(x.minus(2))
    end
  end
  var x :: Number(even): 5"
 "contract failure")

(check-pyret/libs
 "fun even(x):
    cond:
      | x.equals(0) => true
      | x.equals(1).or(x.equals(-1)) => false
      | else => even(x.minus(2))
    end
  end
  var x :: Number(even): 10 x"
 ten)

