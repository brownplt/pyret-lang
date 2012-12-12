#lang racket

(require
  rackunit
  "test-utils.rkt"
  "../lang/runtime.rkt"
  "match-set.rkt")

(define six (p:mk-num 6))
(define eight (p:mk-num 8))
(define forty8 (p:mk-num 48))

;; tests for type annotation runtime checks
(check-pyret-exn
 "def x :: Number: 'hello'"
 "runtime:")

(check-pyret
 "def x :: String: 'hello'"
 (mk-str "hello"))


(check-pyret-exn "def x :: Number: true" "runtime:")
(check-pyret "def x :: Number: 48 x" forty8)
(check-pyret-exn "def x :: Number: 37 x = 'not-a-num'" "runtime:")

(check-pyret "def x :: Number: 23 x = 6" six)
(check-pyret "fun f(x :: Number) -> String: (def x :: String: 'daniel' x) f(42)" (mk-str "daniel"))
(check-pyret-exn "fun f(x :: Number) -> String: (def x :: String: 'daniel' x = 6) f(42)"
                 "runtime:")
(check-pyret "def x :: Number: 6 fun f(): (def x :: String: 'daniel' x = 'bobby') f() x"
             six)
(check-pyret-exn "def x :: Number: 6 fun f(): (x = 'bobby') f() x"
                 "runtime:")
(check-pyret-exn "fun g(h): h() end def x :: Number: 6 fun f(): (x = 'bobby') g(f) x"
                 "runtime:")

(check-pyret-exn
 "fun f(x :: Number) -> String: x.tostring() end
  f = \\x :: String -> String: (x)
  f('foo')"
 "runtime:")
(check-pyret-exn
 "fun f(x :: Number) -> String: x.tostring() end
  f = \\x :: String -> String: (x)
  f(6)"
 "runtime:")

(check-pyret
 "fun f(g :: (Number -> String)): g(10) end
  f(\n: (n.tostring()))"
 ten)

(check-pyret-exn
 "fun f(g :: (Number -> String)): g('hello') end
  f(\n: (n.tostring()))"
 "runtime:")

(check-pyret-exn
 "fun f(g :: (Number -> String)): g(10) end
  f(\n: n)"
 "runtime:")

(check-pyret
 "data Maybe(a) | some: value :: a end
  fun f(x :: Maybe(Number)) -> Number: x.value end
  f(some(10))"
 ten)

(check-pyret
 "data Maybe(a) | some: value :: a end
  fun f(x :: (Maybe(Number) -> Number)) -> Number: x(some(10)) end
  f(\m: (m.value))"
 ten)

(check-pyret-exn
  "data Maybe(a) | some: value :: a end
  fun f(x :: Maybe(Number)) -> Number: x.value end
  f(some('hello'))"
  "runtime:")

(check-pyret
 "data Maybe(a) | some: value :: a end
  fun f(x :: (Maybe(Number) -> Number)) -> Number: x(some('string')) end
  f(\m: (m.value))"
 "runtime:")
