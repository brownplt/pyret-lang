#lang racket

(require
  rackunit
  "test-utils.rkt"
  "../lang/runtime.rkt"
  "match-set.rkt")

(define six (mk-num 6))
(define eight (mk-num 8))
(define forty8 (mk-num 48))

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