fun b(a :: Boolean): a end
fun n(a :: Number): a end
fun s(a :: String): a end

n(num-random(5))
num-random-seed(42) # returns nothing
n(num-max(9, 5))
n(num-min(3, 4))
n(num-round(4))
n(num-round-even(~2.3))

n(num-abs(4))
n(num-sin(1))
n(num-cos(4))
n(num-tan(32))
n(num-asin(0))
n(num-acos(1))
n(num-atan(3))
n(num-modulo(1, 2))
n(num-truncate(4))
n(num-sqrt(1))
n(num-sqr(3))
n(num-ceiling(2.2))
n(num-floor(4.5))
n(num-log(1))
n(num-exp(3))
n(num-exact(~3))

n(num-to-rational(4))
n(num-to-roughnum(1))

b(num-is-positive(4))
b(num-is-negative(2))
b(num-is-non-positive(0))
b(num-is-non-negative(2))
b(num-is-integer(1))
b(num-is-fixnum(9))
b(num-is-rational(33))
b(num-is-roughnum(44))

n(num-expt(9, 4))
s(num-tostring(99))
s(num-to-string(33))
s(num-to-string-digits(45, 6))

b(num-equal(9, 9))
b(num-within(0.5)(1, 4))
b(num-within-rel(0.5)(1, 2))
b(num-within-abs(0.4)(5, 6))

b(within(0.5)("a", true))
b(within-rel(0.5)(5, true))
b(within-abs(0.5)("a", {}))
b(within-rel-now(0.5)("a", lam(): 5 end))
b(within-abs-now(0.5)(false, true))

