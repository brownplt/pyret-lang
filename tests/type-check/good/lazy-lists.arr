import lists as L

fun d-dx(f :: (Number -> Number)) -> (Number -> (Number -> Number)):
  lam(x :: Number) -> (Number -> Number):
    lam(epsilon :: Number) -> Number:
      (f(x + epsilon) - f(x)) / epsilon
    end
  end
end

fun square(x :: Number) -> Number: x * x end
fun double(x :: Number) -> Number: 2 * x end

d-dx-square = d-dx(square)

data Stream<T>:
  | lz-link(h :: T, t :: ( -> Stream<T>))
end

rec ones = lz-link(1, lam(): ones end)
fun nats-from(n :: Number):
  lz-link(n, lam(): nats-from(n + 1) end)
end
rec nats = nats-from(0)

fun lz-first<T>(s :: Stream<T>) -> T: s.h end
fun lz-rest<T>(s :: Stream<T>) -> Stream<T>: s.t() end

fun take<T>(n :: Number, s :: Stream<T>) -> List<T>:
  if n == 0:
    empty
  else:
    link(lz-first(s), take(n - 1, lz-rest(s)))
  end
end

fun lz-map<T,U>(f :: (T -> U), s :: Stream<T>): 
  lz-link(f(lz-first(s)), lam(): lz-map(f, lz-rest(s)) end)
end

tenths = block:
  fun by-ten(d :: Number):
    new-denom = d / 10
    lz-link(new-denom, lam(): by-ten(new-denom) end)
  end
  by-ten(1)
end

d-dx-square-at-10 = d-dx-square(10)

take(5, lz-map(d-dx-square-at-10, tenths))

approximations-of-d-dx-square = lz-map(d-dx-square(10), tenths)
print(take(5, approximations-of-d-dx-square))
print(map(lam(x :: Number): x * 1.0;, take(5, lz-map(d-dx-square(10), tenths))))
