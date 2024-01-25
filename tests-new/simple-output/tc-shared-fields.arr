### Looks shipshape
include global

data Side:
  | left(x :: Number)
  | right(x :: Number)
end

type HasX<T> = { x :: T }

fun f(thing :: {x :: Number}) -> Number:
  thing.x
end
fun f2(thing :: {x :: Number}) -> Number:
  thing.x
end

side-thing :: HasX<Number> = left(1)
side-thing2 :: Side = left(1)
# x :: Number = side-thing.x
x :: Number = f(side-thing) # Type checks
y :: Number = f2(side-thing2)
check:
    x is 1
    y is 1
end
