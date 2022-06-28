### Looks shipshape
include lists
include raw-array

type F<CONTENTS> = (RawArray<CONTENTS> -> List<CONTENTS>)

fun higher-order-thing(f :: F<Number>) -> List<Number>:
  f([raw-array: 1])
end

fun do-hot(s :: F<Number>) -> List<Number>:
  higher-order-thing(s)
end


fun listy(l :: List<Number>) -> Number:
  l.get(3)
end

fun call-listy(l :: List<Number>) -> Number:
  listy(l)
end

check:
  call-listy([list: 9, 8, 7, 6, 5, 4, 3, 2, 1]) is 6
end