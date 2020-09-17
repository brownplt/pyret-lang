include primitive-types
import raw-array as RA
import lists as L
import number as N

include from RA:
  raw-array
end

include from L:
  type List,
  empty,
  link,
  list,
  filter,
  raw-array-to-list,
  foldl,
  foldr
end

include from N:
  num-to-string,
end

fun is-positive(x :: Number) -> Boolean:
  x > 0
end

fun sub(acc :: Number, e :: Number) -> Number:
  acc - e
end

fun concat(acc :: String, x :: Number) -> String:
   acc + num-to-string(x)
end

check "filter":
  list1 = [list: -5, 0, 21, 100, -3, 5]
  filter(is-positive, list1) is [list: 21, 100, 5]
  list1 is [list: -5, 0, 21, 100, -3, 5]

  list2 = [list: 100, 51, -90, 0, 3, -100]
  filter(is-positive, list2) is [list: 100, 51, 3]
  list2 is [list: 100, 51, -90, 0, 3, -100]
end

check "raw-array":
  raw-array-to-list([raw-array: 1, 2, 3]) is [list: 1, 2, 3]
  raw-array-to-list([raw-array: -1, -2, -3, 1, 2, 3]) is [list: -1, -2, -3, 1, 2, 3]
end

check "foldl":
  list1 = [list: 1, 2, 3, 4, 5]
  foldl(sub, 0, list1) is -15
  foldl(concat, "", list1) is "12345"
end

check "foldr":
  list1 = [list: 1, 2, 3, 4, -5]
  foldr(sub, 0, list1) is -5
  foldr(concat, "", list1) is "-54321"
end
