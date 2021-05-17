### true
include global

fun even(x :: Number) -> Boolean:
  if x == 0: true
  else: odd(x - 1)
  end
end

fun odd(x :: Number) -> Boolean:
  if x == 0: false
  else: even(x - 1)
  end
end

console-log(even(4) and even(100) and not(even(77)) and odd(3))