#lang pyret

import test as T

fun odd(n):
  case:
    | n == 0 => false
    | n == 1 => true
    | else => even(n - 1)
  end
check
  T.assert-equals(odd(9), true)
  T.assert-equals(odd(8), false)
end


fun even(n):
  case:
    | n == 0 => true
    | n == 1 => false
    | else => odd(n - 1)
  end
check
  T.assert-equals(even(9), false)
  T.assert-equals(even(8), true)
end
