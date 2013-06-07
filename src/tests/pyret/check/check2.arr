#lang pyret

eq = checkers.check-equals

fun odd(n):
  case:
    | n == 0 => false
    | n == 1 => true
    | else => even(n - 1)
  end
check:
  eq(odd(9), true)
  eq(odd(8), false)
end


fun even(n):
  case:
    | n == 0 => true
    | n == 1 => false
    | else => odd(n - 1)
  end
check:
  eq(even(9), false)
  eq(even(8), true)
end

