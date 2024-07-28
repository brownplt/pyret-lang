### The program didn't define any tests
include global

fun never(x:: Number) block:
  check: x is 1 end
  x
end

"not calling never(), no tests"