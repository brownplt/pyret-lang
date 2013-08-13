#lang pyret

check:
  1 is 1
  1 is 2
end

fun foo():
  1
  check:
    2 is 2
  end
where:
  foo() is 2
end