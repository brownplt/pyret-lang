#lang pyret

fun is-even(n):
  n.modulo(2) == 0
end

fun are-equal(n, m):
  n == m
end

check:
  4 satisfies is-even
  3 satisfies are-equal(3, _)
end
