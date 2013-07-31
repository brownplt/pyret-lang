#lang pyret

fun build-some-lists(n):
  if (n <= 0): []
  else if n > 0: list.link(list.range(0, 100), build-some-lists(n - 1))
  end
end

build-some-lists(1000)
