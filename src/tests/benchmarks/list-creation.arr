#lang pyret

import profile as P

fun build-some-lists(n):
  case:
    | (n <= 0) => []
    | n > 0 => list.link(list.range(0, 100), build-some-lists(n - 1))
  end
end

P.profile(fun: build-some-lists(1000) end)
