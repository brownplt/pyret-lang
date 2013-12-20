#lang pyret

provide *

data CList:
  | clink(cyclic first, rest)
  | cempty
end

graph:
x = clink(5, cempty)
y = clink(x, cempty)
end
