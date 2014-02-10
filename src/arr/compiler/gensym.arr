#lang pyret

provide *

var gensym-counter = 0

make-name = block:
  fun(base):
    gensym-counter := 1 + gensym-counter
    base + (gensym-counter.tostring())
  end
end

