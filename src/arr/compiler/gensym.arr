#lang pyret

provide *

var gensym-counter = 0

fun reset():
  gensym-counter := 0
end

make-name = block:
  lam(base):
    gensym-counter := 1 + gensym-counter
    base + (tostring(gensym-counter))
  end
end

