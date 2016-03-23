#lang pyret

provide *

var gensym-counter = 0

make-name = block:
  lam(base):
    gensym-counter := 1 + gensym-counter
    base + (tostring(gensym-counter))
  end
end

