#lang pyret

provide *

var gensym-counter = 0

fun reset():
  gensym-counter := 0
end

fun make-name(base) block:
  gensym-counter := 1 + gensym-counter
  base + (tostring(gensym-counter))
end
