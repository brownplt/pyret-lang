#lang pyret

provide *

make-name = block:
  fun(base):
    gensym(base)
  end
end

