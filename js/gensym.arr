#lang pyret

provide *

make-name = block:
  var count = 0
  fun(base):
    count := 1 + count
    base + count.tostring()
  end
end

