#lang pyret

import "list.arr" as list
provide {
  keys: keys
} end

fun mklist(obj):
  cond:
    | obj.is-empty => list.empty()
    | else => list.link(obj.first, mklist(obj.rest))
  end
end

fun keys(obj):
  mklist(prim-keys(obj))
end


