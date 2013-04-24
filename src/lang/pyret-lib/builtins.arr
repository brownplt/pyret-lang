#lang pyret/library

import "list.arr" as list
provide {
  keys: keys,
  has-field: has-field
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

fun has-field(obj, name):
  keys(obj).member(name)
end
