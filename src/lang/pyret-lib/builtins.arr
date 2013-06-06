#lang pyret/library

import "list.rkt" as list
provide {
  keys: keys,
  has-field: has-field,
  mklist: mklist
} end

fun mklist(obj):
  case:
    | obj.is-empty => list.empty
    | else => list.link(obj.first, mklist(obj.rest))
  end
end

fun keys(obj):
  mklist(prim-keys(obj))
end

fun has-field(obj, name):
  prim-has-field(obj, name)
end

