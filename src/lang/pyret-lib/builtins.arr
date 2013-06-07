#lang pyret/library

import "list.rkt" as list
import "error.rkt" as error
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

fun equiv(obj1, obj2):
  doc "Check if two objects have all the same keys with equiv fields"
  fun all_same(obj1, obj2):
    left_keys = keys(obj1)
    try:
      case:
        | has-field(obj1, "equals") => obj1 == obj2
        | Method(obj1).or(Function(obj2)) => false
        | else =>
          for list.fold(same from true, key from left_keys):
            case:
              | has-field(obj2, key).not() => false
              | else =>
                left_val = obj1.[key]
                right_val = obj2.[key]
                same.and(equiv(left_val, right_val))
            end
          end
      end
    except(_):
      false
    end
  end
  all_same(obj1, obj2).and(all_same(obj2, obj1))
check:
  eq = checkers.check-equals
  eq(equiv({}, {}), true)
  eq(equiv({x : 5}, {y : 6}), false)
  eq(equiv({x : 5}, {x : 6}), false)
  eq(equiv({x : 5}, {x : 5}), true)
  eq(equiv({x : 5, y : 6}, {y : 6, x : 5}), true)
  eq(equiv({x : {z: "foo"}, y : 6}, {y : 6, x : {z: "foo"}}), true)
  eq(equiv({x : {z: "foo"}, y : [true, 6]}, {y : [true, 6], x : {z: "foo"}}), true)
  eq(equiv(fun: end, fun: end), false)
  # TODO(joe & dbp): this should probably return true some day, with list helping
  # us out a little bit
  eq(equiv([{}], [{}]), false)
end

