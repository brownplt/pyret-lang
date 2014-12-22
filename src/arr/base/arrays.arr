#lang pyret/library

provide {
  array: {
    make: make
  },
  build-array: build-array,
  array-from-list: array-from-list,
  is-array: is-array,
  array-of: array-of,
  array-set-now: array-set-now,
  array-get-now: array-get-now,
  array-length: array-length,
  array-to-list-now: array-to-list-now
} end
provide-types *

import lists as lists
type List = lists.List

newtype Array as ArrayT

get-arr-key = {}

fun make(arr :: RawArray) -> Array:
  ArrayT.brand({
    get-arr(_, key):
      if key == get-arr-key: arr else: raise("Cannot get arr externally") end
    end,
    get-now(_, ix :: Number): raw-array-get(arr, ix) end,
    set-now(self, ix :: Number, val) -> Nothing:
      raw-array-set(arr, ix, val)
      nothing
    end,
    length(_): raw-array-length(arr) end,
    to-list-now(_): raw-array-to-list(arr) end,
    _equals(self, other, eq):
      eq(self.get-arr(get-arr-key), other.get-arr(get-arr-key))
    end,
    _torepr(self, shadow torepr):
      "[array: " + self.to-list-now().map(torepr).join-str(", ") + "]"
    end,
    _tostring(self, shadow tostring):
      "[array: " + self.to-list-now().map(tostring).join-str(", ") + "]"
    end
  })
end

is-array = ArrayT.test

fun build-array<a>(f :: (Number -> a), len :: Number):
  arr = raw-array-of(nothing, len)
  fun loop(i):
    when i < len:
      raw-array-set(arr, i, f(i))
      loop(i + 1)
    end
  end
  loop(0)
  make(arr)
end

fun array-from-list(l):
  arr = raw-array-of(0, l.length())
  for lists.each_n(n from 0, elt from l):
    raw-array-set(arr, n, elt)
  end
  make(arr)
end

fun array-of<a>(elt :: a, count :: Number) -> Array<a>:
  arr = raw-array-of(elt, count)
  make(arr)
end

fun array-set-now<a>(arr :: Array<a>, index :: Number, val :: a) -> Nothing:
  arr.set-now(index, val)
end

fun array-get-now<a>(arr :: Array<a>, index :: Number) -> a:
  arr.get-now(index)
end

fun array-length<a>(arr :: Array<a>) -> Number:
  arr.length()
end

fun array-to-list-now<a>(arr :: Array<a>) -> List<a>:
  arr.to-list-now()
end
