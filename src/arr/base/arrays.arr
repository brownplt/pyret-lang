#lang pyret/library

provide {
  array: {
    make: make
  },
  build-array: build-array,
  array-from-list: array-from-list,
  is-array: is-array
} end
provide-types *

import lists as lists

newtype Array as ArrayT

get-arr-key = {}

fun make(arr :: RawArray) -> Array:
  ArrayT.brand({
    get-arr(_, key):
      if key == get-arr-key: arr else: raise("Cannot get arr externally") end
    end,
    get(_, ix): raw-array-get(arr, ix) end,
    set(self, ix, val):
      raw-array-set(arr, ix, val)
      self
    end,
    length(_): raw-array-length(arr) end,
    to-list(_): raw-array-to-list(arr) end,
    _equals(self, other, eq):
      eq(self.get-arr(get-arr-key), other.get-arr(get-arr-key))
    end,
    _torepr(self, shadow torepr):
      "[array: " + self.to-list().map(torepr).join-str(", ") + "]"
    end,
    tostring(self, shadow tostring):
      "[array: " + self.to-list().map(tostring).join-str(", ") + "]"
    end
  })
end

is-array = ArrayT.test

fun <a> build-array(f :: (Number -> a), len :: Number):
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

