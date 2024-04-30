#lang pyret/library

provide {
  array: array,
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

import global as _
include lists
import valueskeleton as VS

newtype Array as ArrayT

get-arr-key = {}

fun make(arr :: RawArray) -> Array:
  ArrayT.brand({
    method get-arr(_, key):
      if key == get-arr-key: arr else: raise("Cannot get arr externally") end
    end,
    method get-now(_, ix :: Number): raw-array-get(arr, ix) end,
    method set-now(self, ix :: Number, val) -> Nothing block:
      raw-array-set(arr, ix, val)
      nothing
    end,
    method length(_): raw-array-length(arr) end,
    method to-list-now(_): raw-array-to-list(arr) end,
    method _equals(self, other, eq):
      eq(self.get-arr(get-arr-key), other.get-arr(get-arr-key))
    end,
    method _output(self): VS.vs-collection("array", self.to-list-now().map(VS.vs-value)) end
  })
end

is-array = ArrayT.test

fun build-array<a>(f :: (Number -> a), len :: Number) block:
  arr = raw-array-of(nothing, len)
  fun loop(i):
    when i < len block:
      raw-array-set(arr, i, f(i))
      loop(i + 1)
    end
  end
  loop(0)
  make(arr)
end

fun array-from-list(l) block:
  arr = raw-array-of(0, l.length())
  for each_n(n from 0, elt from l):
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

array = {
  make: make,
  make0: lam(): make([raw-array: ]) end,
  make1: lam(a): make([raw-array: a]) end,
  make2: lam(a, b): make([raw-array: a, b]) end,
  make3: lam(a, b, c): make([raw-array: a, b, c]) end,
  make4: lam(a, b, c, d): make([raw-array: a, b, c, d]) end,
  make5: lam(a, b, c, d, e): make([raw-array: a, b, c, d, e]) end
}
