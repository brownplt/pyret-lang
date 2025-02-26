#lang pyret/library

provide:
  array,
  build-array,
  array-from-list,
  is-array,
  array-of,
  array-set-now,
  array-get-now,
  array-length,
  array-to-list-now,
  array-filter,
  array-map,
  array-fold,
  array-concat,
  array-duplicate,
  array-sort-nums,
  array-sort-by
end
provide-types *

import global as _
import lists as L
import global as G
import valueskeleton as VS

raw-array-sort-by = G.raw-array-sort-by
raw-array-sort-nums = G.raw-array-sort-nums
raw-array-duplicate = G.raw-array-duplicate
raw-array-concat = G.raw-array-concat

newtype Array as ArrayT

get-arr-key = {}

fun make<A>(arr :: RawArray<A>) -> Array<A>:
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

    method filter(self, f :: (A -> Boolean)): make(raw-array-filter(f, arr)) end,
    method map<B>(self, f :: (A -> B)) -> Array<B>: make(raw-array-map(f, arr)) end,
    method fold<B>(self, f :: (B, A, Number -> B), init :: B, start-index :: Number) -> B: raw-array-fold(f, init, arr, start-index) end,
    method concat(self, other :: Array<A>) -> Array<A>: make(raw-array-concat(arr, other.get-arr(get-arr-key))) end,
    method duplicate(self) -> Array<A>: make(raw-array-duplicate(arr)) end,

    method sort-nums(self, asc :: Boolean) block:
      raw-array-sort-nums(arr, asc)
      self
    end,
    method sort-by(self, key :: (A -> Number), asc :: Boolean): make(raw-array-sort-by(arr, key, asc)) end,

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
  for L.each_n(n from 0, elt from l):
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

fun array-to-list-now<a>(arr :: Array<a>) -> L.List<a>:
  arr.to-list-now()
end


array-filter :: <A> (A -> Boolean), Array<A> -> Array<A>
fun array-filter(f, self): self.filter(f) end

array-map :: <A, B> (A -> B), Array<A> -> Array<B>
fun array-map(f, self): self.map(f) end

array-fold :: <A, B> (B, A, Number -> B), B, Array<A>, Number -> B
fun array-fold(f, init, self, start-index): self.fold(f, init, start-index) end

array-concat :: <A> Array<A>, Array<A> -> Array<A>
fun array-concat(self, other): self.concat(other) end

array-duplicate :: <A> Array<A> -> Array<A>
fun array-duplicate(self): self.duplicate() end

array-sort-nums :: <A> Array<A>, Boolean -> Array<A>
fun array-sort-nums(self, asc): self.sort-nums(asc) end

array-sort-by :: <A> Array<A>, (A -> Number), Boolean -> Array<A>
fun array-sort-by(self, key, asc): self.sort-by(key, asc) end

array = {
  make: make,
  make0: lam(): make([raw-array: ]) end,
  make1: lam(a): make([raw-array: a]) end,
  make2: lam(a, b): make([raw-array: a, b]) end,
  make3: lam(a, b, c): make([raw-array: a, b, c]) end,
  make4: lam(a, b, c, d): make([raw-array: a, b, c, d]) end,
  make5: lam(a, b, c, d, e): make([raw-array: a, b, c, d, e]) end
}
