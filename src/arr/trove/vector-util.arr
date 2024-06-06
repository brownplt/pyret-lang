provide:
  *,
  type *
end

import lists as L
import global as G
include from G:
  raw-array-from-list,
  raw-array-duplicate,
  raw-array-and-mapi,
  raw-each-loop
end

#|
   This file defines basic vector operations purely in terms of RawArrays.
   The ergonomics of this file are less convenient than the Vector data type
   defined in the matrices library, but may be used more efficiently
   by the matrix-utils or stats libraries.
|#


type RawVector = RawArray<Number>

close-enough = within-abs(1e-6)


fun NonZeroNat(n :: Number): num-is-integer(n) and (n > 0) end
fun Nat(n :: Number): num-is-integer(n) and (n >= 0) end

type NonZeroNat = Number%(NonZeroNat)
type Nat = Number%(Nat)

## RawArray utility functions
fun raw-array-fold2<a, b, c>(f :: (a, b, c, Number -> a), base :: a, a1 :: RawArray<b>, a2 :: RawArray<c>, start-idx :: Number) -> a:
  doc: "Adaptation of lists.fold2 to work with RawArrays"
  len1 = raw-array-length(a1)
  len2 = raw-array-length(a2)
  fun fold2help(acc, idx):
    if (idx >= len1) or (idx >= len2):
      acc
    else:
      fold2help(f(acc, raw-array-get(a1, idx), raw-array-get(a2, idx), idx), idx + 1)
    end
  end
  fold2help(base, start-idx)
where:
  raw-array-fold2(lam(l, a, b, n): link(a, link(b, l)) end, [list:], [raw-array: 1, 2], [raw-array: 3, 4], 0)
    is [list: 2, 4, 1, 3]
end

fun raw-array-map2<a, b, c>(f :: (a, b -> c), arr1 :: RawArray<a>, arr2 :: RawArray<b>) -> RawArray<c>:
  doc: "Like lists.map, but for RawArrays"
  arrlen = num-min(raw-array-length(arr1), raw-array-length(arr2))
  for raw-array-build(idx from arrlen):
    f(raw-array-get(arr1, idx), raw-array-get(arr2, idx))
  end
end

fun dot(contents1 :: RawVector, contents2 :: RawVector) -> Number:
  doc: "Computes the dot product of the two vectors, up to the smaller of the two lengths"
  for raw-array-fold2(acc from 0, elt1 from contents1, elt2 from contents2, _ from 0):
    (elt1 * elt2) + acc
  end
end

fun magnitude-squared(contents :: RawVector) -> Number:
  for raw-array-fold(acc from 0, elt from contents, _ from 0):
    acc + num-sqr(elt)
  end
end

fun magnitude(contents :: RawVector) -> Number:
  num-sqrt(magnitude-squared(contents))
end

fun is-zero(contents :: RawVector) -> Boolean:
  raw-array-and-mapi({(v, _): close-enough(v, 0)}, contents, 0)
end

fun cross(contents1 :: RawVector, contents2 :: RawVector) -> RawArray<Number>:
  if (raw-array-length(contents1) <> 3) or (raw-array-length(contents2) <> 3):
    raise("Cross products are only defined between two 3-d vectors.")
  else:
    s0 = raw-array-get(contents1, 0)
    s1 = raw-array-get(contents1, 1)
    s2 = raw-array-get(contents1, 2)
    o0 = raw-array-get(contents2, 0)
    o1 = raw-array-get(contents2, 1)
    o2 = raw-array-get(contents2, 2)
    [raw-array:
      (s1 * o2) - (s2 * o1),
      (s2 * o0) - (s0 * o2),
      (s0 * o1) - (s1 * o0)]
  end
end

fun normalize(contents :: RawVector) -> RawVector block:
  copy = raw-array-duplicate(contents)
  normalize-now(copy)
  copy
end

fun normalize-now(contents :: RawVector) -> Nothing:
  n = magnitude(contents)
  raw-each-loop(lam(i):
      raw-array-set(contents, i, raw-array-get(contents, i) / n)
    end, 0, raw-array-length(contents))
end

fun scale(contents :: RawVector, scalar :: Number) -> RawVector block:
  copy = raw-array-duplicate(contents)
  scale-now(copy, scalar)
  copy
end

fun scale-now(contents :: RawVector, scalar :: Number) -> Nothing:
  raw-each-loop(lam(i):
      raw-array-set(contents, i, raw-array-get(contents, i) * scalar)
    end, 0, raw-array-length(contents))
end

fun plus(contents1 :: RawVector, contents2 :: RawVector) -> RawVector:
  if raw-array-length(contents1) <> raw-array-length(contents2):
    raise("Cannot add vectors of different lengths")
  else:
    for raw-array-map2(elt1 from contents1, elt2 from contents2):
      elt1 + elt2
    end
  end
end

fun minus(contents1 :: RawVector, contents2 :: RawVector) -> RawVector:
  if raw-array-length(contents1) <> raw-array-length(contents2):
    raise("Cannot subtract vectors of different lengths")
  else:
    for raw-array-map2(elt1 from contents1, elt2 from contents2):
      elt1 - elt2
    end
  end
end

fun add-scaled-now(dest :: RawVector, src :: RawVector, scalar :: Number, startIdx :: Nat) -> Nothing:
  raw-each-loop(lam(i):
      raw-array-set(dest, i,
        raw-array-get(dest, i) + (scalar * raw-array-get(src, i)))
    end, startIdx, num-min(raw-array-length(dest), raw-array-length(src)))
end
