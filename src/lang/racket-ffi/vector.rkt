#lang pyret/library

provide {
  vector: vector,
  vector-length: vector-length,
  vector-get: vector-get,
  vector-set: vector-set,
  vector-to-list: vector-to-list
} end

import "../pyret-lib/moorings.rkt" as M

list = M.list
error = M.error
builtins = M.builtins
checkers = M.checkers
option = M.option
List = list.List

fun vector(l :: List) -> Vector:
  n = l.length()
  v = const-vector(nothing, n)
  for list.each2(elt from l, i from list.range(0, n)): v.set(i, elt);
  v
end

fun vector-length(v :: Vector): v.length();

fun vector-get(v :: Vector, n :: Number): v.get(n);

fun <a> vector-set(v :: Vector<a>, n :: Number, val :: a) -> Vector<a>:
  v.set(n, val)
end

fun <a> vector-to-list(v :: Vector<a>) -> List<a>: v.to-list();

