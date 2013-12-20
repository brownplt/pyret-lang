#lang pyret/library

provide {
  array: array,
  array-length: array-length,
  array-get: array-get,
  array-set: array-set,
  array-to-list: array-to-list
} end

import "../pyret-lib/moorings.rkt" as M

list = M.list
error = M.error
builtins = M.builtins
checkers = M.checkers
option = M.option
List = list.List

fun array(l :: List) -> Array:
  n = l.length()
  v = array-of(nothing, n)
  for list.each2(elt from l, i from list.range(0, n)): v.set(i, elt);
  v
end

fun array-length(v :: Array): v.length();

fun array-get(v :: Array, n :: Number): v.get(n);

fun <a> array-set(v :: Array<a>, n :: Number, val :: a) -> Array<a>:
  v.set(n, val)
end

fun <a> array-to-list(v :: Array<a>) -> List<a>: v.to-list();

