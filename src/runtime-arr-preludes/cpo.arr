#
# NOTES:
#   * Translated from standard-imports in arr/compiler/compile-structs.arr
#   * Needs to be compiled with compile-mode builtin-general to replace "file" imports
#

# TODO(alex): make-array vs array?

provide from A:
  raw-array as array,
  raw-array-length as array-length,
  raw-array-of as array-of,
  raw-array-set as array-set-now,
  raw-array-get as array-get-now,
  is-raw-array as is-array,
end

provide from L:
  type List,
  to-raw-array as array-from-list,
  raw-array-to-list as array-to-list-now,

  list,
  is-List,
  is-empty,
  is-link,
  empty,
  link,
  range,
  range-by,
  repeat,
  filter,
  partition,
  split-at,
  any,
  find,
  map,
  map2,
  map3,
  map4,
  map_n,
  map2_n,
  map3_n,
  map4_n,
  each,
  each2,
  each3,
  each4,
  each_n,
  each2_n,
  each3_n,
  each4_n,
  fold,
  fold2,
  fold3,
  fold4
end

provide from O:
  type Option,
  is-Option,
  is-some,
  is-none,
  some,
  none
end

provide from S:
  type Set,
  tree-set,
  list-set,
  empty-set,
  empty-tree-set,
  empty-list-set,
  list-to-set,
  list-to-list-set,
  list-to-tree-set,
end

provide from RG:
  not,
  raise
  # including 'nothing' will cause a shadowing error
  # nothing
end

import primitive-types as PT
import raw-array as A
import lists as L
import option as O
import sets as S
import runtime-global as RG
