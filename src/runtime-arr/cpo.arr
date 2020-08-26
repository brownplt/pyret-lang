# TODO(alex): make-array vs array?

# NOTE(alex): Translated from standard-imports in arr/compiler/compile-structs.arr
provide from A:
  raw-array as array,
  raw-array-length as array-length,
  raw-array-of as array-of,
  raw-array-set as array-set-now,
  raw-array-get as array-get-now,
  is-raw-array as is-array,
end

provide from L:
  to-raw-array as array-from-list,
  raw-array-to-list as array-to-list-now,
end

provide from G:
  type RawArray as Array
end

import global as G
import raw-array as A
import lists as L
