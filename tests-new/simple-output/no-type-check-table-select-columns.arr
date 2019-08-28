### true

# no-type-check-table-select-columns.arr
# `select` syntax.

import global as G
import tables as T

my-table = table: a, b, c
  row: 1, 2, 3
  row: 4, 5, 6
  row: 7, 8, 9
end

my-select-table = select a, c from my-table end

my-correct-select-table = table: a, c
  row: 1, 3
  row: 4, 6
  row: 7, 9
end

are-equal = T._primitiveEqual(my-correct-select-table, my-select-table)

are-not-equal = T._primitiveEqual(my-correct-select-table, my-table)

passes-when-true = are-equal and G.not(are-not-equal)

G.console-log(passes-when-true)
