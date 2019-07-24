### true

# no-type-check-table-select-columns.arr
# `select` syntax.

import global as g
import js-file("../object-equality-helper") as helper

my-table = table: a, b, c
  row: 1, 2, 3
  row: 4, 5, 6
  row: 7, 8, 9
end

my-select-table = select a, c my-table end

my-correct-select-table = table: a, c
  row: 1, 3
  row: 4, 6
  row: 7, 9
end

are-equal = helper._objectDeepEqual(my-correct-select-table, my-select-table)

are-not-equal = helper._objectDeepEqual(my-correct-select-table, my-table)

passes-when-true = are-equal and g._not(are-not-equal)

g.console-log(passes-when-true)
