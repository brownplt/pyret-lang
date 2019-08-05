### true

# no-type-check-table-dot-row.arr
# table.row tests

import global as G
import tables as T
import list as L

my-table = table: a, b, c
  row: 1, 2, 3
  row: 4, 5, 6
  row: 7, 8, 9
end

my-row = my-table.row(10, 11, 12)

expected-row = [T.raw-row: {"a"; 10}, {"b"; 11}, {"c"; 12}]

passes-when-true = T._primitiveEqual(my-row, expected-row)

G.console-log(passes-when-true)