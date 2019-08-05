### true

# no-type-check-table-row-n.arr
# table.row-n test.

import global as G
import tables as T
import list as L

my-table = table: a, b, c
  row: 1, 2, 3
  row: 4, 5, 6
  row: 7, 8, 9
end

row-0 = my-table.row-n(0)
row-1 = my-table.row-n(1)
row-2 = my-table.row-n(2)

expected-row-0 = [T.raw-row: {"a"; 1}, {"b"; 2}, {"c"; 3}]
expected-row-1 = [T.raw-row: {"a"; 4}, {"b"; 5}, {"c"; 6}]
expected-row-2 = [T.raw-row: {"a"; 7}, {"b"; 8}, {"c"; 9}]

passes-when-true =
  T._primitiveEqual(expected-row-0, row-0)
  and
  T._primitiveEqual(expected-row-1, row-1)
  and
  T._primitiveEqual(expected-row-2, row-2)

G.console-log(passes-when-true)