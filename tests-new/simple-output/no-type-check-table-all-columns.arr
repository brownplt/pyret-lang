### true

# no-type-check-table-all-columns.arr
# table.all-columns test.

import global as G
import tables as T
import list as L

my-table = table: a, b, c
  row: 1, 2, 3
  row: 4, 5, 6
  row: 7, 8, 9
end

columns = my-table.all-columns()

expected-columns =
  [L.list:
    [L.list: 1, 4, 7],
    [L.list: 2, 5, 8],
    [L.list: 3, 6, 9]]

passes-when-true = T._primitiveEqual(expected-columns, columns)

G.console-log(passes-when-true)