### true

# no-type-check-table-from-columns.arr
# table-from-columns test.

import global as G
import equality as E
import tables as T
import lists as L

my-table =
  [T.table-from-columns:
    {"a"; [L.list: 1, 4, 7]},
    {"b"; [L.list: 2, 5, 8]},
    {"c"; [L.list: 3, 6, 9]}]

expected-table = table: a, b, c
  row: 1, 2, 3
  row: 4, 5, 6
  row: 7, 8, 9
end

passes-when-true = E.equal-always(expected-table, my-table)

G.console-log(passes-when-true)
