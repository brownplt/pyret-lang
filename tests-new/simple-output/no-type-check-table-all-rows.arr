### true

# no-type-check-table-add-column.arr
# table-add-column test.

import global as G
import tables as T
import list as L
import js-file("../object-equality-helper") as Eq

my-table = table: a, b, c
  row: 1, 2, 3
  row: 4, 5, 6
  row: 7, 8, 9
end

rows = my-table.all-rows()

expected-rows =
  [L.list:
    [T.raw-row: {"a"; 1}, {"b"; 2}, {"c"; 3}],
    [T.raw-row: {"a"; 4}, {"b"; 5}, {"c"; 6}],
    [T.raw-row: {"a"; 7}, {"b"; 8}, {"c"; 9}]]

passes-when-true = Eq._objectDeepEqual(expected-rows, rows)

G.console-log(passes-when-true)