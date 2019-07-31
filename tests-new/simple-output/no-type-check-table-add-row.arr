### true

# no-type-check-table-add-row.arr
# table-add-row test.

import global as G
import tables as T
import list as L
import js-file("../object-equality-helper") as Eq

my-table = table: a, b
  row: 1, 2
  row: 4, 5
  row: 7, 8
end

new-table = my-table.add-row([T.raw-row: {"a"; 10}, {"b"; 11}])

expected-table = table: a, b
  row: 1, 2
  row: 4, 5
  row: 7, 8
  row: 10, 11
end

passes-when-true = Eq._objectDeepEqual(expected-table, new-table)

G.console-log(passes-when-true)