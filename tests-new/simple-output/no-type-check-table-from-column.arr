### true

# no-type-check-table-from-columns.arr
# table-from-columns test.

import global as G
import tables as T
import list as L
import js-file("../object-equality-helper") as Eq

my-table = T.table-from-column("a", [L.list: 1, 2, 3])

expected-table = table: a
  row: 1
  row: 2
  row: 3
end

passes-when-true = Eq._objectDeepEqual(expected-table, my-table)

G.console-log(passes-when-true)