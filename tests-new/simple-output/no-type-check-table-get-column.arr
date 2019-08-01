### true

# no-type-check-table-get-column.arr
# table-get-column test.

import global as G
import tables as T
import list as L
import js-file("../object-equality-helper") as Eq

my-table = table: a, b, c
  row: 1, 2, 3
  row: 4, 5, 6
  row: 7, 8, 9
end

column-0 = T.get-column(my-table, "a")
column-1 = T.get-column(my-table, "b")
column-2 = T.get-column(my-table, "c")

expected-column-0 = [L.list: 1, 4, 7]
expected-column-1 = [L.list: 2, 5, 8]
expected-column-2 = [L.list: 3, 6, 9]

passes-when-true =
  Eq._objectDeepEqual(expected-column-0, column-0)
  and
  Eq._objectDeepEqual(expected-column-1, column-1)
  and
  Eq._objectDeepEqual(expected-column-2, column-2)

G.console-log(passes-when-true)
