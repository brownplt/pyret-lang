### true

# no-type-check-table-build-column.arr
# table.build-column test.

import global as G
import tables as T
import js-file("../object-equality-helper") as Eq

my-table = table: a, b
  row: 1, 2
  row: 3, 4
  row: 5, 6
end

fun adder(r):
  r.get-value("a") + r.get-value("b")
end

my-new-table = my-table.build-column("c", adder)

expected-table = table: a, b, c
  row: 1, 2, 3
  row: 3, 4, 7
  row: 5, 6, 11
end

passes-when-true = Eq._objectDeepEqual(expected-table, my-new-table)

G.console-log(passes-when-true)