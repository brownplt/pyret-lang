### true

# no-type-check-table-column-names.arr
# table.column-names tests

import global as G
import equality as E
import tables as T
import lists as L

my-table = table: a, b, c
  row: 1, 2, 3
  row: 4, 5, 6
  row: 7, 8, 9
end

names = my-table.column-names()

expected-names = [L.list: "a", "b", "c"]

passes-when-true = E.equal-always(expected-names, names)

G.console-log(passes-when-true)
