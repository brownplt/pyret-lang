### true

# no-type-check-order-by-columns.arr

import global as G
import equality as E
import tables as T
import lists as L

my-table = table: name, age, favorite-color
  row: "Bob", 12, "blue"
  row: "Alice", 17, "green"
  row: "Bob", 13, "yellow"
  row: "Bob", 13, "orange"
  row: "Eve", 13, "red"
end

cols = [L.list: {"age"; true}, {"name"; false}]

my-ordered-table = T.order-by-columns(my-table, cols)

my-correct-ordered-table = table: name, age, favorite-color
  row: "Bob", 12, "blue"
  row: "Eve", 13, "red"
  row: "Bob", 13, "yellow"
  row: "Bob", 13, "orange"
  row: "Alice", 17, "green"
end

are-equal = E.equal-always(my-correct-ordered-table, my-ordered-table)
are-not-equal = E.equal-always(my-correct-ordered-table, my-table)

passes-when-true = are-equal and G.not(are-not-equal)

G.console-log(passes-when-true)
