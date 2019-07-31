### true

# no-type-check-order-by-columns.arr

import global as g
import tables as t
import list as l
import js-file("../object-equality-helper") as helper

my-table = table: name, age, favorite-color
  row: "Bob", 12, "blue"
  row: "Alice", 17, "green"
  row: "Bob", 13, "yellow"
  row: "Bob", 13, "orange"
  row: "Eve", 13, "red"
end

cols = [l.list: {"age"; true}, {"name"; false}]

my-ordered-table = t.order-by-columns(my-table, cols)

my-correct-ordered-table = table: name, age, favorite-color
  row: "Bob", 12, "blue"
  row: "Eve", 13, "red"
  row: "Bob", 13, "yellow"
  row: "Bob", 13, "orange"
  row: "Alice", 17, "green"
end

are-equal = helper._objectDeepEqual(my-correct-ordered-table, my-ordered-table)
are-not-equal = helper._objectDeepEqual(my-correct-ordered-table, my-table)

passes-when-true = are-equal and g._not(are-not-equal)

g.console-log(passes-when-true)
