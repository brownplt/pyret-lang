### true

# no-type-check-increasing-by.arr

import global as g
import tables as t
import js-file("../object-equality-helper") as helper

my-table = table: name, age, favorite-color
  row: "Bob", 13, "blue"
  row: "Alice", 12, "green"
  row: "Eve", 10, "red"
end

my-ordered-table = t.increasing-by(my-table, "age")

my-correct-ordered-table = table: name, age, favorite-color
  row: "Eve", 10, "red"
  row: "Alice", 12, "green"
  row: "Bob", 13, "blue"
end

are-equal = helper._objectDeepEqual(my-correct-ordered-table, my-ordered-table)
are-not-equal = helper._objectDeepEqual(my-correct-ordered-table, my-table)

passes-when-true = are-equal and g._not(are-not-equal)

g.console-log(passes-when-true)
