### true

# no-type-check-filter-by.arr

import global as g
import tables as t
import js-file("../object-equality-helper") as helper

my-table = table: name, age, favorite-color
  row: "Bob", 13, "blue"
  row: "Alice", 12, "green"
  row: "Eve", 10, "red"
  row: "Frank", 13, "yellow"
end

fun predicate(element):
  element == 13
end

my-filtered-table = t.filter-by(my-table, "age", predicate)

my-correct-table = table: name, age, favorite-color
  row: "Bob", 13, "blue"
  row: "Frank", 13, "yellow"
end

are-equal = helper._objectDeepEqual(my-correct-table, my-filtered-table)
are-not-equal = helper._objectDeepEqual(my-correct-table, my-table)

passes-when-true = are-equal and g._not(are-not-equal)

g.console-log(passes-when-true)
