### true

# no-type-check-table-rename-column.arr
# renameColumn function

import global as g
import tables as t
import js-file("../object-equality-helper") as helper

my-table = table: name, age, favNum
  row: "Bob", 12, 1
  row: "Alice", 17, 2
  row: "Eve", 13, 3
end

my-renamed-table = t.rename-column(my-table, "age", "theirAge")

my-correct-renamed-table = table: name, theirAge, favNum
  row: "Bob", 12, 1
  row: "Alice", 17, 2
  row: "Eve", 13, 3
end

are-equal = helper._objectDeepEqual(my-correct-renamed-table, my-renamed-table)

are-not-equal = helper._objectDeepEqual(my-correct-renamed-table, my-table)

passes-when-true = are-equal and g._not(are-not-equal)

g.console-log(passes-when-true)
