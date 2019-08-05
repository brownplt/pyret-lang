### true

# no-type-check-table-rename-column.arr
# renameColumn function

import global as G
import tables as T

my-table = table: name, age, favNum
  row: "Bob", 12, 1
  row: "Alice", 17, 2
  row: "Eve", 13, 3
end

my-renamed-table = T.rename-column(my-table, "age", "theirAge")

my-correct-renamed-table = table: name, theirAge, favNum
  row: "Bob", 12, 1
  row: "Alice", 17, 2
  row: "Eve", 13, 3
end

are-equal = T._primitiveEqual(my-correct-renamed-table, my-renamed-table)

are-not-equal = T._primitiveEqual(my-correct-renamed-table, my-table)

passes-when-true = are-equal and G._not(are-not-equal)

G.console-log(passes-when-true)
