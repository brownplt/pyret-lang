### true

# no-type-check-table-empty.arr
# empty function

import global as G
import tables as T

my-table = table: name, age, favNum
  row: "Bob", 12, 1
  row: "Alice", 17, 2
  row: "Eve", 13, 3
end

my-emptied-table = T.empty(my-table)

my-correct-empty-table = table: name, age, favNum
end

are-equal = T._primitiveEqual(my-correct-empty-table, my-emptied-table)

are-not-equal = T._primitiveEqual(my-correct-empty-table, my-table)

passes-when-true = are-equal and G._not(are-not-equal)

G.console-log(passes-when-true)
