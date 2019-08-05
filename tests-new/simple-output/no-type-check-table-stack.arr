### true

# no-type-check-table-stack.arr
# stack function

import global as G
import tables as T

my-table = table: name, age, favNum
  row: "Bob", 12, 1
  row: "Alice", 17, 2
  row: "Eve", 13, 3
end

bot-table = table: favNum, name, age
  row: 4, "Anne", 14
  row: 5, "Frank", 15
end

my-stacked-table = T.stack(my-table, bot-table)

my-correct-table = table: name, age, favNum
  row: "Bob", 12, 1
  row: "Alice", 17, 2
  row: "Eve", 13, 3
  row: "Anne", 14, 4
  row: "Frank", 15, 5
end

are-equal = T._primitiveEqual(my-correct-table, my-stacked-table)

are-not-equal = T._primitiveEqual(my-correct-table, my-table)

passes-when-true = are-equal and G._not(are-not-equal)

G.console-log(passes-when-true)
