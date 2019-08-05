### true

# no-type-check-decreasing-by.arr

import global as G
import tables as T

my-table = table: name, age, favorite-color
  row: "Bob", 10, "blue"
  row: "Alice", 12, "green"
  row: "Eve", 13, "red"
end

my-ordered-table = T.decreasing-by(my-table, "age")

my-correct-ordered-table = table: name, age, favorite-color
  row: "Eve", 13, "red"
  row: "Alice", 12, "green"
  row: "Bob", 10, "blue"
end

are-equal = T._primitiveEqual(my-correct-ordered-table, my-ordered-table)
are-not-equal = T._primitiveEqual(my-correct-ordered-table, my-table)

passes-when-true = are-equal and G._not(are-not-equal)

G.console-log(passes-when-true)
