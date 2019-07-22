### 2

# no-type-check-table-length.arr
# .length function

import global as g

my-table = table: name :: String, age :: Number, favorite-color :: String
  row: "Bob", 12, "Blue"
  row: "Alice", 17, "Green"
end

g.console-log(my-table.length())