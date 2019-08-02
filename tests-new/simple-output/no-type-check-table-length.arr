### 2

# no-type-check-table-length.arr
# .length function

import global as G
import tables as T

my-table = table: name :: String, age :: Number, favorite-color :: String
  row: "Bob", 12, "Blue"
  row: "Alice", 17, "Green"
end

G.console-log(my-table.length())