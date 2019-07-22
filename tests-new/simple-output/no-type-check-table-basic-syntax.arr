### No error reported

# no-type-check-table-basic-syntax.arr
# Basic `table:` and `row:` syntax

import global as g

my-table = table: name :: String, age :: Number, favorite-color :: String
  row: "Bob", 12, "Blue"
  row: "Alice", 17, "Green"
end

g.console-log("No error reported")