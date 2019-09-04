### true

# no-type-check-table-load-table.arr
# load-table syntax test.

import global as G
import tables as T

my-table = load-table: first, second, third
  source: T._makeTableSkeletonFromCSVString(```a,b,c
1,2,3
4,5,6
7,8,9 ```)
end

expected-table = table: first, second, third
  row: "1", "2", "3"
  row: "4", "5", "6"
  row: "7", "8", "9"
end

passes-when-true = T._primitiveEqual(expected-table, my-table)

G.console-log(passes-when-true)