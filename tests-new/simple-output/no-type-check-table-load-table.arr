### true

# no-type-check-table-load-table.arr
# load-table syntax test.

import global as G
import tables as T

my-table = load-table: first, second, third
  source: T._makeTableSkeletonFromCSVString(```a,b,c
1,true,a
4,false,b
7,true,c ```)
end

expected-table = table: first, second, third
  row: 1, true, "a"
  row: 4, false, "b"
  row: 7, true, "c"
end

passes-when-true = T._primitiveEqual(expected-table, my-table)

G.console-log(passes-when-true)
