### Looks shipshape

import lists as L
import tables as T 
import global as G
import file as F

check:
    G.console-log(F.real-path("."))
    t = T.table-from-csv-file('tests-new/ts-simple-output/data/data.csv')
    t.get-column("a") is [L.list: 1, 4]
end