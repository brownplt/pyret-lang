### [ 1, 2, 3 ]

import list as L
import global as G

include from L:
  type List
end

my-list :: List<Number> = [L.list: 1, 2, 3]

fun printList(list :: List<Number>):
  G.console-log( list )
end

printList( my-list )
