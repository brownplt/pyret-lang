### List [ 1, 2, 3 ]

import list-immutable as L
import global as G

include from L:
  type List
end

my-list :: List<Number> = [L.list: 1, 2, 3]

fun printList(list :: List<Number>):
  G.console-log( G.js-to-string(list) )
end

printList( my-list )
