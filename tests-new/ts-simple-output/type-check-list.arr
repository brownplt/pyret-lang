### 123

import lists as L
import global as G

include from L:
  type List
end

my-list :: List<Number> = [L.list: 1, 2, 3]

fun printList(list :: List<Number>):
  msg = for L.fold( s from "", e from list):
   s + G.to-string(e)
  end

  G.console-log( msg )
end

printList( my-list )
