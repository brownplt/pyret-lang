### 123

import list as L
import global as G

include from L:
  type List
end

my-list :: List<Number> = [L.list: 1, 2, 3]

fun printList(list :: List<Number>):
  msg = for L.reduce( s from "", e from list):
   s + G.js-to-string(e)
  end

  G.console-log( msg )
end

printList( my-list )
