### pass

import string-dict-immutable as D
import global as G

include from D:
  type StringDict
end

dict :: StringDict<Number> = [D.string-dict: {x: 1, y: 2, z: 3}]

fun printList(str-dict :: StringDict<Number>):
  G.console-log("pass")
end

printList( dict )
