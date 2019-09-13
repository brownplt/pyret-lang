### pass

import list as L
import string-dict as D
import global as G

include from D:
  type StringDict
end

dict :: StringDict<Number> = [D.string-dict: {x: 1, y: 2, z: 3}]

fun type-check(str-dict :: StringDict<Number>):
  G.console-log("pass")
end

type-check( dict )
