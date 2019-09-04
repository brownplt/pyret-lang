### z3y2x1

import list as L
import string-dict as D
import global as G

include from D:
  type StringDict
end

dict :: StringDict<Number> = [D.string-dict: {x: 1, y: 2, z: 3}]

fun printList(str-dict :: StringDict<Number>):
  msg = for L.reduce(string from "", key from D.keys(str-dict)):
    string-v :: String = G.js-to-string(D.get(str-dict, key))
    result :: String = string + key 
    result + string-v
    # TODO(alex): Unable to infer: string + key + string-v
  end
  G.console-log( msg )
end

printList( dict )
