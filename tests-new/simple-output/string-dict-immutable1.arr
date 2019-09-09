### pass
import string-dict-immutable as D
import list-immutable as L
import global as G

dict = [D.string-dict: {x: 1, y: 2, z: 3}]

keys = D.keys(dict)
key-result = L.contains(keys, "x") and L.contains(keys, "y") and L.contains(keys, "z")
values = D.values(dict)
value-result = L.contains(values, 1) and L.contains(values, 2) and L.contains(values, 3)

if key-result and value-result:
  G.console-log("pass")
else:
  G.console-log( G.js-to-string(dict) )
end
