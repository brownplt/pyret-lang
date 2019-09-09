### pass
import string-dict-immutable as D
import global as G

dict = [D.string-dict: {x: 1, y: 2, z: 3}]

keys = D.keys(dict)
key-result = keys.contains("x") and keys.contains("y") and keys.contains("z")
values = D.values(dict)
value-result = values.contains(1) and values.contains(2) and values.contains(3)

if key-result and value-result:
  G.console-log("pass")
else:
  G.console-log( G.js-to-string(dict) )
end
