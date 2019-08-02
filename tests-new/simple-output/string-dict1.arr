### 123
import string-dict as D
import global as G

dict = [D.string-dict: {x: 1, y: 2, z: 3}]

xValue = D.get(dict, "x")
yValue = D.get(dict, "y")
zValue = D.get(dict, "z")

output = G.js-to-string(xValue) + G.js-to-string(yValue) + G.js-to-string(zValue)

G.console-log(output)
