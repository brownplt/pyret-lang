### 123
import string-dict as D
import global as G

dict = [D.alternating-string-dict: "x", 1, "y", 2, "z", 3]

xValue = dict.get-value("x")
yValue = dict.get-value("y")
zValue = dict.get-value("z")

output = G.js-to-string(xValue) + G.js-to-string(yValue) + G.js-to-string(zValue)

G.console-log(output)
