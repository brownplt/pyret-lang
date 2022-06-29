### 123
import string-dict as D
import global as G

dict = [D.alternating-string-dict: "x", 1, "y", 2, "z", 3]

xValue = dict.get-value("x")
yValue = dict.get-value("y")
zValue = dict.get-value("z")

output = G.to-string(xValue) + G.to-string(yValue) + G.to-string(zValue)

G.console-log(output)
