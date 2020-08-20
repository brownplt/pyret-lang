### true
import global as G
include lists

r1 = ("foo" + "bar") == "foobar"
r2 = ([list: "qux"] + [list: "qax", "zoo"]) == [list: "qux", "qax", "zoo"]
result = r1 and r2

G.console-log(result)
