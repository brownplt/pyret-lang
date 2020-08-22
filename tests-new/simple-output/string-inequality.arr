### true
import global as G

r1 = "a" < "b"
r2 = "foo" < "bar"
r3 = "foo" > "bar"
r4 = "FoO" < "Foo"

result = r1 and G.not(r2) and r3 and r4
G.console-log(result)
