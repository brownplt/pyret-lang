o = {x : {x : 1}}
p = {x : {x : {}, y : {a : 1, b : 2}}}

test-print(o.x)
test-print(o.x.x)
test-print(p.x.x)
test-print(p.y.a)
test-print(p.y.b)
