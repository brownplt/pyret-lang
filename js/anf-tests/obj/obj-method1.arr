o = {x : method(self): self.y;, y : 42}

meth = o:x

y = {a : meth, y : "A"}

test-print(o.x())
test-print(y.a())
test-print(y:y)
