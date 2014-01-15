o = {a : method(self): self.value;, value: 42, p : method(self, other): self.a() + p;}

test-print(o.p(100))

y = {value : 64, f : o:p}

test-print(y.p(100))
