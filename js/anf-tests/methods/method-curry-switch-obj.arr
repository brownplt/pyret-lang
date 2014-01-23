meth = method(self): self.x end
o = {x:10, m: meth}
f = o.m

o2 = {m2: f}
o2.m2()

