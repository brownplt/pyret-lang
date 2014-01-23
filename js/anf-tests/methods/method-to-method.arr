o = {
  m1(self, x): self.m2(x) + 1 end,
  m2(self, y): 10 - y end
}
test-print(o.m1())
