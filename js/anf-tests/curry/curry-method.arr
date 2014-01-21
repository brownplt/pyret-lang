o = {
  m(self, x): self.x + x end,
  m2(self, x, y, z, w): self.x + x + y + z + w end,
  x: 10
}

test-print((_.m(3))(o))
test-print((_.m(_))(o, 2))
test-print((o.m(_))(4))

test-print((o.m(1, _, 3, _))(2, 4))
test-print((o.m(_, _, _, 4))(1, 2, 3))
test-print((o.m(1, _, _, _))(2, 3, 4))
