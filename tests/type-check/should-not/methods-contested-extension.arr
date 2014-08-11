r = {
  a: 1,
  b(self): self.{d: 1}.{e(self2): self2.d + 1 end} end,
  c(self): self.{d: "one"} end,
}

r.b().c().e()
