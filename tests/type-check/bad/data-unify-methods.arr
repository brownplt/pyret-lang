data MOption<T>:
  | noneM with:
    method bind(self, transformer :: Number): self end,
    method foo(self): self end
  | someM(v :: T) with:
    method bind(self, transformer): transformer(self.v) end
end

fun addMO(a :: MOption<Number>, b :: MOption<Number>) -> MOption<Number>:
  a.bind({(av :: Number):
        b.bind({(bv :: Number):
          some(av + bv)})})
end

check:
  addMO(someM(3), someM(5)) is some(8)
  addMO(someM(3), noneM) is none
  addMO(noneM, someM(5)) is none
end
