### true
import global as G

data Wrapper:
  | wrapped(x :: Number)
sharing:
  method _plus(self, other :: Wrapper) -> Wrapper:
    wrapped(self.x + other.x)
  end,
  method _minus(self, other :: Wrapper) -> Wrapper:
    wrapped(self.x - other.x)
  end,
  method _times(self, other :: Wrapper) -> Wrapper:
    wrapped(self.x * other.x)
  end,
  method _divide(self, other :: Wrapper) -> Wrapper:
    wrapped(self.x / other.x)
  end,
end

r1 = (wrapped(100) - wrapped(90)) == wrapped(10)
r2 = (wrapped(100) + wrapped(90)) == wrapped(190)
r3 = (wrapped(7) * wrapped(5)) == wrapped(35)
r4 = (wrapped(21) / wrapped(3)) == wrapped(7)

result = r1 and r2 and r3 and r4
G.console-log(result)
