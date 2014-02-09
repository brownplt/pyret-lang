#lang pyret

provide *

data Option:
  | none with:
      orelse(self, v :: Any):
        doc: "Return the default provided value"
        v
      where:
        none.orelse("any value") is "any value"
      end,
      andthen(self, f): self end
  | some(value) with:
      orelse(self, v :: Any):
        doc: "Return self.value, rather than the default"
        self.value
      where:
        some("value").orelse("unused default") is "value"
      end,
      andthen(self, f): f(self.value) end
end



