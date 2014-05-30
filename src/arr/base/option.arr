#lang pyret

provide *

data Option:
  | none with:
      orelse(self, v :: Any):
        doc: "Return the default provided value"
        v
      end,
      andthen(self, f): self end
  | some(value) with:
      orelse(self, v :: Any):
        doc: "Return self.value, rather than the default"
        self.value
      end,
      andthen(self, f): f(self.value) end
end



