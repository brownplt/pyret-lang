#lang pyret

provide *
provide-types *

data Option<a>:
  | none with:
    or-else(self :: Option<a>, v :: a) -> a:
      doc: "Return the default provided value"
      v
    end,
    and-then<b>(self :: Option<a>, _ :: (a -> b)) -> Option<b>:
      doc: "Return none"
      self
    end
  | some(value) with:
    or-else(self :: Option<a>, v :: a) -> a:
      doc: "Return self.value, rather than the default"
      self.value
    end,
    and-then<b>(self :: Option<a>, f :: (a -> b)) -> Option<b>:
      doc: "Returns the function applied to self.value"
      some(f(self.value))
    end
where:
  none.or-else(1) is 1
  none.and-then(lam(x): some(x + 2) end) is none

  some(5).or-else(0) is 5
  some(5).and-then(lam(x): some(x + 2) end) is some(7)
end
