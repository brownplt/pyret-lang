#lang pyret

eq = checkers.check-equals

data Foo:
  | foo with:
      _equals(self,other): is-foo(other) end
  | bar with:
      _equals(self,other): false end
where:
  eq("foo=foo",foo,foo)
  eq("bar=bar",bar,bar)
end
