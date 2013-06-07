#lang pyret

eq = checkers.check-equals 

data Foo:
  | foo with:
      equals(self,other): is-foo(other) end
  | bar with:
      equals(self,other): false end
check:
  eq(foo,foo)
  eq(bar,bar)
end
