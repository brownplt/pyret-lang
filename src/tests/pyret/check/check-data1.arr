#lang pyret

data Foo:
  | foo with
      equals(self,other): is-foo(other) end
check
  checkers.check-equals(foo,foo)
end