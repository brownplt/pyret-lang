#lang pyret

data Foo:
  | foo with:
      _equals(self,other): is-foo(other) end
check:
  checkers.check-equals("foo=foo",foo,foo)
end
