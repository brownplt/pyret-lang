#lang pyret

eq = checkers.check-equals

fun test():
  data Foo:
    | foo with:
        _equals(self,other): is-foo(other) end
    | bar with:
        _equals(self,other): false end
  where:
        eq("foo=foo",foo,foo)
  end

  1
where:
  eq("foo()=1",test(),1)
end
