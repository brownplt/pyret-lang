#lang pyret

eq = checkers.check-equals 

fun test():
  data Foo:
    | foo with:
        _equals(self,other): is-foo(other) end
    | bar with:
        _equals(self,other): false end
  check:
        eq("foo=foo",foo,foo)
        eq("bar=bar",bar,bar)
  end

  1
check:
  eq("test()=1",test(),1)
end
