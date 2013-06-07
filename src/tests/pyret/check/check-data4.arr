#lang pyret

eq = checkers.check-equals 

fun foo():
  data Foo:
    | foo with:
        equals(self,other): is-foo(other) end
    | bar with:
        equals(self,other): false end
  check:
        eq("foo=foo",foo,foo)
  end

  1
check:
  eq("foo()=1",foo(),1)
end
