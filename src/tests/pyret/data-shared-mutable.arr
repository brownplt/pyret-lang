#lang pyret

data Foo:
  | foo1
  | foo2(x)
sharing:
  one-mutable: mk-simple-mutable(0)
end

check:
  foo1!{one-mutable: 1}
  foo2(42)!one-mutable is 1
  foo1!one-mutable is 1
end