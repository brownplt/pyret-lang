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

data Bar:
  | bar1
  | bar2(x) with:
    one-mutable: mk-simple-mutable(0)
end

check:
  bar2(42)!{one-mutable: 1}
  bar2(42)!one-mutable is 1
end

data Baz:
  | baz1(x)
  | baz2 with:
    one-mutable: mk-simple-mutable(0)
end

check:
  baz2!{one-mutable: 1}
  baz2!one-mutable is 1
end