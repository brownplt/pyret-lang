provide {
  foo: foo,
  f: f,
  foobar-fun: foobar-fun,
  is-internal-error: is-internal-error
} end
provide:
  module E
end
provide-types {
  Foo:: Foo,
  FooBar:: FooBar
}

import error as E

data Foo:
  | foo(a :: Number)
end

data Bar:
  | bar
end

type FooBar = Foo

fun f(x :: Number) -> Number:
  x
end

fun foobar-fun(thing :: FooBar) -> Foo:
  thing
end

fun g() -> Number:
  1
end

is-internal-error = E.is-internal-error
