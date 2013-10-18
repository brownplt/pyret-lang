#lang pyret

data Foo<T>:
  | base(a :: T)
  | app(a :: Any<T>)
  | pred(a :: Any(fun(o): true end))
  | rec(a :: {x : T})
  | arr(a :: (T -> T))
end

check:
  Foo(base(10)) is true
  Foo(pred(10)) is true
  Foo(rec({x:10})) is true
  Foo(arr(fun(o): o end)) is true
end