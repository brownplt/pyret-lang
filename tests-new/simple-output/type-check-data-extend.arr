### true
import global as G
import list as L

not = G.not

data Foo:
  | foo(x :: Boolean)
  | bar(x :: Boolean, y :: String)
end

data Nested:
  | nested(n :: Foo)
end

f1 :: Foo = foo(true)
f2 :: Foo = f1.{x: false}

fun update(f :: Foo) -> Foo:
  f.{x: false}
end

b1 = bar(true, "bar")
b2 :: Foo = update(b1)

n1 = nested(foo(true))
nested-foo = n1.n.{x: false}

result = f1.x and not(f2.x) and b1.x and not(b2.x)
  and n1.n.x and not(nested-foo.x)

if result:
  G.console-log(result)
else:
  G.console-log([L.list: f1, f2, b1, b2 ])
end
