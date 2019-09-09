### true
import global as G
import list as L

not = G.not

data Foo:
  | foo(x :: Boolean)
  | bar(x :: Boolean, y :: String)
end

f1 :: Foo = foo(true)
f2 :: Foo = f1.{x: false}

fun update(f :: Foo) -> Foo:
  f.{x: false}
end

b1 = bar(true, "bar")
b2 :: Foo = update(b2)

result = f1.x and not(f2.x) and and b1.x and not(b2.x)

if result:
  G.console-log(result)
else:
  G.console-log([L.list: f1, f2, b1, b2 ])
end
