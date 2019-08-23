### true
import global as G

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

f3 :: Foo = update(bar(true, "bar"))

result = not(f2.x) and not(f3.x)

if result:
  G.console-log(result)
else:
  G.console-log([list: f1, f2, f3 ])
end
