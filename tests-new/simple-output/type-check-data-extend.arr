### false
import global as G

data Foo:
  | foo(x :: Boolean)
end

f1 :: Foo = foo(true)
f2 :: Foo = f1.{x: false}

G.console-log(f2.x)
