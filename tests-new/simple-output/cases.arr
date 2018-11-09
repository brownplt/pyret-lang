### baz
import global as G

data Foo:
  | bar(x :: Number)
  | baz(x :: Number)
end

cases(Foo) baz(5):
  | bar(y) => G.console-log("bar")
  | baz(y) => G.console-log("baz")
end
