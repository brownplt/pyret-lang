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

# TODO(alex): bug where module is not being emitted if s-cases is the last expresssion
# Happens only in tester
G.console-log("ZAP")
