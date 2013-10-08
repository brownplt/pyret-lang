data Foo:
  | bar(x)
end

cases(Foo) bar(1):
  | bar() => "not enough args"
end