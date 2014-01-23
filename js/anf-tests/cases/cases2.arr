data Foo:
  | bar(a,b)
  | baz(c)
end

cases(Foo) bar(1,2):
  | baz(_) => 0
  | bar(a,b) => a + b
end