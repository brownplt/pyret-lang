data Foo:
  | bar(a,b)
  | baz
end

cases(Foo) bar(1,2):
  | baz => 0
  | bar(_,b) => b * 2
end