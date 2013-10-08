data Foo:
  | bar
  | baz
end

cases(Foo) bar:
  | baz => "match me!"
  | baz => "don't match me!"
end