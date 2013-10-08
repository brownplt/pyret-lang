data Foo:
  | bar
  | baz
end

cases(Foo) bar:
  | baz => "don't match me!"
end