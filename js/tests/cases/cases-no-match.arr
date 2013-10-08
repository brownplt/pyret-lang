data Foo:
  | bar
  | baz
end

cases(Foo) bar:
  | baz => "do not match me!"
end