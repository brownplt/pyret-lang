data Foo:
  | bar
  | baz
end

cases(Foo) bar:
  | baz => "match me"
  | baz => "do not match me"
end