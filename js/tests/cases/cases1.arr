data Foo:
  | bar
  | baz
end

cases(Foo) bar:
  | bar => "is a bar"
  | baz => "is a baz"
end