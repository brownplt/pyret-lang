data Foo:
  | bar
  | baz
end

cases(Foo) bar:
  | baz(x) => "there is no x"
end
