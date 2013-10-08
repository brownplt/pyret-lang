data Foo:
  | bar
  | baz
end

cases(Foo) 5:
  | else => "Bad cases"
end
