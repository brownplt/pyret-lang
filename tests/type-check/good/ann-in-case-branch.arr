
data Foo:
  | foo(a :: String)
end

cases(Foo) foo("hello"):
  | foo(b :: Any) =>
    print(b)
end
