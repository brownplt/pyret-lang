

data Foo:
  | bar()
end

a :: Number = cases(Foo) bar():
  | bar => 5
end
