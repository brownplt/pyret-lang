data Foo:
  | foo() with: tostring(self): "huzahh!" end
end

tostring(foo())