### pass
import global as G
import list as L

data Foo:
  | foo(x :: String)
  | bar(x :: String, y :: String) with:
    method get-x(self):
      self.y
    end
sharing:
  method get-x(self):
    self.x
  end
end

data Nested:
  | nested(n :: Foo)
end

foo1 = foo("foo")
x1 = foo1.get-x()

foo2 = foo1.{ x: "bar" }
x2 = foo1.get-x()
x3 = foo2.get-x()

bar1 = bar("X", "Y")
bar2 = bar1.{ y: "MY" }
x4 = bar1.get-x()
x5 = bar2.get-x()

nested1 = nested(foo("NESTED"))
nested-foo = nested.n.{ x: "UPDATED" }
x6 = nested1.n.get-x()
x7 = nested-foo.get-x()

result = 
  (x1 == "foo") and
  (x2 == "foo") and
  (x3 == "bar") and
  (x4 == "Y") and
  (x5 == "MY") and
  (x6 == "NESTED") and
  (x7 == "UPDATED")

if result:
  G.console-log("pass")
else:
  G.console-log([L.list: foo1, foo2, nested1, nested-foo])
end
