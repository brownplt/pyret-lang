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

my-foo1 = foo("foo")
x1 = my-foo1.get-x()

my-foo2 = my-foo1.{ x: "bar" }
x2 = my-foo1.get-x()
x3 = my-foo2.get-x()

my-bar1 = bar("X", "Y")
my-bar2 = my-bar1.{ y: "MY" }
x4 = my-bar1.get-x()
x5 = my-bar2.get-x()

my-nested1 = nested(foo("NESTED"))
my-nested-foo = my-nested1.n.{ x: "UPDATED" }
x6 = my-nested1.n.get-x()
x7 = my-nested-foo.get-x()

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
  G.console-log([L.list: 
    G.js-to-string(my-foo1), 
    G.js-to-string(my-foo2), 
    G.js-to-string(my-nested1), 
    G.js-to-string(my-nested-foo)])
end
