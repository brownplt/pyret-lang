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

foo1 = foo("foo")
x1 = foo1.get-x()

foo2 = foo1.{ x: "bar" }
x2 = foo1.get-x()
x3 = foo2.get-x()

bar1 = bar("X", "Y")
bar2 = bar1.{ y: "MY" }
x4 = bar1.get-x()
x5 = bar2.get-x()

result = 
  (x1 == "foo") and
  (x2 == "foo") and
  (x3 == "bar") and
  (x4 == "Y") and
  (x5 == "MY") 

if result:
  G.console-log("pass")
else:
  G.console-log([L.list: foo1, foo2])
end
