### memberfoomemberbarymemberbazsharedsharedmixwith

import global as G

data Foo:
  | foo(x :: String)
  | bar(x :: String, y :: Number)
  | baz(y :: String) with:
    method get-x(self):
      self.y
    end
  | qax(y :: String)
  | qux
  | mix with:
    x: "mixwith" 
sharing:
  x: "shared",
  method get-x(self) -> String :
    self.x
  end
end

foo1 = foo("memberfoo")
bar1 = bar("memberbar", 21)
baz1 = baz("ymemberbaz")
qax1 = qax("y")
qux1 = qux
mix1 = mix

message = foo1.get-x() + bar1.get-x() + baz1.get-x() + qax1.get-x() + qux1.get-x() + mix1.get-x()

G.console-log(message)
