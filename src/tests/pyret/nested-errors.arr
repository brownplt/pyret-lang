#lang pyret

fun unary-x(blah): blah end

non-func = 5

data Foo:
  | string-x(x) with: _torepr(self):
      temp :: String = self.x
      "string-x(" + torepr(self.x) + ")"
    end
  | get-y(x) with: _torepr(self): self.y end
  | call-unary with: _torepr(self): unary-x() end
  | bad-prim(x :: Number) with: _torepr(self): "bad-prim(" + self.x + ")" end
  | call-non-func with: _torepr(self): non-func() end
  | mut-x(mutable x) with: _torepr(self): self.x end
end

check:
  string-x(5).z raises "While reporting that z"
  string-x(5).z raises "expected String and got"
  torepr(string-x(5)) raises "expected String and got"
  string-x('blah').z raises "z was not found"
  torepr(string-x('blah')) is 'string-x("blah")'
  get-y(5).z raises "While reporting that z"
  get-y(5).z raises "y was not found"
  torepr(get-y(5)) raises "y was not found" 
  call-unary.z raises "While reporting that z"
  call-unary.z raises "Expected 1 argument, but got none"
  torepr(call-unary) raises "Expected 1 argument, but got none"
  bad-prim(5).z raises "While reporting that z"
  bad-prim(5).z raises "Bad args to prim: plus"
  torepr(bad-prim(5)) raises "Bad args to prim: plus"
  call-non-func.z raises "While reporting that z"
  call-non-func.z raises "expected function, got number"
  torepr(call-non-func) raises "expected function, got number"
  mut-x(5).z raises "While reporting that z"
  mut-x(5).z raises "Cannot look up mutable field \"x\""
  torepr(mut-x(5)) raises "Cannot look up mutable field \"x\""
end
