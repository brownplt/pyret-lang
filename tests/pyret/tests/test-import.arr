import file("tests/exporter.arr") as E
import x, f from file("tests/exporter.arr")
import error as Err

check "Should import only what is exported":
  E.x is 10
  E.f satisfies is-function
  E.not-provided raises-satisfies Err.is-field-not-found
end

check "Should import constants": x is 10 end

check "Should only instantiate module once":
  f() is 1
  E.f() is 2
  f() is 3
end


