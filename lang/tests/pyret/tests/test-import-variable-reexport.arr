import file("defines-vars.arr") as D
import file("reexports-vars.arr") as R
include from R: x, y2, g end

# The values below are chosen not to collide with test-import-variable.arr,
# which mutates the same module-level vars earlier in the same run.

check "assign to a var included through a provide-from re-export":
  x := 42
  x is 42
  D.x is 42
  R.x is 42
  g(43) # assigns x inside the defining module
  x is 43
  R.x is 43
end

check "assign to a var re-exported under a different name":
  y2 := 44
  y2 is 44
  D.y is 44
  R.y2 is 44
end
