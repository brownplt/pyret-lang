### shipshape

include global
data D:
  | x(ref n :: Number, ref y :: String, ref z :: Boolean)
end

check:
  x1 = x(10, "a", true)
  x1!{n : 100}
  x1!n is 100
  x1!y is "a"
  x1!z is true

  x1!{y: "updated", z : false, n : 99}
  x1 is=~ x(99, "updated", false)
end
