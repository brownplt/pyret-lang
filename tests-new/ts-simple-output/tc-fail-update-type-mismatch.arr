##! type-mismatch
include global

data D:
  | x(ref n :: Number, ref y :: String, ref z :: Boolean)
end

x1 = x(10, "a", true)
x1!{n : "a"}