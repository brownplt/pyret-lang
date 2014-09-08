
fun square(x :: Number) -> Number: x * x;
fun double(x :: Number) -> Number: 2 * x;

epsilon = 0.001

fun d-dx(f :: (Number -> Number)) -> (Number -> Number):
  lam(x :: Number) -> Number:
    (f(x + epsilon) - f(x)) / epsilon
  end
end

d-dx-square = d-dx(square)
d-dx-square(0)
d-dx-square(1)
d-dx-square(10)
