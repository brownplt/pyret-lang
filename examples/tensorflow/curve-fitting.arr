import tensorflow as TF
import chart as C
import image as I

type Tensor = TF.Tensor
type DataSeries = C.DataSeries

fun positive-rand() -> Number:
  num-random(10000000) / 10000000
end

fun generate-data(num-points :: Number, coefficients :: Object, sigma :: Number) -> Object:
  a = TF.make-scalar(coefficients.a)
  b = TF.make-scalar(coefficients.b)
  c = TF.make-scalar(coefficients.c)
  d = TF.make-scalar(coefficients.d)

  xs = TF.random-uniform([list: num-points], some(-1), some(1))

  # The below represents ax^3 + bx^2 + cx + d:
  ys = a.multiply(xs.expt(TF.make-scalar(3)))
    .add(b.multiply(TF.tensor-square(xs)))
    .add(c.multiply(xs))
    .add(d)
    .add(TF.random-normal([list: num-points], some(0), some(sigma)))

  # Normalize the y values to the range 0 to 1:
  y-min = TF.reduce-min(ys, none)
  y-max = TF.reduce-max(ys, none)
  y-range = TF.subtract-tensors(y-max, y-min)
  ys-normalized = TF.divide-tensors(TF.subtract-tensors(ys, y-min), y-range)

  {xs: xs, ys: ys-normalized}
end

fun predict(a :: Tensor, b :: Tensor, c :: Tensor, d :: Tensor, x :: Tensor) -> Tensor:
  # The below represents ax^3 + bx^2 + cx + d:
  a.multiply(x.expt(TF.make-scalar(3)))
    .add(b.multiply(TF.tensor-square(x)))
    .add(c.multiply(x))
    .add(d)
end

fun loss(prediction :: Tensor, actual-values :: Tensor) -> Tensor:
  TF.subtract-tensors(prediction, actual-values)
    ^ TF.tensor-square(_)
    ^ TF.reduce-mean(_, none)
end

fun plot(scatter-plot :: DataSeries, a :: Tensor, b :: Tensor, c :: Tensor, d :: Tensor) block:
  a-val = a.data-now().first
  b-val = b.data-now().first
  c-val = c.data-now().first
  d-val = d.data-now().first

  print("Equation:")
  print("y = "
      + num-to-string(a-val) + "x^3 + "
      + num-to-string(b-val) + "x^2 + "
      + num-to-string(c-val) + "x + "
      + num-to-string(d-val))
  function-plot = C.from-list.function-plot(
    lam(x): (a-val * num-expt(x, 3)) + (b-val * num-sqr(x)) + (c-val * x) + d-val end)
  chart-image = C.render-charts([list: scatter-plot, function-plot]).get-image()
  I.scale(0.6, chart-image)
end

# Generate synthetic data based on a cubic function
test-data = generate-data(100, {a: -0.8, b: -0.2, c: 0.9, d: 0.5}, 0.04)
train-x = test-data.xs.data-now()
train-y = test-data.ys.data-now()

# Plot the random points ahead of time for better perfomance:
scatter-plot = C.from-list.scatter-plot(train-x, train-y)

# Generate a few variables representing coefficients in the equation,
# randomized to some value between 0 and 1
a = TF.make-scalar(positive-rand()).to-variable()
b = TF.make-scalar(positive-rand()).to-variable()
c = TF.make-scalar(positive-rand()).to-variable()
d = TF.make-scalar(positive-rand()).to-variable()

# Plot the random cubic function overlayed on the initial points:
plot(scatter-plot, a, b, c, d)

# Create an optimizer:
LEARNING-RATE = 0.5
TRAINING-CYCLES = 200
optimizer = TF.train-sgd(LEARNING-RATE)

# Train the model
for each(i from range(0, TRAINING-CYCLES)):
  optimizer.minimize(lam() block:
      prediction = predict(a, b, c, d, test-data.xs)
      loss(prediction, test-data.ys)
    end, empty)
end

# Plot the resulting cubic function overlayed on the initial points:
plot(scatter-plot, a, b, c, d)
