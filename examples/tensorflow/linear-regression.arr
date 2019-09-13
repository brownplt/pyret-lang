# linear-regression:
# This program shows how we can use an `Optimizer` to perform a linear
# regression on a few random data points. It’s a good starting point from
# which to develop a TF program that can do curve fitting on any arbitrary
# equations.

import tensorflow as TF
import chart as C
import image as I
import lists as L

type Tensor = TF.Tensor
type Optimizer = TF.Optimizer
type ChartWindow = C.ChartWindow
type Image = I.Image

# Create a tiny helper function:
fun positive-rand() -> Number:
  doc: "Generates a positive Number between 0 and 1"
  num-random(10000000) / 10000000
end

# `train-x` and `train-y` represent random points in a dataset, plotted
# on `scatter-plot`:
train-x = [list:
  3.3, 4.4, 5.5, 6.71, 6.93, 4.168, 9.779, 6.182, 7.59, 2.167, 7.042,
  10.791, 5.313, 7.997, 5.654, 9.27, 3.1]

train-y = [list:
  1.7, 2.76, 2.09, 3.19, 1.694, 1.573, 3.366, 2.596, 2.53, 1.221,
  2.827, 3.465, 1.65, 2.904, 2.42, 2.94, 1.3]

scatter-plot = C.from-list.scatter-plot(train-x, train-y)

# Create two scalar Tensors `m` and `b` that are variables:
m = TF.make-scalar(positive-rand()).to-variable()
b = TF.make-scalar(positive-rand()).to-variable()

# Setup a few helper functions before training:
fun predict(x :: Tensor) -> Tensor:
  doc: ```Uses the current values of m and b to predict what Y-values will
       be generated given a Tensor `x` representing X-values```

  temp = TF.multiply-tensors(m, x)
  TF.add-tensors(temp, b)
end

fun loss(prediction :: Tensor, actual-values :: Tensor) -> Tensor:
  doc: ```Used to calculate a measure of difference between the predicted
       Y-values and the actual Y-values```

  TF.reduce-mean(TF.tensor-square(TF.subtract-tensors(prediction, actual-values)), none)
end

# Train the model by creating an Optimizer. The optimizer will change any
# variable tensors used in the function passed into it in an attempt to
# minimize the returned loss:
fun train():
  doc: "Trains the model"
  learning-rate = 0.005
  optimizer = TF.train-sgd(learning-rate)

  optimizer.minimize(lam() block:
      prediction = predict(TF.list-to-tensor(train-x).as-1d())
      step-loss = loss(prediction, TF.list-to-tensor(train-y).as-1d())
      step-loss
    end, empty)
end

fun plot() -> ChartWindow:
  doc: "Plots the current mx + b function and overlays it on the scatter plot"
  shadow m = m.data-now().first
  shadow b = b.data-now().first

  function-plot = C.from-list.function-plot(lam(x): (m * x) + b end)
  C.render-charts([list: scatter-plot, function-plot])
end

fun train-steps(steps :: Number) -> Image block:
  doc: "Trains the model `steps` times"
  for L.each(_ from L.range(0, steps)) block:
    train()
    print("y = " + num-to-string(m.data-now().first) + "x + " + num-to-string(b.data-now().first))
  end
  plot().get-image()
end
