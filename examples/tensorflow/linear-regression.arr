import tensorflow as TF
import chart as C

fun predict(m, b, x):
  temp = TF.multiply-tensors(m, x)
  TF.add-tensors(temp, b)
end

fun loss(prediction, actual-values):
  TF.subtract-tensors(prediction, actual-values)
    ^ TF.tensor-square(_)
    ^ TF.mean(_)
end

train-x = [list: 3.3, 4.4, 5.5, 6.71, 6.93, 4.168, 9.779, 6.182, 7.59, 2.167, 7.042, 10.791, 5.313, 7.997, 5.654, 9.27, 3.1]
train-y = [list: 1.7, 2.76, 2.09, 3.19, 1.694, 1.573, 3.366, 2.596, 2.53, 1.221, 2.827, 3.465, 1.65, 2.904, 2.42, 2.94, 1.3]

C.from-list.scatter-plot(train-x, train-y)

m = TF.random-normal([list: 1, 1]).as-scalar().to-variable()
b = TF.random-normal([list: 1, 1]).as-scalar().to-variable()

m.data-now().first
b.data-now().first

learning-rate = 0.01
optimizer = TF.train-sgd(learning-rate)

fun train():
  optimizer.minimize(lam():
      prediction = predict(m, b, TF.make-tensor(train-x).as-1d())
      loss(prediction, TF.make-tensor(train-y).as-1d())
    end, empty)
end

train()
