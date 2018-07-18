import tensorflow as TF

# Train a simple model
model = TF.make-sequential({})

first-layer = TF.dense-layer({
    units: 100,
    activation: 'relu',
    inputShape: [raw-array: 10]})
second-layer = TF.dense-layer({
      units: 1,
      activation: 'linear'})

model.add(first-layer)
model.add(second-layer)

model.compile({loss: 'meanSquaredError', optimizer: 'sgd'})

xs = TF.random-normal([list: 100, 10], none, none)
ys = TF.random-normal([list: 100, 1], none, none)

model.fit(xs, ys, {epochs: 100}, lam(epoch :: Number, log :: Object):
    print("Epoch "
        + num-to-string(epoch)
        + ": loss = "
        + num-to-string(log.loss))
  end)
