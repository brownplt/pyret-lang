import tensorflow as TF

# Define a simple model
model = TF.make-sequential({})

layer = TF.make-dense-layer({units: 1, inputShape: [raw-array: 1]})
model.add(layer)

model.compile({loss: 'meanSquaredError', optimizer: 'sgd'})

xs = [TF.tensor: 1, 2, 3, 4]
ys = [TF.tensor: 1, 3, 5, 7]

model.fit(xs, ys, {epochs: 10}, lam(epoch :: Number, log :: Object):
    print("Epoch "
        + num-to-string(epoch)
        + ": loss = "
        + num-to-string(log.loss))
  end)
