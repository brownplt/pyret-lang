import tensorflow as TF
import lists as L
include string-dict

type Tensor = TF.Tensor
type Model = TF.Model
type Sequential = TF.Sequential

DATASET = "Put your data set here!"

data TextData:
  | text(
      text-string :: String,
      sample-len :: Number,
      sample-step :: Number,
      char-dict :: StringDict<Number>,
      reverse-lookup :: List<Number>)
sharing:
  method get-example-begin-indices(self :: TextData) -> List<Number>:
    fun sampler(current-index :: Number, endpoint :: Number) -> List<Number>:
      if current-index < endpoint:
        link(current-index, sampler(current-index + self.sample-step, endpoint))
      else:
        empty
      end
    end
    sample = sampler(0, (string-length(self.text-string) - self.sample-len) - 1)
    L.shuffle(sample)
  end,

  method next-data-epoch(self :: TextData, num-examples :: Number) -> Object block:
    char-dict-size = self.char-dict.count()
    xs = TF.make-buffer([list: num-examples, self.sample-len, char-dict-size])
    ys = TF.make-buffer([list: num-examples, char-dict-size])

    example-begin-indices = self.get-example-begin-indices()

    self-indices = self.text-to-indices(self.text-string)

    for each(i from range(0, num-examples)) block:
      begin-index = example-begin-indices.get(num-modulo(i, example-begin-indices.length()))
      for each(j from range(0, self.sample-len)) block:
        xs.set(1, [list: i, j, self-indices.get(begin-index + j)])
      end
      ys.set(1, [list: i, self-indices.get(begin-index + self.sample-len)])
    end

    {xs: xs.to-tensor(), ys: ys.to-tensor()}
  end,

  method get-random-slice(self :: TextData) -> Object:
    max-index = (string-length(self.text-string) - self.sample-len) - 1
    start-index = num-random(max-index)
    end-index = start-index + self.sample-len

    string-slice = string-substring(self.text-string, start-index, end-index)
    index-slice = self.text-to-indices(string-slice)

    {
      text: string-slice,
      indices: index-slice
    }
  end,

  method text-to-indices(self :: TextData, string :: String) -> List<Number>:
    fun helper(characters :: List<String>) -> List<Number>:
      cases (List) characters:
        | empty => empty
        | link(f, r) =>
          link(
            self.char-dict.get-value(f),
            helper(r))
      end
    end

    helper(string-explode(string))
  end
end

fun list-of-chars-to-dict(list-of-chars :: List<String>) -> Object:
  for fold(
      base from {dict: [string-dict:], reverse: [list:]},
      char from list-of-chars):

    cases (Option) base.dict.get(char):
      | none =>
        {
          dict: base.dict.set(char, base.reverse.length()),
          reverse: base.reverse.append([list: char])
        }
      | some(_) => base
    end
  end
end

fun make-text-data(corpus :: String, sample-len :: Number, sample-step :: Number) -> TextData:
  result = list-of-chars-to-dict(string-explode(corpus))
  text(corpus, sample-len, sample-step, result.dict, result.reverse)
end

fun create-model(
    lstm-layer-sizes :: List<Number>,
    sample-length :: Number,
    char-set-size :: Number) -> Sequential block:
  doc: "Creates an LSTM model"

  model = TF.make-sequential({})

  size-count = lstm-layer-sizes.length()
  for each2(layer-size from lstm-layer-sizes, count from range(0, size-count)):
    lstm-layer = TF.lstm-layer({
        units: layer-size,
        returnSequences: (count < (size-count - 1)),
        inputShape: [raw-array: sample-length, char-set-size]
      })
    model.add(lstm-layer)
  end

  model.add(TF.dense-layer({units: char-set-size, activation: "softmax"}))

  model
end

fun compile-model(model :: Sequential, learning-rate :: Number):
  optimizer = TF.train-rmsprop(learning-rate, none, none, none, false)
  model.compile({optimizer: optimizer, loss: 'categoricalCrossentropy'})
end

fun fit-model(
    model,
    text-data :: TextData,
    num-epochs :: Number,
    examples-per-epoch :: Number,
    batch-size :: Number,
    validation-split :: Number):

  batches-per-epoch = examples-per-epoch / batch-size
  total-batches = num-epochs * batches-per-epoch

  for each(epoch from range(0, num-epochs)) block:
    print("Starting epoch #" + num-to-string(epoch) + "...")
    start-time = time-now()

    next-data = text-data.next-data-epoch(examples-per-epoch)
    xs        = next-data.xs
    ys        = next-data.ys

    model.fit(xs, ys,
      {
        epochs: 1,
        batchSize: batch-size,
        validationSplit: validation-split
      }, {(_,_): nothing})

    end-time = time-now()
    total-ms = end-time - start-time
    print(" Completed in " + num-to-string(total-ms) + "ms.")
  end
end

fun sample-on-probabilities(
    preds :: Tensor,
    temperature :: Tensor)
  -> Number:
  log-preds = TF.tensor-log(preds).divide(temperature)
  exp-preds = TF.tensor-exp(log-preds)
  sum-preds = TF.reduce-sum(exp-preds)
  new-preds = exp-preds.divide(sum-preds)

  TF.multinomial(new-preds, 1, none, true).data-now().first
end

fun generate-text(
    model :: Sequential,
    text-data :: TextData,
    sentence-indices :: List<Number>,
    length :: Number,
    temperature :: NumPositive)
  -> String:

  temperature-scalar = TF.make-scalar(temperature)

  generation = for fold(
      base from {result: "", indices: sentence-indices},
      _ from range(0, length)) block:
    # Encode the current input sequence as a one-hot Tensor:
    input-buffer = TF.make-buffer([list: 1, text-data.sample-len, text-data.char-dict.count()])
    for each(i from range(0, text-data.sample-len)):
      input-buffer.set(1, [list: 0, i, base.indices.get(i)])
    end
    input = input-buffer.to-tensor()

    # Call model.predict() to get the probability values of the next character:
    output = model.predict(input, {})

    # Sample randomly based on the probability values:
    winner-index = num-exact(sample-on-probabilities(output.squeeze(none), temperature-scalar))
    winner-char = text-data.reverse-lookup.get(winner-index)

    {
      result: base.result + winner-char,
      indices: base.indices.drop(1).append([list: winner-index])
    }
  end

  generation.result
end

SAMPLE-LEN = 40
SAMPLE-STEP = 3

LEARNING-RATE = 0.01

NUM-EPOCHS = 5
EXAMPLES-PER-EPOCH = 512
BATCH-SIZE = 128
VALIDATION-SPLIT = 0.0625

GENERATED-LENGTH = 200
TEMPERATURE = 0.75

LSTM-LAYER-SIZES = [list: 128]

start-model = time-now()

text-data = make-text-data(DATASET, SAMPLE-LEN, SAMPLE-STEP)
model = create-model(LSTM-LAYER-SIZES, SAMPLE-LEN, text-data.char-dict.count())
compile-model(model, LEARNING-RATE)
fit-model(model, text-data, NUM-EPOCHS, EXAMPLES-PER-EPOCH, BATCH-SIZE, VALIDATION-SPLIT)

end-model = time-now()
total-model-time = end-model - start-model

print("Entire model generation took " + num-to-string(total-model-time) + "ms.")

seed-sentence = text-data.get-random-slice()
string-sentence = seed-sentence.text
sentence-indices = seed-sentence.indices

generate-text(model, text-data, sentence-indices, GENERATED-LENGTH, TEMPERATURE)
