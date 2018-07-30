provide *
provide-types *

include string-dict

import lists as L
import tensorflow as TF

######################
## Type Definitions ##
######################

type Tensor = TF.Tensor
type Sequential = TF.Sequential

######################
## Data Definitions ##
######################

data File:
  | file(name :: String, content :: String)
end

data Recommender:
  | recommender(model :: Sequential, bag-of-books :: List<String>)
end

data Pair:
  | pair(book-1 :: String, book-2 :: String)
end

NEWLINE-CHAR = "\n"

f1 = file("list_1.txt", "A\nB")
f2 = file("list_2.txt", "A\nB\nC")
f3 = file("list_3.txt", "A\nB\nC\nD")
f4 = file("list_4.txt", "A\nB\nC\nD\nE")
f5 = file("list_5.txt", "B\nC")
f6 = file("list_6.txt", "A\nD\nC\nB") # f3 but in a different order

fun generate-model(book-records :: List<File>, num-epochs :: Number) -> Recommender block:
  book-lists   = extract-book-lists(book-records)
  bag-of-books = L.distinct(list-flatten(book-lists))

  # Create a 2D Tensor where each row is a book and each column is a book;
  # each entry is the number of times the book in the row was bought with
  # the book in the column
  tensor-pairs = generate-tensors(book-lists, bag-of-books)

  model = create-model(128, bag-of-books.length())
  compile-model(model, 0.01)

  for each(epoch from range(0, num-epochs)) block:
    print("Starting epoch #" + num-to-string(epoch) + "...")
    start-time = time-now()

    xs = tensor-pairs.get-value("xs")
    ys = tensor-pairs.get-value("ys")

    model.fit(xs, ys,
      {
        epochs: 1
      }, {(_,_): nothing})

    end-time = time-now()
    total-ms = end-time - start-time
    print(" Completed in " + num-to-string(total-ms) + "ms.")
  end

  recommender(model, bag-of-books)
  # xs = generate-matrix(book-lists, bag-of-books)
  # ...
end

fun create-model(
    layer-size :: Number,
    book-bag-size :: Number) -> Sequential block:
  doc: "Creates an LSTM model"

  model = TF.make-sequential({})


  #| lstm-layer = TF.lstm-layer({
      units: layer-size,
      inputShape: [raw-array: 1, book-bag-size]
    })
  model.add(lstm-layer) |#
  # linear'|'relu'|'relu6'| 'selu'|'sigmoid'|'softmax'|'softplus'|'softsign'|'tanh'|string
  model.add(TF.dense-layer({units: book-bag-size, activation: "softmax", inputShape: [raw-array: book-bag-size]}))

  model
end

fun compile-model(model :: Sequential, learning-rate :: Number):
  optimizer = TF.train-rmsprop(learning-rate, none, none, none, false)
  model.compile({optimizer: optimizer, loss: 'categoricalCrossentropy'})
end

fun generate-pairs(record-lists :: List<List<String>>) -> List<Pair>:
  fun make-pairs(base-book :: String, book-list :: List<String>) -> List<Pair>:
    cases (List) book-list:
      | empty => empty
      | link(f, r) =>
        if base-book == f:
          make-pairs(base-book, r)
        else:
          new-pair = pair(base-book, f)
          link(new-pair, make-pairs(base-book, r))
        end
    end
  end

  cases (List) record-lists:
    | empty => empty
    | link(f, r) =>
      pairs = for fold(base from [list:], book from f):
        base.append(make-pairs(book, f))
      end
      pairs.append(generate-pairs(r))
  end
end

fun generate-tensors(
    record-lists :: List<List<String>>,
    bag-of-books :: List<String>)
  -> StringDict<Tensor> block:

  pairs = generate-pairs(record-lists)

  num-pairs = pairs.length()
  num-books = bag-of-books.length()
  xs-buffer = TF.make-buffer([list: num-pairs, num-books])
  ys-buffer = TF.make-buffer([list: num-pairs, num-books])

  indices = generate-index-dict(bag-of-books)

  for each2(p from pairs, i from range(0, num-pairs)):
    cases (Pair) p block:
      | pair(book-1, book-2) =>
        book-1-index = indices.get-value(book-1)
        book-2-index = indices.get-value(book-2)
        xs-buffer.set(1, [list: i, book-1-index])
        ys-buffer.set(1, [list: i, book-2-index])
    end
  end

  [string-dict:
    "xs", xs-buffer.to-tensor(),
    "ys", ys-buffer.to-tensor()]
end

fun generate-index-dict(lst :: List<String>) -> StringDict<Number>:
  for fold2(base from [string-dict:], str from lst, index from range(0, lst.length())):
    base.set(str, index)
  end
end

fun extract-book-lists(book-records :: List<File>) -> List<List<String>>:
  cases (List) book-records:
    | empty => empty
    | link(f, r) =>
      books-in-file = string-split-all(f.content, NEWLINE-CHAR)
      link(books-in-file, extract-book-lists(r))
  end
end

fun list-flatten<A>(nested-list :: List<List<A>>) -> List<A>:
  cases (List) nested-list:
    | empty => empty
    | link(f, r) => f.append(list-flatten(r))
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

  TF.multinomial(new-preds, 1, none, true).data-sync().first
end

fun recommend(title :: String, book-recommender :: Recommender):
  doc: ```Takes in the title of a book and a trained model and returns
       a recommendation for that book given the model```
  cases (Recommender) book-recommender block:
    | recommender(model, bag-of-books) =>

      temperature-scalar = TF.make-scalar(0.6)

      indices      = generate-index-dict(bag-of-books)
      input-buffer = TF.make-buffer([list: 1, bag-of-books.length()])
      input-buffer.set(1, [list: 1, indices.get-value(title)])

      input = input-buffer.to-tensor()

      output = model.predict(input, {})

      output
      #|
      # Sample randomly based on the probability values:
      winner-index = num-exact(sample-on-probabilities(output.squeeze(none), temperature-scalar))
      winner-book = bag-of-books.get(winner-index)

      winner-book|#
  end
end
