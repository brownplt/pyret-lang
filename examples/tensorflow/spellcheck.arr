import tensorflow as TF
import lists as L

include string-dict

type Tensor = TF.Tensor
type Sequential = TF.Sequential

data Spellchecker:
  | spell-checker(model :: Sequential, char-dict :: StringDict<Number>)
end

# README:
# Create a spellchecker with s = create-spellchecker(TEXT, 5, 10)
# Run spellchecks with the model with res = spellcheck("someword", s, 10)

RANDOM-PRECISION = 1000000
PADDING-TOKEN = "<PAD>"

TEXT = "For the placement process, you will be using the Racket programming language. Follow the link to download Racket. Installing it will also install the DrRacket programming environment, in which you can write and run your programs. The first part of the reading (see below) explains how to use DrRacket. For the following assignment, you must use Racket's Beginning Student language. Go to the Language menu, select Choose Language, and pick the language from the Teaching Languages section. If you have questions, you can use our Piazza board. Please do not post answers or ask questions in a way that reveals your answers (for this and all future assignments). Read the policy on Honesty and Sharing: you're responsible for adhering to it. If you have prior programming experience, you may find this assignment quite easy. That's because the assignments are designed so even students who haven't had access to a computing course before can still have a fair shot at trying out. The assignments will grow in complexity, so you may well find yourself wanting to use Piazza later. Brown University has an Academic Code that governs all our transactions. This Code includes a section on Respect for the Integrity of the Academic Process, which establishes its policy on cheating. We expect that you, as students and scholars, will abide by this faithfully and fully. You should be extremely careful when using Internet resources for assistance other than those specifically linked from the course website or specified in the assignment. You are welcome to use reference material, e.g., programming language documentation or an encyclopaedia. Be aware that performing a generic Web search may get you to much more, such as solutions. If you accidentally find a solution and choose to use it, document that you are doing so. You will lose some credit for the assignment, but at least you won't be in violation of the Code. You shouldn't post looking for solutions on third-party mailing lists, either. Because Brown does not operate on an honor code, we have no choice but to police you. Therefore, we will check for violations of the Code. If we believe you are in violation, we will prosecute as per the department's and Brown's regulations; and because we do not like our trust being betrayed, we will not be inclined to mercy. We have successfully prosecuted students in the past, and will not hesitate to do so in the future. We really do have the patience to go through all stages of the review. Don't test us. Regret Clause: Exceptions are possible only if you admit your violation to the professor (not a TA) within 24 hours of the assignment due time. This gives you an option if you cheated in desperation the night an assignment was due, or allowed someone to cheat from you, or something else—and then felt guilty about it soon after. Violations may still be sent through the normal University process even if you admit to them under this clause, under the professor's discretion, though perhaps with mitigating recommendations so the penalty may be less harsh than if you were caught by us."

# d = spellcheck("prior", create-spellchecker(TEXT, 5, 10), 10)
fun create-spellchecker(corpus :: String, min-length :: Number, max-length :: Number):
  bag-of-chars = get-char-list(corpus)
  char-dict    = make-char-dict(bag-of-chars)

  initial-words  = get-words(corpus)
  selected-words = select-words-of-length(initial-words, min-length, max-length)

  correct-words = transform-words-to-ints(selected-words, char-dict)
  noisy-words   = correct-words.map(add-noise(_, 0.8, char-dict))

  correct-sized = cut-words-to-length(correct-words, max-length, char-dict)
  noisy-sized   = cut-words-to-length(noisy-words, max-length, char-dict)

  model    = create-model(128, max-length, char-dict.count())
  compiled = compile-model(model, 0.01)

  num-epochs = 20
  examples-per-epoch = 20
  batch-size = 20
  validation-split = 0.0625
  _ = fit-model(
    compiled,
    correct-sized,
    noisy-sized,
    max-length,
    char-dict.count(),
    num-epochs,
    examples-per-epoch,
    batch-size,
    validation-split)

  spell-checker(compiled, char-dict)
end

fun sample-on-probabilities(
    preds :: Tensor,
    temperature :: Tensor)
  -> Number:
  log-preds = TF.tensor-log(preds).divide(temperature)
  exp-preds = TF.tensor-exp(log-preds)
  sum-preds = TF.reduce-sum(exp-preds, some(2))
  new-preds = exp-preds.divide(sum-preds)

  TF.multinomial(new-preds, 1, none, true).data-now()
end

fun spellcheck(word :: String, spellchecker :: Spellchecker, max-length :: Number) -> String:
  cases (Spellchecker) spellchecker block:
    | spell-checker(model, char-dict) =>
      word-ints   = transform-words-to-ints([list: word], char-dict)
      word-padded = cut-words-to-length(word-ints, max-length, char-dict).first

      print(word-padded)

      xs-buffer = TF.make-buffer([list: 1, max-length, char-dict.count()])
      for each2(x-char from word-padded, i from range(0, max-length)) block:
        print(x-char)
        print(i)
        xs-buffer.set(1, [list: 0, i, x-char])
      end

      input  = xs-buffer.to-tensor()
      output = model.predict(input, {})

      parse-output(output, max-length, spellchecker.char-dict)
      #|
      temperature-scalar = TF.make-scalar(0.6)
      winner-index = sample-on-probabilities(output.squeeze(none), temperature-scalar)
      winner-index|#
  end
end

fun select-data(correct-words :: List<List<Number>>, noisy-words :: List<List<Number>>, data-size :: Number) -> Object:
  start-index = num-random(correct-words.length())

  correct-start = correct-words.drop(start-index)
  noisy-start   = noisy-words.drop(start-index)

  current-count = correct-start.length()
  if current-count < data-size:
    remaining = data-size - current-count
    {
      correct: correct-start.append(correct-words.take(remaining)),
      noisy: noisy-start.append(noisy-words.take(remaining))
    }
  else:
    {
      correct: correct-start,
      noisy: noisy-start
    }
  end
end

fun get-example-tensors(
    correct-words :: List<List<Number>>,
    noisy-words :: List<List<Number>>,
    max-length :: Number,
    char-count :: Number,
    data-size :: Number)
  -> Object:

  sample-data = select-data(correct-words, noisy-words, data-size)
  sample-xs   = sample-data.correct
  sample-ys   = sample-data.noisy

  xs-buffer = TF.make-buffer([list: data-size, max-length, char-count])
  ys-buffer = TF.make-buffer([list: data-size, max-length, char-count])

  _ = for each3(x from sample-xs, y from sample-ys, i from range(0, sample-xs.length())):
    for each3(x-char from x, y-char from y, j from range(0, max-length)) block:
      xs-buffer.set(1, [list: i, j, x-char])
      ys-buffer.set(1, [list: i, j, y-char])
    end
  end

  {
    xs: xs-buffer.to-tensor(),
    ys: ys-buffer.to-tensor()
  }
end

fun fit-model(
    model :: Sequential,
    correct-words :: List<List<Number>>,
    noisy-words :: List<List<Number>>,
    max-length :: Number,
    char-count :: Number,
    num-epochs :: Number,
    examples-per-epoch :: Number,
    batch-size :: Number,
    validation-split :: Number)
  -> Nothing:

  for each(epoch from range(0, num-epochs)) block:
    print("Starting epoch #" + num-to-string(epoch) + "...")
    start-time = time-now()

    next-data =
      get-example-tensors(
        correct-words,
        noisy-words,
        max-length,
        char-count,
        examples-per-epoch)
    xs = next-data.xs
    ys = next-data.ys

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

fun create-model(layer-size :: Number, max-length :: Number, char-count :: Number) -> Sequential block:

  model = TF.make-sequential({})

  INPUT-LAYERS = 2
  HIDDEN-SIZE = 700
  for each(layer-number from range(0, INPUT-LAYERS)) block:
    model.add(TF.lstm-layer({units: HIDDEN-SIZE, batchInputShape: [list: none, some(max-length), some(char-count)], kernelInitializer: "heNormal", biasInitializer: "heNormal", returnSequences: not(layer-number == (INPUT-LAYERS - 1))}))
    model.add(TF.dropout-layer({rate: 0.3}))
  end

  model.add(TF.repeat-vector-layer({n: max-length}))

  OUTPUT-LAYERS = 2
  for each(layer-number from range(0, OUTPUT-LAYERS)) block:
    model.add(TF.lstm-layer({units: HIDDEN-SIZE, kernelInitializer: "heNormal", biasInitializer: "heNormal", returnSequences: true}))
    model.add(TF.dropout-layer({rate: 0.3}))
  end

  model.add(TF.time-distributed-layer({layer: TF.dense-layer({units: char-count, kernelInitializer: "heNormal", biasInitializer: "heNormal"})}))
  model.add(TF.activation-layer({activation: "softmax"}))

  model
end

fun compile-model(model :: Sequential, learning-rate :: Number) -> Sequential block:
  model.compile({optimizer: "adam", loss: 'categoricalCrossentropy', metrics: [raw-array: "accuracy"]})

  model
end

fun cut-words-to-length(words :: List<List<Number>>, max-length :: Number, char-dict :: StringDict<Number>) -> List<List<Number>>:
  cases (List) words:
    | empty => empty
    | link(f, r) =>
      rest-words  = cut-words-to-length(r, max-length, char-dict)
      word-length = f.length()
      if word-length > max-length:
        cut-word = f.take(max-length)
        link(cut-word, rest-words)
      else:
        padded-word = pad-word(f, max-length, char-dict)
        link(padded-word, rest-words)
      end
  end
end

fun pad-word(
    word :: List<Number>,
    max-length :: Number,
    char-dict :: StringDict<Number>)
  -> List<Number>:
  word-length = word.length()
  length-diff = max-length - word-length
  padding     = repeat(length-diff, char-dict.get-value(PADDING-TOKEN))
  word.append(padding)
end

fun add-noise(
    sentence :: List<NumInteger>,
    threshold :: Number,
    char-dict :: StringDict<NumInteger>)
  -> List<NumInteger>:
  doc: ```Relocates, removes, or adds characters to the List<String> sentence
       to synthesize spelling mistakes; `threshold` is the probability that
       an edit to any given character will not be made```

  # Always preserve the first character of the sentence:
  cases (List) sentence:
    | empty => empty
    | link(first-char, rest-of-sentence) =>

      # Iterate over the rest of the characters:
      for fold(base from [list: first-char], char from rest-of-sentence):
        rand = num-random(RANDOM-PRECISION) / RANDOM-PRECISION

        # If rand is less than the threshold, then no edit to the sentence will be made:
        if rand < threshold:
          base.append([list: char])
        else:
          # Otherwise, let's choose an edit at random:
          new-rand = num-random(RANDOM-PRECISION) / RANDOM-PRECISION
          if new-rand > 0.67:
            # 33% chance character will swap locations:
            cases (List) base:
              | empty => [list: char]
              | link(f, r) =>
                temp  = base.last()
                front = base.take(base.length() - 1)
                front.append([list: char, temp])
            end
          else if new-rand < 0.33:
            # 33% chance an extra character will be added to the sentence (subtract
            # 1 from char-dict.count() so <PAD> isn't added):
            base.append([list: num-random(char-dict.count() - 1), char])
          else:
            # 33% chance the character will be forgotten:
            base
          end
        end
      end
  end
end

fun transform-words-to-ints(words :: List<String>, char-dict :: StringDict<Number>) -> List<List<Number>>:
  cases (List) words:
    | empty => empty
    | link(f, r) =>
      word-chars = string-explode(f)
      word-ints  = for map(char from word-chars):
        char-dict.get-value(char)
      end
      link(word-ints, transform-words-to-ints(r, char-dict))
  end
end

fun get-char-list(str :: String) -> List<String>:
  list-to-set(string-explode(clean-punctuation(str))).to-list()
end

fun make-char-dict(bag-of-chars :: List<String>) -> StringDict<Number>:
  dict = for fold2(
      base from [string-dict:],
      char from bag-of-chars,
      index from range(0, bag-of-chars.length())):
    base.set(char, index)
  end
  dict.set(PADDING-TOKEN, dict.count())
end

fun clean-punctuation(str :: String) -> String:
  string-replace(
    string-replace(
      string-replace(
        string-replace(
          string-replace(
            string-replace(
              string-replace(
                string-replace(
                  string-replace(str, ";", ""),
                  ":", ""),
                "-", ""),
              "(", ""),
            ")", ""),
          "!", ""),
        "-", ""),
      ".", ""),
    ",", "")
end

fun get-words(str :: String) -> List<String>:
  string-split-all(string-to-lower(clean-punctuation(str)), " ")
end

fun select-words-of-length(
    words :: List<String>, min :: Number, max :: Number)
  -> List<String>:
  cases (List) words:
    | empty => empty
    | link(f, r) =>
      word-length = string-length(f)
      next-words  = select-words-of-length(r, min, max)
      if (word-length < min) or (word-length > max):
        next-words
      else:
        link(f, next-words)
      end
  end
end

fun parse-output(result-tensor :: Tensor, max-length :: Number, char-dict :: StringDict<Number>) -> String:
  fun get-letter-from-int(num :: Number, remaining-keys :: List<String>) -> String:
    cases (List) remaining-keys:
      | empty => ""
      | link(f, r) =>
        if char-dict.get-value(f) == num:
          f
        else:
          get-letter-from-int(num, r)
        end
    end
  end

  result = result-tensor.data-now()
  word-ints = result.map(num-exact)

  for fold(word from "", range-index from range(0, max-length)):
    letter-range = word-ints.drop(range-index * char-dict.count()).take(char-dict.count())

    max-letter = for fold2(
        base from {val: 0, index: 44},
        letter-probability from letter-range,
        letter-index from range(0, char-dict.count())):
      if letter-probability > base.val:
        {val: letter-probability, index: letter-index}
      else:
        base
      end
    end

    if max-letter.index == char-dict.get-value(PADDING-TOKEN):
      word + ""
    else:
      char = get-letter-from-int(max-letter.index, char-dict.keys().to-list())
      word + char
    end
  end
end
