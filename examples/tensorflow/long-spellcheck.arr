import tensorflow as TF
import lists as L
import string-dict as SD

type Tensor = TF.Tensor
type StringDict<A> = SD.StringDict<A>

RANDOM-PRECISION = 1000000

TEXT = "For the placement process, you will be using the Racket programming language. Follow the link to download Racket. Installing it will also install the DrRacket programming environment, in which you can write and run your programs. The first part of the reading (see below) explains how to use DrRacket. For the following assignment, you must use Racket's Beginning Student language. Go to the Language menu, select Choose Language, and pick the language from the Teaching Languages section. If you have questions, you can use our Piazza board. Please do not post answers or ask questions in a way that reveals your answers (for this and all future assignments). Read the policy on Honesty and Sharing: you're responsible for adhering to it. If you have prior programming experience, you may find this assignment quite easy. That's because the assignments are designed so even students who haven't had access to a computing course before can still have a fair shot at trying out. The assignments will grow in complexity, so you may well find yourself wanting to use Piazza later. Read the Prologue of HtDP 2/e. You won't need all the content of this part to do the work below, but from the next assignment onward you will be defining functions and using conditionals (covered in the rest of the part), so you may find it useful read ahead. If you want to learn more about Racket's support for images, see the Quick Introduction. If you are new to programming: people learn programming by actually writing programs and running them. As you read, type in the code in the document, see how it works, change it a little and see what happens. Actually trying the code in the assigned reading will be essential for you to get comfortable with the material. Do not passively read the material: you will get far less out of it than you imagine! You should create these flag images using the Racket image primitives (for rectangles, stars, etc.), not by finding the images and pasting them in. You are, however, welcome to use a Web search to figure out what the flags look like. You may pick the dimensions of your flag, but please make it large enough for us to see (with a width of at least 100), but preferably not so large that we need to scroll a lot to see them all. For maybe the only time this summer or during the semester, we will not judge you on absolute correctness (e.g., checking whether you got the precise ratios of the different parts right). We're going to visually eyeball it for general adherence. Of course, feel free to geek out on the vexillological details as much as you wish!"

fun string-join(strings :: List<String>) -> String:
  cases (List) strings:
    | empty => ""
    | link(f, r) => f + string-join(r)
  end
end

fun create-char-to-int-dict(text :: String) -> StringDict<NumInteger>:
  char-list = L.distinct(string-explode(text))
  dict = for fold(base from [SD.string-dict:], char from char-list):
    base.set(char, base.count())
  end
  dict.set("<PAD>", dict.count()).set("<EOS>", dict.count() + 1).set("<GO>", dict.count() + 2)
end

fun lookup-int-to-char(
    char-dict :: StringDict<NumInteger>,
    lookup :: NumInteger)
  -> String:

  fun helper(keys :: List<String>):
    cases (List) keys:
      | empty => ""
      | link(f, r) =>
        if char-dict.get-value(f) == lookup:
          f
        else:
          helper(r)
        end
    end
  end

  helper(char-dict.keys().to-list())
end

fun get-sentences(input-string :: String) -> List<String>:
  removed-newlines     = string-replace(input-string, "\n", "")
  removed-questions    = string-replace(removed-newlines, "?", ".")
  removed-exclamations = string-replace(removed-questions, "!", ".")

  split-on-periods = string-split-all(removed-exclamations, ". ")
  split-on-periods.map(lam(x): x + "." end)
end

fun convert-sentences-to-integers(
    sentences :: List<List<String>>,
    char-dict :: StringDict<NumInteger>)
  -> List<List<NumInteger>>:
  cases (List) sentences:
    | empty => empty
    | link(f, r) =>
      link(
        string-explode(f).map(lam(x): char-dict.get-value(x) end),
        convert-sentences-to-integers(r, char-dict))
  end
end

fun convert-integers-to-sentences(
    integer-lists :: List<List<NumInteger>>,
    char-dict :: StringDict<NumInteger>)
  -> List<List<String>>:
  cases (List) integer-lists:
    | empty => empty
    | link(f, r) =>
      link(
        string-join(f.map(lam(x): lookup-int-to-char(char-dict, x) end)),
        convert-integers-to-sentences(r, char-dict))
  end
end

fun add-noise(
    sentence :: List<NumInteger>,
    threshold :: Number,
    char-dict :: StringDict<NumInteger>)
  -> List<NumInteger>:
  doc: ```Relocates, removes, or adds characters to the List<String> sentence
       to synthesize spelling mistakes; `threshold` is the probability that
       an edit to any given character will not be made```

  LETTERS = [list:
    'a','b','c','d','e','f','g','h','i','j','k','l','m',
    'n','o','p','q','r','s','t','u','v','w','x','y','z']

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
            # 33% chance an extra lower case letter will be added to the sentence:
            random-letter = LETTERS.get(num-random(LETTERS.length()))
            base.append([list: char-dict.get-value(random-letter), char])
          else:
            # 33% chance the character will be forgotten:
            base
          end
        end
      end
  end
end

fun create-model-inputs() -> StringDict<Tensor>:
  [SD.StringDict:
    "inputs", TF.make-input([list: none, none]),
    "targets", TF.make-input([list: none, none]),
    "keep-prob", TF.make-input([list:]),
    "inputs-length", TF.make-input([list: none]),
    "targets-length", TF.make-input([list: none]),
    "max-target-length", TF.make-input([list:])]
end

fun process-encoding-input(targets, char-dict, batch-size):
  doc: ```Remove the last word id from each batch and concat the <GO> to
       the begining of each batch```
  ending = TF.strided-slice(targets, [list: 0, 0], [list: batch-size, -1], [list: 1, 1])
  base-fill = TF.fill([list: batch-size, 1], char-dict.get-value("<GO>"))
  TF.concatenate([list: base-fill, ending], some(1))
end

fun pad-sentence-batch(sentence-batch :: List<List<Number>>, char-dict :: StringDict<NumInteger>) -> List<List<Number>>:
  fun get-longest-sentence(
      current :: List<Number>,
      sentences :: List<List<Number>>)
    -> List<Number>:

    cases (List) sentences:
      | empty => current
      | link(f, r) =>
        if f.length() > current.length():
          get-longest-sentence(f, r)
        else:
          get-longest-sentence(current, r)
        end
    end
  end

  longest-sentence = get-longest-sentence([list:], sentence-batch)

  for map(sentence from sentence-batch):
    length-difference = longest-sentence.length() - sentence.length()
    padding = repeat(char-dict.get-value("<PAD>"), length-difference)
    sentence.append(padding)
  end
end

fun get-batches(
    sentences :: List<List<Number>>,
    batch-size :: Number,
    threshold :: Number,
    char-dict :: StringDict<NumInteger>)
  -> List<Object>:

  for map(batch-index from range(1, num-floor(sentences.length() / batch-size))):
    upper-index = batch-index * batch-size
    lower-index = (batch-index - 1) * batch-size
    batch       = sentences.take(upper-index).drop(lower-index)

    noisy-sentences = for map(sentence from batch):
      add-noise(sentence, threshold, char-dict)
    end

    tokenized-sentences = for map(sentence from batch):
      sentence.append([list: char-dict.get-value("<EOS>")])
    end

    padded-noisy     = pad-sentence-batch(noisy-sentences, char-dict)
    padded-tokenized = pad-sentence-batch(tokenized-sentences, char-dict)

    noisy-lengths = for map(sentence from padded-noisy):
      sentence.length()
    end
    tokenized-lengths = for map(sentence from padded-tokenized):
      sentence.length()
    end

    {
      noisy-batch: padded-noisy,
      batch: padded-tokenized,
      noisy-lengths: noisy-lengths,
      lengths: tokenized-lengths
    }
  end
end

#|
fun create-seq2seq-model(
    inputs,
    targets,
    keep-prob,
    inputs-length,
    targets-length,
    max_target-length,
    vocab-size,
    rnn-size,
    num-layers,
    vocab-to-int,
    batch-size,
    embedding-size,
    direction):

  enc-embeddings = TF.make-variable(TF.random-uniform([list: vocab-size, embedding-size], some(-1), some(1)))
  enc-embed-input =
end

   def seq2seq_model():
    '''Use the previous functions to create the training and inference logits'''

    enc_embeddings = tf.Variable(tf.random_uniform([vocab_size, embedding_size], -1, 1))
    enc_embed_input = tf.nn.embedding_lookup(enc_embeddings, inputs)
    enc_output, enc_state = encoding_layer(rnn_size, inputs_length, num_layers,
                                           enc_embed_input, keep_prob, direction)

    dec_embeddings = tf.Variable(tf.random_uniform([vocab_size, embedding_size], -1, 1))
    dec_input = process_encoding_input(targets, vocab_to_int, batch_size)
    dec_embed_input = tf.nn.embedding_lookup(dec_embeddings, dec_input)

    training_logits, inference_logits  = decoding_layer(dec_embed_input,
                                                        dec_embeddings,
                                                        enc_output,
                                                        enc_state,
                                                        vocab_size,
                                                        inputs_length,
                                                        targets_length,
                                                        max_target_length,
                                                        rnn_size,
                                                        vocab_to_int,
                                                        keep_prob,
                                                        batch_size,
                                                        num_layers,
                                                        direction)

    return training_logits, inference_logits
|#

char-dict = create-char-to-int-dict(TEXT)
sentences = convert-sentences-to-integers(get-sentences(TEXT), char-dict)
noisy-sentences = map(add-noise(_, 0.9, char-dict), sentences)

get-batches(sentences, 5, 0.9, char-dict)
