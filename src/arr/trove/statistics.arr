#lang pyret/library

provide {
    mean: mean,
    median: median,
    modes: modes,
    has-mode: has-mode,
    mode-smallest: mode-smallest,
    mode-largest: mode-largest,
    mode-any: mode-any,
    variance: variance,
    stdev: stdev,
    variance-sample: variance-sample,
    stdev-sample: stdev-sample,
    linear-regression: linear-regression,
    multiple-regression: multiple-regression,
    r-squared: r-squared,
    z-test: z-test,
    t-test: t-test,
    t-test-paired: t-test-paired,
    t-test-pooled: t-test-pooled,
    t-test-independent: t-test-independent,
    chi-square: chi-square
} end
provide-types *
import global as _
include lists
import error as E
import math as math
import string-dict as SD
import multiple-regression as MR

fun mean(l :: List<Number>) -> Number:
  doc: "Find the average of a list of numbers"
  if length(l) == 0:
    raise(E.message-exception("The input list is empty"))
  else:
    math.sum(l) / length(l)
  end
end

fun median(l :: List) -> Number:
  doc: "returns the median element of the list"
  block:
    when(is-empty(l)):
     raise(E.generic-type-mismatch(l, "Non-Empty list")) 
    end

    sorted = builtins.raw-array-sort-nums(raw-array-from-list(l), true)
    size = raw-array-length(sorted)
    index-of-median = num-floor(size / 2)
    if size == 0: raise(E.message-exception("The input list is empty"))
    else:
      if num-modulo(size, 2) == 1:
        raw-array-get(sorted, index-of-median)
      else:
        (raw-array-get(sorted, index-of-median) + raw-array-get(sorted, index-of-median - 1)) / 2
      end
    end
  end
end

fun group-and-count(l :: List<Number>) -> List<{Number; Number}> block:
  doc: "Returns a list of all the values in the list, together with their counts, sorted descending by value"
  
  sorted = builtins.raw-array-sort-nums(raw-array-from-list(l), false)
  size = raw-array-length(sorted)

  if size == 0: empty
  else:
    first = raw-array-get(sorted, 0)
    {front; acc} = for raw-array-fold({{cur; count}; lst} from {{first; 0}; empty}, n from sorted, _ from 0):
        if within(~0.0)(cur, n):
          {{cur; count + 1}; lst}
        else:
          {{n; 1}; link({cur; count}, lst)}
        end
      end
    link(front, acc)
  end
end
  
fun modes-helper(l :: List<Number>) -> {Number; List<Number>}:
  doc: ```Returns the frequency of the modes and a list containing each mode of the input list, 
       or an empty list if the input list is empty.  The modes are returned in sorted order```

  num-counts = group-and-count(l)
  max-repeat = for fold(max from 0, {_; count} from num-counts):
    num-max(max, count)
  end
  { max-repeat;
    # This fold reverses the order of num-counts, so it winds up in increasing order
    for fold(acc from empty, {num; count} from num-counts):
      if count == max-repeat:
        link(num, acc)
      else:
        acc
      end
    end }
end

fun modes(l :: List<Number>) -> List<Number>:
  doc: ```returns a list containing each mode of the input list, or empty if there are no duplicate values```
  {max-repeat; ms} = modes-helper(l)
  if max-repeat < 2:
    [list: ]
  else:
    ms
  end
end

fun has-mode(l :: List<Number>) -> Boolean:
  doc: "Returns true if the list contains at least one mode, i.e. a duplicated value"
  num-counts = group-and-count(l)
  max-repeat = for fold(max from 0, {_; count} from num-counts):
    num-max(max, count)
  end
  max-repeat > 1
end

fun mode-smallest(l :: List<Number>) -> Number:
  doc: "Returns the smallest mode of the list, if there is one"
  {max-repeat; ms} = modes-helper(l)
  if max-repeat == 0:
    raise(E.message-exception("The input list is empty"))
  else if max-repeat == 1:
    raise(E.message-exception("There are no duplicate values in this list"))
  else:
    ms.first
  end
end

fun mode-largest(l :: List<Number>) -> Number:
  doc: "Returns the largest mode of the list, if there is one"
  {max-repeat; ms} = modes-helper(l)
  if max-repeat == 0:
    raise(E.message-exception("The input list is empty"))
  else if max-repeat == 1:
    raise(E.message-exception("There are no duplicate values in this list"))
  else:
    ms.last()
  end
end

fun mode-any(l :: List<Number>) -> Number:
  doc: "Returns some mode of the list, if there is one"
  {max-repeat; ms} = modes-helper(l)
  if max-repeat == 0:
    raise(E.message-exception("The input list is empty"))
  else if max-repeat == 1:
    raise(E.message-exception("There are no duplicate values in this list"))
  else:
    ms.get(num-random(ms.length()))
  end
end

fun variance(l :: List) -> Number:
  doc: ```returns the variance of the list
       of numbers, or raises an error if the list is empty```
  reg-mean = mean(l)
  sq-diff = l.map(lam(k): num-expt((k - reg-mean), 2) end)
  sq-mean = mean(sq-diff)
  sq-mean
end

fun stdev(l :: List) -> Number:
  doc: ```returns the standard deviation of the list
       of numbers, or raises an error if the list is empty```
  num-sqrt(variance(l))
end

fun variance-sample(l :: List) -> Number:
  doc: ```returns the variance of the list
       of numbers, or raises an error if the list is empty```
  len = l.length()
  reg-mean = mean(l)
  sq-diff = l.map(lam(k): num-expt((k - reg-mean), 2) end)
  sq-mean = math.sum(sq-diff) / (len - 1)
  sq-mean
end

fun stdev-sample(l :: List) -> Number:
  doc: ```returns the standard deviation of the list
       of numbers, or raises an error if the list is empty```
  num-sqrt(variance-sample(l))
end

# please see: https://online.stat.psu.edu/stat462/

fun multiple-regression(x_s_s :: List<Any>, y_s :: List<Number>) -> (Any -> Number):
  doc: "multiple-regression"
  MR.multiple-regression(x_s_s, y_s)
end

fun linear-regression(x-s :: List<Number>, y-s :: List<Number>) -> (Number -> Number):
  doc: "returns a linear predictor function for a single independent variable"
  x-s-n = x-s.length()
  if x-s-n <> y-s.length():
    raise(E.message-exception("linear-regression: input lists must have equal lengths"))
  else if x-s-n < 2:
    raise(E.message-exception("linear-regression: input lists must have at least 2 elements each"))
  else:
    predictor1 = MR.multiple-regression(x-s.map(lam(x1 :: Number): {x1} end), y-s)
    fun predictor2(x2 :: Number) -> Number:
      predictor1({x2})
    end
    predictor2
  end
end

fun r-squared(x :: List<Number>, y :: List<Number>, f :: (Number -> Number)) -> Number:
  shadow x = map(num-to-roughnum, x)
  shadow y = map(num-to-roughnum, y)
  y-mean = mean(y)
  f-of-x = map(f, x)
  ss-tot = math.sum(map(lam(yi): num-sqr(yi - y-mean) end, y))
  ss-res = math.sum(map2(lam(yi, fi): num-sqr(yi - fi) end, y, f-of-x))

  if within-abs(0.0000001)(~0, ss-res):
    1
  else:
    1 - (ss-res / ss-tot)
  end
end

fun z-test(sample-list :: List, population-sd :: Number, population-mean :: Number) -> Number:
  doc: "z-test"
  sample-size = sample-list.length()
  sample-mean = mean(sample-list)
  sd-of-mean = population-sd / num-sqrt(sample-size)
  (sample-mean - population-mean) / sd-of-mean
end

fun t-test(sample-list :: List, population-mean :: Number) -> Number:
  doc: "t-test"
  sample-size = sample-list.length()
  sample-mean = mean(sample-list)
  estimated-population-variance = variance-sample(sample-list)
  variance-of-mean = estimated-population-variance / sample-size
  (sample-mean - population-mean) / num-sqrt(variance-of-mean)
end

# please see:
#   https://en.wikipedia.org/wiki/Student's_t-test
#   https://www.investopedia.com/terms/t/t-test.asp
#     (this has a typo for the pooled t-test, corrected here)

fun t-test-paired(l1 :: List, l2 :: List) -> Number:
  doc: "t-test-paired"
  n = l1.length()
  n2 = l2.length()
  if n <> n2:
    raise(E.message-exception("t-test-paired: input lists must have equal lengths"))
  else if n == 0:
    raise(E.message-exception("t-test-paired: input lists must have at least one element"))
  else:
    diffs = map2(lam(x1, x2): x1 - x2 end, l1, l2)
    diffs-mean = mean(diffs)
    s-hat = stdev-sample(diffs)
    diffs-mean / (s-hat / num-sqrt(n))
  end
end

fun t-test-pooled(sample-list-1 :: List, sample-list-2 :: List) -> Number:
  doc: "t-test-pooled"
  n1 = sample-list-1.length()
  n2 = sample-list-2.length()
  if (n1 == 0) or (n2 == 0):
    raise(E.message-exception("t-test-pooled: input lists must have at least one element"))
  else:
    m1 = mean(sample-list-1)
    m2 = mean(sample-list-2)
    v1 = variance-sample(sample-list-1)
    v2 = variance-sample(sample-list-2)
    v = (((n1 - 1) * v1) + ((n2 - 1) * v2)) / ((n1 + n2) - 2)
    (m1 - m2) / num-sqrt((v / n1) + (v / n2))
  end
end

fun t-test-independent(sample-list-1 :: List, sample-list-2 :: List) -> Number:
  doc: "t-test-independent"
  n1 = sample-list-1.length()
  n2 = sample-list-2.length()
  if (n1 == 0) or (n2 == 0):
    raise(E.message-exception("t-test-independent: input lists must have at least one element"))
  else:
    m1 = mean(sample-list-1)
    m2 = mean(sample-list-2)
    v1 = variance-sample(sample-list-1)
    v2 = variance-sample(sample-list-2)
    (m1 - m2) / num-sqrt((v1 / n1) + (v2 / n2))
  end
end

fun chi-square(observed-values :: List, predicted-values :: List) -> Number:
  doc: "chi-square"
  math.sum(map2(lam(observed-value, predicted-value):
                  num-expt(observed-value - predicted-value, 2) / predicted-value end,
                observed-values, predicted-values))
end
