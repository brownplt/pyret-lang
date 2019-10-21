#lang pyret/library

provide {
    mean: mean,
    median: median,
    modes: modes,
    has-mode: has-mode,
    mode-smallest: mode-smallest,
    mode-largest: mode-largest,
    mode-any: mode-any,
    stdev: stdev,
    stdev-sample: stdev-sample,
    linear-regression: linear-regression,
    r-squared: r-squared
} end
provide-types *
import global as _
include lists
import error as E
import math as math
import string-dict as SD

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
  
fun stdev(l :: List) -> Number:
  doc: ```returns the standard deviation of the list 
       of numbers, or raises an error if the list is empty```
  reg-mean = mean(l)
  sq-diff = l.map(lam(k): num-expt((k - reg-mean), 2) end)
  sq-mean = mean(sq-diff)
  num-sqrt(sq-mean)
end

fun stdev-sample(l :: List) -> Number:
  doc: ```returns the standard deviation of the list 
       of numbers, or raises an error if the list is empty```
  len = l.length()
  reg-mean = mean(l)
  sq-diff = l.map(lam(k): num-expt((k - reg-mean), 2) end)
  sq-mean = math.sum(sq-diff) / (len - 1)
  num-sqrt(sq-mean)
end

fun linear-regression(x :: List<Number>, y :: List<Number>) -> (Number -> Number):
  doc: "returns a linear predictor function calculated with ordinary least squares regression"
  if x.length() <> y.length():
    raise(E.message-exception("linear-regression: input lists must have equal lengths"))
  else if x.length() < 2:
    raise(E.message-exception("linear-regression: input lists must have at least 2 elements each"))
  else:
    shadow y = map(num-to-roughnum, y)
    shadow x = map(num-to-roughnum, x)
    xpt-xy = math.sum(map2(lam(xi, yi): xi * yi end, x, y))
    xpt-x-xpt-y = (math.sum(x) * math.sum(y)) / x.length()
    covariance = xpt-xy - xpt-x-xpt-y
    v1 = math.sum(map(lam(n): n * n end, x))
    v2 = (math.sum(x) * math.sum(x)) / x.length()
    variance = v1 - v2
    beta = covariance / variance
    alpha = mean(y) - (beta * mean(x))

    fun predictor(in :: Number) -> Number:
      (beta * in) + alpha
    end

    predictor
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

