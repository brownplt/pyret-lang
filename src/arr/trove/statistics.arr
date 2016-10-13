#lang pyret/library

provide *
provide-types *
import global as _
import option as O
import either as E
import equality as equality
import valueskeleton as VS
import lists as L
import math as math

empty = L.empty
# A StatModel is a data type for representing
# statistical models.  We create this type so that
# users can extract, coefficients and meta-data from
# models, as well as apply the model.
#
# This could be extended to include general linear 
# regression, exponential and logistic regression, etc.

data StatModel:
  | simple-linear-model(alpha :: Number, beta :: Number, r-sqr :: Number) with:

    method predictor(self :: StatModel) -> (Number -> Number):
      doc: "the function predicting the value of a dependent variable"
      lam(X): (self.beta * X) + self.alpha end
    end,

    method apply(self :: StatModel, l :: L.List<Number>) -> L.List<Number>:
      doc: "applies the predictor function to a sample set of the independent variable"
      L.map(self.predictor(), l)
    end,

    method r-squared(self :: StatModel) -> Number:
      doc: "gives the coefficient of correlation"
      self.r-sqr
    end
end

fun mean(l :: L.List<Number>) -> Number:
  doc: "Find the average of a list of numbers"
  if L.length(l) == 0:
    raise("You can't take the average of an empty list")
  else:
    math.sum(l) / L.length(l)
  end
end

fun median(l :: L.List):
  doc: "returns the median element of the list"
  sorted = l.sort()
  index = L.length(sorted)
  cases (L.List) sorted:
    |empty => raise("The list is empty")
    |link(first, rest) => sorted.get(num-floor(index / 2))
  end
end

fun stdev(l :: L.List) -> Number:
  doc: "returns the standard deviation of the list of numbers"
  reg-mean = mean(l)
  sq-diff = l.map(lam(k): num-expt((k - reg-mean), 2) end)
  sq-mean = mean(sq-diff)
  num-sqrt(sq-mean)
end

fun chi-sqr(counts :: L.List<L.List<Number>>) -> Number:
  doc: "returns the chi squared correlation between two observed variables"
  
  fun row-wise-sums(observed :: L.List<L.List<Number>>) -> L.List<Number>:
    cases (L.List) observed:
      | empty => empty
      | link(first, rest) =>
      
      num-rows = L.length(first)
      indices = L.range(0, num-rows)
      L.map(
        lam(ind): math.sum(L.map(lam(entry): L.get(entry, ind) end, observed)) end,
        indices)
    end
  end
  
  fun column-wise-sums(observed :: L.List<L.List<Number>>) -> L.List<Number>:
    L.map(lam(col): math.sum(col) end, observed)
  end
  
  fun expected-table-column(row-sums :: L.List<Number>, col-sum :: Number, 
    total :: Number) -> L.List<Number>:
    
    L.map(
      lam(row): (row * col-sum) / total end,
      row-sums)
  end

  fun expected-table(observed :: L.List<L.List<Number>>) -> L.List<L.List<Number>> :
    num-columns = L.length(observed)
    row-sums = row-wise-sums(observed)
    col-sums = column-wise-sums(observed)
    total = math.sum(row-sums)
  
    L.map(
      lam(col): expected-table-column(row-sums, col, total) end,
      col-sums)
  end

  fun flatten(table-list :: L.List<L.List<Number>>) -> L.List<Number> :
    cases (L.List) table-list:
      | empty => empty
      | link(a, rest) => L.append(a, flatten(rest))
    end
  end

  obs = L.distinct(L.map(lam(ob): ob.length() end, counts))
  
  if L.length(obs) > 1:
    raise("Mismatched list dimensions")
  else:
    expected = expected-table(counts)
    math.sum(L.map2(lam(e, o): num-sqr(e - o) / e end, 
      flatten(expected), flatten(counts)))
  end
end

fun lin-reg-2V(x :: L.List<Number>, y :: L.List<Number>) -> StatModel:
  doc: "returns a linear regression model calculated with ordinary least squares"
  if x.length() <> y.length():
    raise("lin-reg-2V: input lists must have equal lengths")
  else if x.length() < 2:
    raise("lin-reg-2V: input lists must have at least 2 elements each")
  else:
    xpt_xy = math.sum(L.map2(lam(xi, yi): xi * yi end, x, y))
    xpt_x_xpt_y = (math.sum(x) * math.sum(y)) / x.length()
    covariance = xpt_xy - xpt_x_xpt_y
    v1 = math.sum(L.map(lam(n): n * n end, x))
    v2 = (math.sum(x) * math.sum(x)) / x.length()
    variance = v1 - v2
    beta = covariance / variance
    alpha = mean(y) - (beta * mean(x))

    y-mean = mean(y)
    f = L.map(lam(xi): (beta * xi) + alpha end, x)
    ss-tot = math.sum(L.map(lam(yi): num-sqr(yi - y-mean) end, y))
    ss-res = math.sum(L.map2(lam(yi, fi): num-sqr(yi - fi) end, y, f))

    r-sqr = if within-abs(0.0000001)(~0, ss-res):
      1
    else:
      1 - (ss-res / ss-tot)
    end
 
    simple-linear-model(alpha, beta, r-sqr)
  end
end

