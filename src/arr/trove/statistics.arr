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

fun mean(l :: L.List<Number>) -> Number:
  doc: "Find the average of a list of numbers"
  if L.length(l) == 0:
    raise("The input list is empty")
  else:
    math.sum(l) / L.length(l)
  end
end

fun median(l :: L.List) -> Number:
  doc: "returns the median element of the list"

  fun is-odd(n :: Number) -> Boolean:
    num-modulo(n, 2) == 1
  end

  sorted = l.sort()
  size = L.length(sorted)
  index_of_median = num-floor(size / 2)
  cases (L.List) sorted:
    |empty => raise("The input list is empty")
    |link(first, rest) => 
      if is-odd(size):
        sorted.get(index_of_median)
      else:
        (sorted.get(index_of_median) + sorted.get(index_of_median - 1)) / 2
      end
  end
end

fun modes(l :: L.List) -> L.List<Number>:
  doc: ```returns a list containing each mode of the input list, 
       or an empty list if the input list is empty```
  length-of-repeated = lam(lst :: L.List<Number>) -> L.List<{Number; Number}>:
    aggregate = lam(prev :: L.List<{Number; Number}>, current-val :: Number) 
      -> L.List<{Number; Number}>:
      
      cases (L.List) prev:
        | empty => [L.list: {current-val; 1}]
        | link(first, rest) => 
          prev-val = first.{0}
          prev-count = first.{1}
          
          if within(~0.0)(prev-val, current-val):
            L.link({prev-val; prev-count + 1}, rest)
          else:
            L.link({current-val; 1}, prev)
          end
      end
    end
    
    L.fold(aggregate, [L.list: ], lst)
  end
  
  sorted-list = l.sort()
  number-counts = length-of-repeated(sorted-list)
  
  find-maximal-appearing = lam(prev :: L.List<{Number; Number}>, 
      current-elt :: {Number; Number}) -> L.List<{Number; Number}>:
    
    cases (L.List) prev:
      | empty => [L.list: current-elt]
      | link(f, r) =>
        current-elt-count = current-elt.{1}
        list-head-count = f.{1}
        
        if current-elt-count > list-head-count:
          [L.list: current-elt]
        else if current-elt-count == list-head-count:
          L.link(current-elt, prev)
        else:
          prev
        end
    end
  end

  maximal-appearing = L.fold(find-maximal-appearing, [L.list: ], number-counts)
  L.map(lam(x :: {Number; Number}): x.{0} end, maximal-appearing)
  
end

fun mode(l :: L.List) -> Number:
  doc: ```returns an option containing the mode of the
       input list, or raises an error if input list is empty.
       If the input has multiple modes, this function
       returns the mode with the least value```

  cases (L.List) modes(l):
    | empty => raise("The input list is empty")
    | link(f, r) => f
  end
end

fun stdev(l :: L.List) -> Number:
  doc: ```returns the standard deviation of the list 
       of numbers, or raises an error if the list is empty```
  reg-mean = mean(l)
  sq-diff = l.map(lam(k): num-expt((k - reg-mean), 2) end)
  sq-mean = mean(sq-diff)
  num-sqrt(sq-mean)
end

fun linear-regression(x :: L.List<Number>, y :: L.List<Number>) -> (Number -> Number):
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

		fun predictor(in :: Number) -> Number:
			(beta * in) + alpha
		end

		predictor
	end
end

fun r-squared(x :: L.List<Number>, y :: L.List<Number>, f :: (Number -> Number)) -> Number:
	y-mean = mean(y)
  f-of-x = L.map(f, x)
  ss-tot = math.sum(L.map(lam(yi): num-sqr(yi - y-mean) end, y))
  ss-res = math.sum(L.map2(lam(yi, fi): num-sqr(yi - fi) end, y, f-of-x))

  if within-abs(0.0000001)(~0, ss-res):
    1
  else:
    1 - (ss-res / ss-tot)
  end
end

