#lang pyret/library

provide *
provide-types *
import global as _
import option as O
import either as E
import equality as equality
import valueskeleton as VS

fun sum(l :: List<Number>) -> Number:
	lists.fold(lam(a,b): a + b end, 0, l)
end

fun mean(l :: List<Number>) -> Number:
  doc: "Find the average of a list of numbers"
  if length(l) == 0:
    raise("You can't take the average of an empty list")
  else:
    sum(l) / length(l)
  end
end

fun min(l :: List):
  doc: "Find the minimum element of a list according to the built in ordering of elements"
  cases (List) l:
    |empty => raise("The list is empty")
    |link(first, rest) => min-helper(first, rest)
  end
end

fun min-helper(curr-min, l :: List):
  cases (List) l:
    |empty => curr-min
    |link(first, rest) =>
      if first < curr-min:
        min-helper(first, rest)
      else:
        min-helper(curr-min, rest)
      end
  end
end

fun max(l :: List):
  doc: "Find the maximum element of a list according to the built in ordering of elements"
  cases (List) l:
    |empty => raise("The list is empty")
    |link(first, rest) => max-helper(first, rest)
  end
end

fun max-helper(curr-max, l :: List):
  cases (List) l:
    |empty => curr-max
    |link(first, rest) =>
      if first > curr-max:
        max-helper(first, rest)
      else if first <= curr-max:
        max-helper(curr-max, rest)
      end
  end
end

fun median(l :: List):
  doc: "returns the median element of the list"
  sorted = l.sort()
  index = length(sorted)
  cases (List) sorted:
    |empty => raise("The list is empty")
    |link(first, rest) => sorted.get(num-floor(index / 2))
  end
end

fun stdev(l :: List) -> Number:
  doc: "returns the standard deviation of the list of numbers"
  reg-mean = mean(l)
  sq-diff = l.map(lam(k): num-expt((k - reg-mean), 2) end)
  sq-mean = mean(sq-diff)
  num-sqrt(sq-mean)
end

fun distinct(l :: List) -> List:
  doc: "returns a list with exactly the distinct elements of the original list removing the first instance"
  cases (List) l:
    | empty => empty
    | link(first, rest) =>
      cases(equality.EqualityResult) member3(rest, first):
        | NotEqual(_, _, _) => link(first, distinct(rest))
        | Unknown(_, _, _) => link(first, distinct(rest))
        | Equal => distinct(rest)
      end
  end
end

