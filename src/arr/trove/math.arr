#lang pyret/library

provide *
provide-types *
import global as _
import option as O
import either as E
import equality as equality
import valueskeleton as VS
import lists as L

fun sum(l :: L.List<Number>) -> Number:
	L.fold(lam(a,b): a + b end, 0, l)
end

fun min(l :: L.List):
  doc: "Find the minimum element of a list according to the built in ordering of elements"
  cases (L.List) l:
    |empty => raise("The list is empty")
    |link(first, rest) => min-helper(first, rest)
  end
end

fun min-helper(curr-min, l :: L.List):
  cases (L.List) l:
    |empty => curr-min
    |link(first, rest) =>
      if first < curr-min:
        min-helper(first, rest)
      else:
        min-helper(curr-min, rest)
      end
  end
end

fun max(l :: L.List):
  doc: "Find the maximum element of a list according to the built in ordering of elements"
  cases (L.List) l:
    |empty => raise("The list is empty")
    |link(first, rest) => max-helper(first, rest)
  end
end

fun max-helper(curr-max, l :: L.List):
  cases (L.List) l:
    |empty => curr-max
    |link(first, rest) =>
      if first > curr-max:
        max-helper(first, rest)
      else if first <= curr-max:
        max-helper(curr-max, rest)
      end
  end
end

# TODO(SDooman): These can be abstracted in a nicer way after we get them out to students
fun arg-min(l :: L.List):
  doc: "Find the index of the minimal element in a list, or raises an error if list is empty"
  cases (L.List) l:
    | empty => raise("The list is empty")
    | link(first, rest) => arg-min-helper(first, 0, 1, rest)
  end
end

fun arg-min-helper(curr-min, min-ind :: Number, curr-ind :: Number, l :: L.List):
  cases (L.List) l:
  | empty => min-ind
  | link(first, rest) =>
    if first < curr-min:
      arg-min-helper(first, curr-ind, curr-ind + 1, rest)
    else:
      arg-min-helper(curr-min, min-ind, curr-ind + 1, rest)
    end
 end
end

fun arg-max(l :: L.List):
  doc: "Find the index of the maximal element in a list, or raises an error if list is empty"
  cases (L.List) l:
    | empty => raise("The list is empty")
    | link(first, rest) => arg-max-helper(first, 0, 1, rest)
  end
end

fun arg-max-helper(curr-max, max-ind :: Number, curr-ind :: Number, l :: L.List):
  cases (L.List) l:
  | empty => max-ind
  | link(first, rest) =>
    if first > curr-max:
      arg-max-helper(first, curr-ind, curr-ind + 1, rest)
    else:
      arg-max-helper(curr-max, max-ind, curr-ind + 1, rest)
    end
 end
end
