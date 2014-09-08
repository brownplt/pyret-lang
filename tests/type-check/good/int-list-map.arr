
data IntList:
  | int-empty
  | int-link(n :: Number, rest :: IntList)
end

fun int-map(lst :: IntList, f :: (Number -> Number)):
  cases(IntList) lst:
    | int-empty => 
      int-empty
    | int-link(n, rest) =>
      int-link(f(n), int-map(rest, f))
  end
end

int-map(int-link(5, int-link(4, int-link(3, int-empty))), _ + 2)
