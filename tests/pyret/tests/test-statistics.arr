import equality as E
import math as math

check "numeric helpers":
  
  math.mean([list:]) raises "empty"
  math.mean([list: 1/2]) is 1/2
  math.mean([list: 1, 3, 4]) is 8/3
  math.mean([list: ~1, 3, 4]) is%(within(0.01)) 8/3
  math.mean([list: "a"]) raises "num-string-binop-error"

  math.median([list: 1, 2, 3]) is 2
  math.median([list: 1.2, ~1.2]) is-roughly ~1.2

  # assuming the sort is stable, these make sense
  math.median([list: ~1, 1, ~1]) is 1
  math.median([list: 1, ~1, ~1]) is-roughly ~1

  math.stdev([list: 3, 4, 5, 6, 7]) is%(within(0.01)) 1.41
  math.stdev([list: 1, 1, 1, 1]) is-roughly ~0
  math.stdev([list:]) raises "empty"

end

check "linear regression":
  math.lin-reg-2V([list: 0], [list: ]) raises "lists must have equal lengths"
  math.lin-reg-2V([list: 0], [list: 1]) raises "lists must have at least 2 elements each"

  f = math.lin-reg-2V([list: 0, 1, 2], [list: -5, -3, -1]).predictor()
  f(0) is -5
  f(2.5) is 0

  g = math.lin-reg-2V([list: -3, 1, 2, 4], [list: ~4, ~1.5, ~0, -2.2]).predictor()
  g(0) is%(within-abs(0.001)) 1.69423
  g(1.94911) is%(within-abs(0.001)) 0.0

end

