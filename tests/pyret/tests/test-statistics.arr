import equality as E
import statistics as statistics

check "numeric helpers":
  
  statistics.mean([list: 1, 2, 3]) is 2
  statistics.mean([list:]) raises "empty"
  statistics.mean([list: "a"]) raises "num-string-binop-error"

  statistics.median([list: 1, 2, 3]) is 2
  statistics.median([list: 1.2, ~1.2]) is-roughly ~1.2

  # assuming the sort is stable, these make sense
  statistics.median([list: ~1, 1, ~1]) is 1
  statistics.median([list: 1, ~1, ~1]) is-roughly ~1

  statistics.max([list:]) raises "empty"
  statistics.min([list:]) raises "empty"

  statistics.max([list: 1, 2, ~3]) is-roughly ~3
  statistics.max([list: -1, 0, 1]) is 1

  statistics.min([list: -1, -10, 1/2]) is -10
  statistics.min([list: 1/10, 1/100, 1/100000000000000000000000]) is 1/100000000000000000000000

  statistics.stdev([list: 3, 4, 5, 6, 7]) is%(within(0.01)) 1.41
  statistics.stdev([list: 1, 1, 1, 1]) is-roughly ~0
  statistics.stdev([list:]) raises "empty"

  statistics.mean([list:]) raises "empty"
  statistics.mean([list: 1, 3, 4]) is 8/3
  statistics.mean([list: ~1, 3, 4]) is%(within(0.01)) 8/3
  statistics.mean([list: 1/2]) is 1/2

  statistics.distinct([list: ~1, ~1]) is-roughly [list: ~1, ~1]
  statistics.distinct([list: ~1, ~1, 1]) is-roughly [list: ~1, ~1, 1]
  statistics.distinct([list: ~1, ~1, 1, 1]) is-roughly [list: ~1, ~1, 1]
  statistics.distinct([list: ~1, ~2, ~3]) is-roughly [list: ~1, ~2, ~3]

end

check "linear regression":
  statistics.lin-reg-2V([list: 0], [list: ]) raises "lists must have equal lengths"
  statistics.lin-reg-2V([list: 0], [list: 1]) raises "lists must have at least 2 elements each"

  f = statistics.lin-reg-2V([list: 0, 1, 2], [list: -5, -3, -1]).predictor()
  f(0) is -5
  f(2.5) is 0

  g = statistics.lin-reg-2V([list: -3, 1, 2, 4], [list: ~4, ~1.5, ~0, -2.2]).predictor()
  g(0) is%(within-abs(0.001)) 1.69423
  g(1.94911) is%(within-abs(0.001)) 0.0

end

