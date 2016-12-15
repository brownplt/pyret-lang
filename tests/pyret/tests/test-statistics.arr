import equality as E
include statistics

check "basic statistical functions":
  
  mean([list:]) raises "empty"
  mean([list: 1/2]) is 1/2
  mean([list: 1, 3, 4]) is 8/3
  mean([list: ~1, 3, 4]) is%(within(0.01)) 8/3
  mean([list: "a"]) raises "num-string-binop-error"

  median([list: 1, 2, 3]) is 2
  median([list: 1.2, ~1.2]) is-roughly ~1.2

  # assuming the sort is stable, these make sense
  median([list: ~1, 1, ~1]) is 1
  median([list: 1, ~1, ~1]) is-roughly ~1

  # Even numbered lists
  median([list: ]) raises "empty"
  median([list: -1, 1]) is 0
  median([list: 1, 2, 3, 4]) is 2.5
  median([list: -1, 0, ~2, 3]) is-roughly ~1
  median([list: ~0, ~1, ~2, ~2, ~6, ~8]) is-roughly ~2

  # Mode
  mode([list: ]) raises "empty"
  mode([list: 1]) is 1
  mode([list: 1, 1, 2]) is 1
  mode([list: -1, 0, -1, 2, 3, -33, ~0.1]) is -1
  mode([list: ~2, ~1.0002, ~2, ~1.0001, ~1]) is-roughly ~2

  # For multimode distributions, returns smallest mode
  mode([list: -1, 0, 1, 2]) is -1
  mode([list: ~0.1, ~0.2, ~0.2, ~0.1]) is-roughly ~0.1

  # Modes (Plural) returns each mode in a list with multiple
  modes([list: ]) raises "empty"
  modes([list: 1]) is [list: 1]
  modes([list: -1, 0, 1, 2]) is [list: -1, 0, 1, 2]
  modes([list: ~0.1, ~0.2, ~0.2, ~0.1]) is-roughly [list: ~0.1, ~0.2]
  modes([list: -1, 2, -1, 2, -1]) is [list: -1]
  modes([list: 1, 1, 2, 2, 3, 3, 3]) is [list: 3]

  stdev([list:]) raises "empty"
  stdev([list: 5]) is 0
  stdev([list: 3, 4, 5, 6, 7]) is%(within(0.01)) 1.41
  stdev([list: 1, 1, 1, 1]) is-roughly ~0
  
end

check "linear regression":
  lin-reg-2V([list: 0], [list: ]) raises "lists must have equal lengths"
  lin-reg-2V([list: 0], [list: 1]) raises "lists must have at least 2 elements each"

  f = lin-reg-2V([list: 0, 1, 2], [list: -5, -3, -1])
  f.r-squared() is 1

  fp = f.predictor()
  fp(0) is -5
  fp(2.5) is 0

  g = lin-reg-2V([list: -3, 1, 2, 4], [list: ~4, ~1.5, ~0, -2.2])
  g.r-squared() is%(within-abs(0.00001)) 0.964508

  gp = g.predictor()
  gp(0) is%(within-abs(0.001)) 1.69423
  gp(1.94911) is%(within-abs(0.001)) 0.0

end

