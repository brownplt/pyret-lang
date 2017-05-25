import equality as E
include statistics

check "numeric helpers":
  
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

  stdev([list: 3, 4, 5, 6, 7]) is%(within(0.01)) 1.41
  stdev([list: 1, 1, 1, 1]) is-roughly ~0
  stdev([list:]) raises "empty"

end

check "linear regression":
	linear-regression([list: 0], [list: ]) raises "lists must have equal lengths"
	linear-regression([list: 0], [list: 1]) raises "lists must have at least 2 elements each"

	x1 = [list: 0, 1, 2]
	y1 = [list: -5, -3, -1]

  f = linear-regression(x1, y1)

	f(0) is -5
	f(2.5) is 0

	r-squared(x1, y1, f) is 1

	x2 = [list: -3, 1, 2, 4]
	y2 = [list: ~4, ~1.5, ~0, -2.2]

  g = linear-regression(x2, y2)

  g(0) is%(within-abs(0.001)) 1.69423
  g(1.94911) is%(within-abs(0.001)) 0.0

  r-squared(x2, y2, g) is%(within-abs(0.00001)) 0.964508

end

