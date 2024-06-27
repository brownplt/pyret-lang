import equality as E
include statistics
import lists as L

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

  # Even numbered lists
  median([list: ]) raises "Non-Empty list"
  median([list: -1, 1]) is 0
  median(L.shuffle([list: 1, 2, 3, 4])) is 2.5
  median(L.shuffle([list: -1, 0, ~2, 3])) is-roughly ~1
  median(L.shuffle([list: ~0, ~1, ~2, ~2, ~6, ~8])) is-roughly ~2

  # Mode
  has-mode([list: ]) is false
  has-mode([list: 1, 2, 3, 4, 5]) is false
  has-mode([list: 1, 2, 3, 2, 5]) is true
  
  mode-smallest([list: ]) raises "empty" 
  mode-smallest([list: 1]) raises "no duplicate values"
  mode-smallest([list: 1, 2, 3, 4, 5]) raises "no duplicate values"
  mode-smallest([list: 1, 1, 2]) is 1
  mode-smallest([list: -1, 0, -1, 2, 3, -33, ~0.1]) is -1
  mode-smallest([list: ~2, ~1.0002, ~2, ~1.0001, ~1]) is-roughly ~2

  # For multimode distributions, returns smallest/largest mode
  mode-smallest([list: -1, 0, 1, -1, 2, 2])  is -1
  mode-smallest([list: ~0.1, ~0.2, ~0.2, ~0.1]) is-roughly ~0.1
  mode-largest([list: ~0.1, ~0.2, ~0.2, ~0.1]) is-roughly ~0.2

  # Modes (Plural) returns each mode in a list with multiple
  modes([list: ]) is [list: ]
  modes([list: 1]) is [list: ]
  modes([list: -1, 0, 1, 2]) is [list: ]
  modes([list: ~0.1, ~0.2, ~0.2, ~0.1]) is-roughly [list: ~0.1, ~0.2]
  modes([list: -1, 2, -1, 2, -1]) is [list: -1]
  modes([list: 1, 1, 2, 2, 3, 3, 3]) is [list: 3]

  variance([list:]) raises "empty"
  variance([list: 5]) is 0
  variance([list: 3, 4, 5, 6, 7]) is%(within(0.01)) 2.0
  variance([list: 1, 1, 1, 1]) is-roughly ~0

  stdev([list:]) raises "empty"
  stdev([list: 5]) is 0
  stdev([list: 3, 4, 5, 6, 7]) is%(within(0.01)) 1.41
  stdev([list: 1, 1, 1, 1]) is-roughly ~0

  variance-sample([list: 3, 4, 5, 6, 7]) is%(within(0.01)) (10 / 4)
  variance-sample([list: 3]) raises "division by zero"
  variance-sample([list: 1, 1, 1, 1]) is-roughly ~0
  variance-sample([list:]) raises "empty"

  stdev-sample([list: 3, 4, 5, 6, 7]) is%(within(0.01)) num-sqrt(10 / 4)
  stdev-sample([list: 3]) raises "division by zero"
  stdev-sample([list: 1, 1, 1, 1]) is-roughly ~0
  stdev-sample([list:]) raises "empty"

  z-test([list: 1, 2, 3], 1.58, 2.58) is%(within(0.01)) -0.6358

  t-test([list: 1, 2, 3], 2.58) is%(within(0.01)) -1.0046

  t-test-paired([list: 1], [list: 2, 3]) raises "lists must have equal lengths"
  t-test-paired([list:], [list:]) raises "lists must have at least one element"
  t-test-paired([list: 1, 2, 3], [list: 4, 6, 8]) is%(within(0.01)) -6.928

  t-test-pooled([list:], [list: 1, 2, 3]) raises "lists must have at least one element"
  t-test-pooled([list: 1, 2, 3], [list: 4, 5, 6]) is%(within(0.01)) -3.674
  t-test-pooled([list: 1, 2, 3], [list: 4, 5, 6, 7]) is%(within(0.01)) -3.873

  t-test-independent([list:], [list: 1, 2, 3]) raises "lists must have at least one element"
  t-test-independent([list: 1, 2, 3], [list: 4, 5, 6]) is%(within(0.01)) -3.674
  t-test-independent([list: 1, 2, 3], [list: 4, 5, 6, 7]) is%(within(0.01)) -4.041


  chi-square([list: 1, 2, 3, 4], [list: 1, 2, 3, 4]) is 0
  chi-square([list: 1, 2, 3, 4], [list: 0.9, 1.8, 3.5, 4.7]) is%(within(0.01)) 0.209
end

check "polymorphic modes":
  has-mode([list: "a", "b", "c", "b", "c"]) is true
  has-mode([list: true, true, false]) is true
  has-mode([list: {1;2}, {1;3}, {1;2}]) is true
  has-mode([list: "a", true, true]) is true
  has-mode([list: "a", "b", "c"]) is false

  modes([list: "a", "b", "c", "b", "c"]) is [list: "b", "c"]
  modes([list: true, true, false]) is [list: true]
  modes([list: {1;2}, {1;3}, {1;2}]) is [list: {1;2}]
  modes([list: "a", true, true]) is [list: true]
  modes([list: "a", "b", "c"]) is [list: ]
end

check "linear regression":
	linear-regression([list: 0], [list: ]) raises "lists must have equal lengths"
	linear-regression([list: 0], [list: 1]) raises "lists must have at least 2 elements each"

	x1 = [list: 0, 1, 2]
	y1 = [list: -5, -3, -1]

  f = linear-regression(x1, y1)

	f(0) is-roughly -5
	f(2.5) is-roughly 0

	r-squared(x1, y1, f) is-roughly 1

	x2 = [list: -3, 1, 2, 4]
	y2 = [list: ~4, ~1.5, ~0, -2.2]

  g = linear-regression(x2, y2)

  g(0) is%(within-abs(0.001)) 1.69423
  g(1.94911) is%(within-abs(0.001)) 0.0

  r-squared(x2, y2, g) is%(within-abs(0.00001)) 0.964508

end

check "multiple regression":
  # multiple-regression function for single independent variable
  x-s-s = [list: [list: 4], [list: 4.5], [list: 5], [list: 5.5], [list: 6], [list: 6.5], [list: 7]]
  y-s   = [list: 33, 42, 45, 51, 53, 61, 62]
  pf1   = multiple-regression(x-s-s, y-s)
  pf1([list: 8]) is-roughly 73.3214
  #
  # check it matches linear-regression function on the same single variable
  x-s = [list: 4, 4.5, 5, 5.5, 6, 6.5, 7]
  pf2 = linear-regression(x-s, y-s)
  pf2(8) is-roughly 73.3214
  #
  # multiple-regression with two independent variables
  x-s-s-i = [list: [list: 4, 3], [list: 4.5, 2], [list: 5, 1.2], [list: 5.5, 4.5], [list: 6, 3.3], [list: 6.5, 10], [list: 7, 0]]
  y-s-i   = [list: 33, 42, 45, 51, 53, 61, 62]
  pf-i    = multiple-regression(x-s-s-i, y-s-i)
  pf-i([list: 8, 9]) is-roughly 74.52888
end
