import equality as E
import math as math

check "min and max":
  
  math.max([list:]) raises "empty"
  math.min([list:]) raises "empty"

  math.max([list: 1, 2, ~3]) is-roughly ~3
  math.max([list: -1, 0, 1]) is 1

  math.min([list: -1, -10, 1/2]) is -10
  math.min([list: 1/10, 1/100, 1/100000000000000000000000]) is 1/100000000000000000000000

	math.arg-min([list:]) raises "empty"
	math.arg-max([list:]) raises "empty"

  math.arg-min([list: 2]) is 0
	math.arg-max([list: -2]) is 0

	math.arg-min([list: ~0, -1, 2, -0.99]) is 1
	math.arg-max([list: 1/100, 1/1000, 2, 2/10000]) is 3

	math.sum([list:]) is 0
	math.sum([list: 0, 1]) is 1
	math.sum([list: -1, ~2, 3, -3]) is-roughly ~1

end
