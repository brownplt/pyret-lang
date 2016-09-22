import equality as E
include math

check "min and max":
  
  max([list:]) raises "empty"
  min([list:]) raises "empty"

  max([list: 1, 2, ~3]) is-roughly ~3
  max([list: -1, 0, 1]) is 1

  min([list: -1, -10, 1/2]) is -10
  min([list: 1/10, 1/100, 1/100000000000000000000000]) is 1/100000000000000000000000

  arg-min([list:]) raises "empty"
  arg-max([list:]) raises "empty"

  arg-min([list: 2]) is 0
  arg-max([list: -2]) is 0

  arg-min([list: ~0, -1, 2, -0.99]) is 1
  arg-max([list: 1/100, 1/1000, 2, 2/10000]) is 3

  sum([list:]) is 0
  sum([list: 0, 1]) is 1
  sum([list: -1, ~2, 3, -3]) is-roughly ~1

end
