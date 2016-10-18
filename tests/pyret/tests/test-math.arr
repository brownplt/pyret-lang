import equality as E
include math

check "min and max":
  
  max([list:]) raises "empty"
  min([list:]) raises "empty"

  max([list: 1, 2, ~3]) is-roughly ~3
  max([list: -1, 0, 1]) is 1
  max([list: 3, 3, 4, 3, 4]) is 4

  min([list: -1, -10, 1/2]) is -10
  min([list: 1/10, 1/100, 1/100000000000000000000000]) is 1/100000000000000000000000
  min([list: 10, 9, 9, 11]) is 9
  min([list: ~2, ~5, ~2, ~4]) is-roughly ~2

  arg-min([list:]) raises "empty"
  arg-max([list:]) raises "empty"

  arg-min([list: 2]) is 0
  arg-max([list: -2]) is 0

  arg-min([list: 0, 1]) is 0
  arg-min([list: 2, 1]) is 1
  arg-min([list: ~0, -1, 2, -0.99]) is 1
  arg-min([list: 1, 2, 1, 0]) is 3
  arg-min([list: 1, 0.5, 0.2, 0.6, 0.2]) is 2

  arg-max([list: 1, 0]) is 0
  arg-max([list: 4, 5]) is 1
  arg-max([list: 1/100, 1/1000, 2, 2/10000]) is 2
  arg-max([list: 1, 1, 2, 3, 3]) is 3
  arg-max([list: ~1, ~2, ~1, ~0.5]) is 1

  sum([list:]) is 0
  sum([list: 0, 1]) is 1
  sum([list: -1, ~2, 3, -3]) is-roughly ~1

end
