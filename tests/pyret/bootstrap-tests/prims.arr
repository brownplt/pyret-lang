
examples:
  add(1, 2) is 3
  add(1/2, 10000/20000) is 1
  add("nan", 5) raises "Number"
  add(5, "nan") raises "Number"

  sub(2, 1) is 1
  sub("nan", 3) raises "Number"

  div(6, 2) is 3
  div(5, 2) is 5/2

  mul(1, 3) is 3
  mul(-1, -3) is 3

  mul(1/3, 3) is 1
  mul(1/2, 4) is 2

  nums-equal(1/100000000000000, 10000/1000000000000000000) is true
  nums-equal(1, 2) is false
  nums-equal(0, 0) is true
  nums-equal(555555555555555 * 666666666666 * 7, 7 * 555555555555555 * 666666666666) is true
  nums-equal(true, 5) raises "Number"
  nums-equal(33, false) raises "Number"

  num-tostring("a") raises "Number"
  num-tostring(5) is "5"
  num-tostring(1/2) is "1/2"

  strings-equal("abcd", "abcd") is true
  strings-equal("", "") is true
  strings-equal("a", "") is false
  strings-equal("", "b") is false
  strings-equal("abcd efgh", "abcdefgh") is false
  strings-equal(42, "a") raises "String"
  strings-equal("a", 55) raises "String"

  either(true, true) is true
  either(true, false) is true
  either(false, true) is true
  either(false, false) is false
  either("nab", false) raises "Boolean"
  either(false, "nab") raises "Boolean"

  both(true, true) is true
  both(true, false) is false
  both(false, true) is false
  both(false, false) is false
  both("nab", false) raises "Boolean"
  both(false, "nab") raises "Boolean"

  not(true) is false
  not(false) is true
  not("nab") raises "Boolean"
end

