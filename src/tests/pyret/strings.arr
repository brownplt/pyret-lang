#lang pyret

check:
  "a".substring(0, 5) raises "End index is past"
  "a".substring(52, 0) raises "Start index is past"
  "a-str".substring(3, 1) raises "end to be greater than start"
  "a-str".substring(-1, 1) raises "non-negative"
  "abc".substring(0, 1) is "a"
  "abc".substring(1, 3) is "bc"

  "".char-at(4) raises "Index too large"
  "abc".char-at(2) is "c"
end
