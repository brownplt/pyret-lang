#lang pyret

check:
  "a".substring(0, 5) raises "End index is past"
  "a".substring(52, 0) raises "Start index is past"
  "a-str".substring(3, 1) raises "end to be greater than start"
  "a-str".substring(-1, 1) raises "non-negative"

  "".char-at(4) raises "Index too large"
end
