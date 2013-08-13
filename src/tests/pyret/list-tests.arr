#lang pyret

fun _(): where:
  checkers.check-equals("", [].join-str(", "), "")
  checkers.check-equals("", [1].join-str(", "), "1")
  checkers.check-equals("", [1,2,3].join-str(", "), "1, 2, 3")
end