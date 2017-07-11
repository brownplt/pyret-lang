#lang pyret

check:
  to-string(5) is "5"
  string-to-number("5") is some(5)
  string-to-number("asdf") is none
end
