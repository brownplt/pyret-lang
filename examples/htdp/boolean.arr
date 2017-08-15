#lang pyret

true and true
true and false
true or false
false or false
not(false)

((string-length("hello world") == string-to-number("11").or-else("failed"))
  or ("hello world" == "good morning"))
 and ((string-length("hello world") + 60) >= 80)
