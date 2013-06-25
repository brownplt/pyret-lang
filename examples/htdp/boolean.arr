#lang pyret

true and true
true and false
true or false
false or false
not false

(("hello world".length() == ("11".tonumber()))
  or ("hello world" == "good morning"))
 and (("hello world".length() + 60) >= 80)
