#lang reader "../src/lang/pyret.rkt"

true:and(true)
true:and(false)
true:or(false)
false:or(false)
false:not()

"hello world":length():equals("11":tonumber())
  :or("hello world":equals("good morning"))
    :and("hello world":length():add(60):greaterequal(80))
