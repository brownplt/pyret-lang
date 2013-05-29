#lang pyret

import "simple-tests.arr" as Simple
import "math.arr" as Math
import test as Test

simpl_results = Simple.run-tests()
math_results = Math.run-tests()

Test.format-results()

