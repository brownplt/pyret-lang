#lang pyret

import "simple-tests.arr" as Simple
import "seal-tests.arr" as Seal
import "math.arr" as Math
import "test.arr" as Test

simpl_results = Simple.run-tests()
seal_results = Seal.run-tests()
math_results = Math.run-tests()

Test.format-results()

