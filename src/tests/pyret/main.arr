#lang pyret

import "simple-tests.arr" as Simple
import "seal-tests.arr" as Seal
import "lib-seal-tests.arr" as LibSeal
import "math.arr" as Math
import "test.arr" as Test

var simpl_results: Simple.run-tests()
var seal_results: Seal.run-tests()
var libseal_results: LibSeal.run-tests()
var math_results: Math.run-tests()

Test.format-results()

