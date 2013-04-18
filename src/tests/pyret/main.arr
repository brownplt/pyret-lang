#lang pyret

import "simple-tests.arr" as Simple
import "seal-tests.arr" as Seal
import "lib-seal-tests.arr" as LibSeal
import "test.arr" as Test

Simple.run-tests()
Seal.run-tests()
LibSeal.run-tests()

Test.format-results()

