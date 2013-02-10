#lang pyret

import "simple-tests.arr" as Simple
import "seal-tests.arr" as Seal
import "unittest.arr" as Test

Simple.run-tests()
Seal.run-tests()

Test.format-results()
""
