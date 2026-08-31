# The compiler-testing half of what used to be main2.arr: every module here
# imports src/arr/compiler/* (directly or via test-compile-helper) and
# invokes the compiler AT TEST RUNTIME, so this suite:
#   * needs a warm module cache in tests/compiled/ (test-compile-helper and
#     CLI.default-test-context resolve it) -- see the cache-warm rules in
#     the Makefile;
#   * always exercises the .arr compiler's logic, no matter which compiler
#     BUILT it. Building this file with the TS compiler tests TS codegen of
#     the .arr compiler, nothing more; the TS implementations are covered
#     by ts-parity-test / ts-type-check-parity / ts-wf-parity / ts-repl-test.
# main2.arr holds the language/runtime suite, which is free of both
# properties. Import order preserved from the original main2.arr.
import file("./tests/test-error-rendering.arr") as _
import file("./tests/test-contracts.arr") as _
import file("./tests/test-rec.arr") as _
import file("./tests/test-compile-errors.arr") as _
import file("./tests/test-well-formed.arr") as _
import file("./tests/test-repl.arr") as _
import file("./tests/test-file-locators.arr") as _
import file("./tests/test-builtin-locator.arr") as _
import file("./tests/test-compile-lib.arr") as _
import file("./tests/test-include.arr") as _
import file("./tests/test-parse-errors.arr") as _
import file("./tests/test-flatness.arr") as _
import file("./tests/test-modules.arr") as _
