import file("../../../src/arr/compiler/compile-structs.arr") as CS
import file("../test-compile-helper.arr") as C

fun c(str) block:
  errs = C.get-compile-errs(str)
  when is-empty(errs):
    print-error("Expected at least one error for running \n\n " + str + "\n\n" + " but got none ")
  end
  errs.first
end
fun cok(str):
  C.get-compile-errs(str)
end

check "mixed ops":
  c("true and false or true") satisfies CS.is-mixed-binops
  c("1 + 2 - 3") satisfies CS.is-mixed-binops
  c("1 + 2 + 3 * 4") satisfies CS.is-mixed-binops
  c("1 / 2 + 3 * 4 - 5") satisfies CS.is-mixed-binops
end

check "nullary methods":
  c("method(): nothing end") satisfies CS.is-no-arguments
  c("{method foo(): nothing end}") satisfies CS.is-no-arguments
end

check "multiple statements on a line":
  msg =  "on the same line"
  c("5-2") satisfies CS.is-same-line
  c("'ab''de'") satisfies CS.is-same-line
  c("a\"abc\"") satisfies CS.is-same-line
  c("a=3b=4") satisfies CS.is-same-line
  c("fun f(x) block: f x end") satisfies CS.is-same-line
  c("fun f(x) block: f (x) end") satisfies CS.is-same-line
  cok("fun f(x) block: f\n (x) end\n10") is empty
  cok("fun f(x) block:\n  f\n  # a comment\n  (x)\nend\n10") is empty
end

