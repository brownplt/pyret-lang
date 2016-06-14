import string-dict as SD
import load-lib as L
import namespace-lib as N
import runtime-lib as R
import either as E
import render-error-display as ED
import file("../../../src/arr/compiler/compile-lib.arr") as CL
import file("../../../src/arr/compiler/cli-module-loader.arr") as CLI
import file("../../../src/arr/compiler/compile-structs.arr") as CM
import file("../../../src/arr/compiler/locators/builtin.arr") as BL

type Either = E.Either

print("Running include tests: " + tostring(time-now()) + "\n")


modules = [SD.mutable-string-dict:
  "foo",
  ```
  include file("bar")

  fun f(x): g(x) + y end
  f(42)
  ```,

  "bar",
  ```
  provide *

  y = 10
  fun g(x): x end
  ```,

  "provides-a-type",
  ```
  provide-types *

  type N = Number
  ```,

  "includes-a-type",
  ```
  include file("provides-a-type")

  x :: N = 42
  x
  ```,

  "includes-and-violates",
  ```
  include file("provides-a-type")

  x :: N = "not-a-num"
  x
  ```,

  "same-type-and-val-name",
  ```
  provide *
  provide-types *

  type n = Number
  n = 12
  ```,

  "type-and-val",
  ```
  include file("same-type-and-val-name")

  x :: n = n
  x
  ```,

  "overlapping-def1",
  ```
  provide *
  n = 10
  ```,

  "overlapping-def2",
  ```
  provide *
  n = 15
  ```,

  "overlapping-import",
  ```
  include file("overlapping-def1")
  include file("overlapping-def2")
  n
  ```,


  "shadows-a-global",
  ```
  provide *
  shadow tostring = "not-tostring"
  ```,

  "global-shadow-import",
  ```
  include file("shadows-a-global")
  ```,


#|
  "shadows-global-type",
  ```
  provide-types { Number :: Boolean }
  ```,

  "global-type-shadow-import",
  ```
  include file("shadows-global-type")
  ```,
|#

  "include-world",
  ```
  include world
  is-function(big-bang)
  ```,


  "gather-includes",
  ```
  provide *
  include world
  include image
  # Should re-exporting be required?
  shadow big-bang = big-bang
  shadow rectangle = rectangle
  ```,

  "gather-includes-include",
  ```
  include file("gather-includes")
  is-function(rectangle) and is-function(big-bang)
  ```

  ]


fun string-to-locator(name :: String):
  {
    needs-compile(self, provs): true end,
    get-modified-time(self): 0 end,
    get-options(self, options): options end,
    get-module(self): CL.pyret-string(modules.get-value-now(name)) end,
    get-native-modules(self): [list:] end,
    get-extra-imports(self): CM.standard-imports end,
    get-dependencies(self): CL.get-standard-dependencies(self.get-module(), self.uri()) end,
    get-globals(self): CM.standard-globals end,
    get-namespace(self, runtime): N.make-base-namespace(runtime) end,
    uri(self): "file://" + name end,
    name(self): name end,
    set-compiled(self, _, _): nothing end,
    get-compiled(self): none end,
    _equals(self, that, rec-eq): rec-eq(self.uri(), that.uri()) end
  }
end

fun dfind(ctxt, dep):
  cases(CM.Dependency) dep:
    | builtin(modname) =>
      CLI.module-finder(ctxt, dep)
    | else =>
      CL.located(string-to-locator(dep.arguments.get(0)), ctxt)
  end
end

fun run-to-result(filename):
  floc = string-to-locator(filename)
  res = CL.compile-and-run-locator(floc, dfind, CLI.default-test-context, L.empty-realm(), R.make-runtime(), [SD.mutable-string-dict:], CM.default-compile-options.{compile-module: true})
  res
end

fun get-run-answer(str):
  cases(Either) run-to-result(str) block:
    | right(ans) => ans
    | left(err) =>
      print-error("Expected an answer, but got compilation errors:")
      for lists.each(e from err):
        print-error(tostring(e))
      end
  end
end

fun get-compile-errs(str):
  cases(Either) run-to-result(str):
    | right(ans) =>
      print-error("Expected compilation errors for " + str + " but got an answer: " + tostring(L.get-result-answer(ans)))
    | left(errs) => errs.map(_.problems).foldr(_ + _, empty)
  end
end
val = lam(str): L.get-result-answer(get-run-answer(str)) end
msg = lam(str): L.render-error-message(get-run-answer(str)) end
cmsg = lam(str):
  get-compile-errs(str).map(lam(err):
    ED.display-to-string(err.render-reason(), torepr, empty)
  end).join-str(" ")
end

check:
  val("foo") is some(52)
  val("includes-a-type") is some(42)
# TODO(joe): this should produce a good string rendering containing "Number",
# but need to fix the renderErrorMessage call in load-lib first
  msg("includes-and-violates") satisfies string-contains(_, "Number")
#  msg("includes-and-violates") satisfies string-contains(_, "error message")
  val("type-and-val") is some(12)
  cmsg("overlapping-import") satisfies string-contains(_, "shadows")
  cmsg("global-shadow-import") satisfies string-contains(_, "shadows")
#  cmsg("global-type-shadow-import") satisfies string-contains(_, "defined")

  # TODO(joe): Fix these by writing out the types of exports
  #val("include-world") is some(true)
  #val("gather-includes-include") is some(true)
  print("Done running include tests: " + tostring(time-now()) + "\n")

end
