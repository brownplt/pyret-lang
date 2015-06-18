import string-dict as SD
import load-lib as L
import namespace-lib as N
import runtime-lib as R
import either as E
import render-error-display as ED
import "compiler/compile-lib.arr" as CL
import "compiler/compile-structs.arr" as CM
import "compiler/locators/builtin.arr" as BL

type Either = E.Either

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
    get-module(self): CL.pyret-string(modules.get-value-now(name)) end,
    get-extra-imports(self): CM.minimal-imports end,
    get-dependencies(self): CL.get-dependencies(self.get-module(), self.uri()) end,
    get-provides(self): CL.get-provides(self.get-module(), self.uri()) end,
    get-globals(self): CM.standard-globals end,
    get-namespace(self, runtime): N.make-base-namespace(runtime) end,
    uri(self): "file://" + name end,
    name(self): name end,
    set-compiled(self, ctxt, provs): nothing end,
    get-compiled(self): none end,
    _equals(self, that, rec-eq): rec-eq(self.uri(), that.uri()) end
  }
end

fun dfind(ctxt, dep):
  l = cases(CM.Dependency) dep:
    | dependency(_, _) => string-to-locator(dep.arguments.get(0))
    | builtin(modname) =>
      BL.make-builtin-locator(modname)
  end
  CL.located(l, ctxt)
end

fun run-to-result(filename):
  floc = string-to-locator(filename)
  wlist = CL.compile-worklist(dfind, floc, {})
  res = CL.compile-and-run-worklist(wlist, R.make-runtime(), CM.default-compile-options) 
  res
end

fun get-run-answer(str):
  cases(Either) run-to-result(str):
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
  msg("includes-and-violates") satisfies string-contains(_, "Number")
  val("type-and-val") is some(12)
  cmsg("overlapping-import") satisfies string-contains(_, "defined")
  cmsg("global-shadow-import") satisfies string-contains(_, "defined")
#  cmsg("global-type-shadow-import") satisfies string-contains(_, "defined")

  val("include-world") is some(true)
  val("gather-includes-include") is some(true)
end
