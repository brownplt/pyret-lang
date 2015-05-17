import string-dict as SD
import load-lib as L
import namespace-lib as N
import runtime-lib as R
import "compiler/compile-lib.arr" as CL
import "compiler/compile-structs.arr" as CM

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

fun dfind(ctxt, dep): string-to-locator(dep.arguments.get(0)) end

clib = CL.make-compile-lib(dfind)

fun run-to-answer(filename):
  floc = string-to-locator(filename)
  wlist = clib.compile-worklist(floc, {})
  ans = CL.compile-and-run-worklist(clib, wlist, R.make-runtime(), CM.default-compile-options) 
  ans
end
val = lam(str): L.get-result-answer(run-to-answer(str)) end
msg = lam(str): L.render-error-message(run-to-answer(str)) end

check:
  val("foo") is some(52)
  val("includes-a-type") is some(42)
  msg("includes-and-violates") satisfies string-contains(_, "Contract Error")
end
