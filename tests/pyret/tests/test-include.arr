import string-dict as SD
import load-lib as L
import namespace-lib as N
import runtime-lib as R
import "compiler/compile-lib.arr" as CL
import "compiler/compile-structs.arr" as CM

fun string-to-locator(name :: String):
  {
    needs-compile(self, provs): true end,
    get-module(self): CL.pyret-string(modules.get-value-now(name)) end,
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

check:
  modules = [SD.mutable-string-dict:
    "foo",
    ```
    include file("bar")

    fun f(x): g(x) + y end
    f(42)
    ```,

    "bar",
    ```
    provide { g: g, y: 10 } end

    fun g(x): x end
    ```]

    floc = string-to-locator("foo")

    wlist = CL.compile-and-run-worklist(clib, wlist, R.make-runtime(), CM.default-compile-options) 
    ans satisfies L.is-success-result
    L.get-result-answer(ans) is 52
end
