import string-dict as SD
import "compiler/compile-lib.arr" as CL

modules = SD.string-dict()
modules.set("foo",
```
provide { f: f } end
import file("bar") as B

fun f(x): B.g(x) end
```)
modules.set("bar",
```
provide { g: g } end

fun g(x): x end
```)

fun string-to-locator(name :: String):
  file = modules.get(name)
  {
    needs-compile(self, provs): true end,
    get-module(self): file end,
    update-compile-context(self, ctxt): ctxt end,
    uri(self): "file://" + name end,
    name(self): name end,
    set-compiled(self, ctxt, provs): nothing end,
    get-compiled(self): none end,
    _equals(self, that, rec-eq): rec-eq(self.uri(), that.uri()) end
  }
end

fun dfind(ctxt, dep): string-to-locator(dep.arguments.get(0)) end

clib = CL.make-compile-lib(dfind)

check "Worklist generation":
  floc = string-to-locator("foo")
  CL.get-dependencies(floc) is [set: CL.dependency("file", [list: "bar"])]
  wlist = clib.compile-worklist(floc, {})
  wlist.length() is 2
  wlist.get(1).locator is floc
  wlist.get(0).locator is string-to-locator("bar")
end
