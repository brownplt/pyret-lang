import string-dict as SD
import "compiler/compile-lib.arr" as CL

fun string-to-locator(modules :: SD.StringDict<String>, name :: String):
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

fun module-finder(modules :: SD.StringDict<String>):
  fun dfind(ctxt, dep): string-to-locator(modules, dep.arguments.get(0)) end
  dfind
end

check "Worklist generation (simple)":
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

  dfind = module-finder(modules)
  clib = CL.make-compile-lib(dfind)

  floc = string-to-locator(modules, "foo")
  CL.get-dependencies(floc) is [set: CL.dependency("file", [list: "bar"])]
  wlist = clib.compile-worklist(floc, {})
  wlist.length() is 2
  wlist.get(1).locator is floc
  wlist.get(0).locator is string-to-locator(modules, "bar")
end
