import string-dict as SD

modules = SD.string-dict()
modules.set("foo",
```
import file("bar") as B
provide { f: f } end

fun f(x): B.g(x) end
```)
modules.set("bar",
```
provide { g: g } end

fun g(x): x end
```)

fun string-to-locator(dep :: String):
  file = modules[dep]
  {
    needs-compiles(self, provs): true end
    get-module(self): file end
    update-compile-context(self, ctxt): ctxt end
    uri(self): "file://" + dep end
    name(self): dep end
    set-compiled(self, ctxt, provs): nothing end
    get-compiled(self): none end
    _equals(self, that, rec-eq): self.uri() == that.uri() end
  }
end

fun find(ctxt :: CompileContext, dep :: Dependency):
  string-to-locator(dep)
end

clib = make-compile-lib(find)

check "Worklist generation":
  floc = string-to-locator("foo")
  wlist = clib.compile-worklist(floc, {})
  wlist.length() is 2
  wlist.get(1).locator is floc
  wlist.get(0).locator is string-to-locator("bar")
end
