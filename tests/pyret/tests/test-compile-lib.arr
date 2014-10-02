import string-dict as SD
import "compiler/compile-lib.arr" as CL
import "compiler/compile-structs.arr" as CM

fun worklist-contains-checker(wlist :: List<CM.ToCompile>):
  locs = wlist.map(_.locator)
  lam(loc :: CL.Locator): locs.member(loc) end
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

  fun string-to-locator(name :: String):
    {
      needs-compile(self, provs): true end,
      get-module(self): modules.get(name) end,
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

  floc = string-to-locator("foo")
  CL.get-dependencies(floc) is [set: CL.dependency("file", [list: "bar"])]
  wlist = clib.compile-worklist(floc, {})
  wlist.length() is 2
  wlist.get(1).locator is floc
  wlist.get(0).locator is string-to-locator("bar")
end

check "Worklist generation (DAG)":
  modules = SD.string-dict()
  modules.set("A",
    ```
    provide { f: f } end
    import file("B") as B
    import file("C") as C

    fun f(x): B.g(x) end
    ```)
  modules.set("B",
    ```
    provide { g: g } end
    import file("D") as D

    fun g(x): D.h(x) end
    ```)
  modules.set("C",
    ```
    provide { g: g } end
    import file("D") as D

    fun g(x): D.h(x) end
    ```)
  modules.set("D",
    ```
    provide { h: h } end

    fun h(x): x end
    ```)

  cresults = SD.string-dict()
  retrievals = SD.string-dict()
  fun string-to-locator(name :: String):
    {
      needs-compile(self, provs): not(cresults.has-key(name)) end,
      get-module(self):
        count = if retrievals.has-key(name): retrievals.get(name) else: 0 end
        retrievals.set(name, count + 1)
        modules.get(name)
      end,
      update-compile-context(self, ctxt): ctxt end,
      uri(self): "file://" + name end,
      name(self): name end,
      set-compiled(self, cr, provs): cresults.set(name, cr) end,
      get-compiled(self): if cresults.has-key(name): some(cresults.get(name)) else: none end end,
      _equals(self, that, rec-eq): rec-eq(self.uri(), that.uri()) end
    }
  end

  fun dfind(ctxt, dep): string-to-locator(dep.arguments.get(0)) end

  clib = CL.make-compile-lib(dfind)

  floc = string-to-locator("A")
  CL.get-dependencies(floc) is [set: CL.dependency("file", [list: "B"]), CL.dependency("file", [list: "C"])]

  wlist = clib.compile-worklist(floc, {})

  # Don't want to be too specific, just make sure they're in the checklist at least once.
  wlist-checker = worklist-contains-checker(wlist)
  string-to-locator("A") satisfies wlist-checker
  string-to-locator("B") satisfies wlist-checker
  string-to-locator("C") satisfies wlist-checker
  string-to-locator("D") satisfies wlist-checker

  # Reset retrievals, because get-provides calls get-module every time currently.
  # This way, we can make sure that we only get one retrieval during actual compilation of each module.
  for each(s from retrievals.keys()): retrievals.set(s, 0) end

  results = clib.compile-program(wlist)

  # Are we respecting needs-compile?
  retrievals.get("A") is 1
  retrievals.get("B") is 1
  retrievals.get("C") is 1
  retrievals.get("D") is 1
end
