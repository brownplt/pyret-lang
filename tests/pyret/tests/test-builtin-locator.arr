import "compiler/locators/builtin.arr" as B
import "compiler/compile-lib.arr" as CL
import "compiler/compile-structs.arr" as CM
import load-lib as L
import string-dict as SD
import namespace-lib as N
import runtime-lib as R

check:
  sd = B.make-builtin-locator("string-dict")
  sd.get-dependencies() is [list:]
  sd.get-compiled() satisfies is-some
  sd.get-compiled().value.provides.aliases.keys() is [tree-set: "MutableStringDict", "StringDict"]
  sd.get-compiled().value.provides.values.keys() is [tree-set:
     "make-string-dict", "string-dict", "string-dict-of",
     "make-mutable-string-dict", "mutable-string-dict"]
end

check:
  modules = SD.make-mutable-string-dict()
  modules.set-now("foo",
    ```
    import protocol("bar") as B
    import string-dict as SD

    # make sure string-dicts are the same
    answer :: SD.StringDict = B.g(5)
    ```)
  modules.set-now("bar",
    ```
    provide { g: g } end
    import string-dict as SD

    fun g(x): [SD.string-dict: 'x', x] end
    ```)

  fun string-to-locator(name :: String):
    {
      needs-compile(self, provs): true end,
      get-module(self): CL.pyret-string(modules.get-value-now(name)) end,
      get-extra-imports(self):
        CM.minimal-imports
      end,
      get-dependencies(self):
        CL.get-dependencies(self.get-module(), self.uri())
      end,
      get-globals(self): CM.standard-globals end,
      get-namespace(self, runtime): N.make-base-namespace(runtime) end,
      uri(self): "protocol://" + name end,
      name(self): name end,
      set-compiled(self, ctxt, provs): nothing end,
      get-compiled(self): none end,
      _equals(self, that, rec-eq): rec-eq(self.uri(), that.uri()) end
    }
  end

  fun dfind(ctxt, dep):
    l = cases(CM.Dependency) dep:
      | builtin(name) => B.make-builtin-locator(name)
      | dependency(_, args) => string-to-locator(args.get(0))
    end
    CL.located(l, nothing)
  end

  floc = string-to-locator("foo")
  CL.get-dependencies(floc.get-module(), floc.uri()) is
    [list:
      CM.dependency("protocol", [list: "bar"]),
      CM.builtin("string-dict")]
  wlist = CL.compile-worklist(dfind, floc, {})
  wlist.length() is 4
  wlist.get(3).locator is floc
  wlist.get(2).locator.uri() is "protocol://bar"
  wlist.get(2).locator is string-to-locator("bar")
  wlist.get(1).locator.uri() is "pyret-builtin://string-dict"
  wlist.get(1).locator.name() is "string-dict"
  wlist.get(0).locator.uri() is "pyret-builtin://string-dict"
  wlist.get(0).locator.name() is "string-dict"

  ans = CL.compile-and-run-worklist(wlist, R.make-runtime(), CM.default-compile-options)
  ans.v satisfies L.is-success-result
end
