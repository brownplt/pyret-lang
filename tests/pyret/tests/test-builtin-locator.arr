import "compiler/locators/builtin.arr" as B
import "compiler/compile-lib.arr" as CL
import "compiler/compile-structs.arr" as CM
import load-lib as L
import string-dict as SD
import namespace-lib as N
import runtime-lib as R

check:
  sd = B.make-builtin-locator("string-dict")
  sd.get-dependencies() is sets.empty-list-set
  sd.get-provides() is [list-set: "MutableStringDict", "StringDict",
     "make-string-dict", "string-dict",
     "make-mutable-string-dict", "mutable-string-dict"]
  sd.get-compiled() satisfies is-some
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
      get-dependencies(self): CL.get-dependencies(self.get-module(), self.uri()) end,
      get-provides(self): CL.get-provides(self.get-module(), self.uri()) end,
      get-compile-env(self): CM.standard-builtins end,
      get-namespace(self, runtime): N.make-base-namespace(runtime) end,
      uri(self): "protocol://" + name end,
      name(self): name end,
      set-compiled(self, ctxt, provs): nothing end,
      get-compiled(self): none end,
      _equals(self, that, rec-eq): rec-eq(self.uri(), that.uri()) end
    }
  end

  fun dfind(ctxt, dep):
    cases(CM.Dependency) dep:
      | builtin(name) => B.make-builtin-locator(name)
      | dependency(_, args) => string-to-locator(args.get(0))
    end
  end

  clib = CL.make-compile-lib(dfind)

  floc = string-to-locator("foo")
  CL.get-dependencies(floc.get-module(), floc.uri()) is
    [set:
      CM.builtin("string-dict"),
      CM.dependency("protocol", [list: "bar"])]
  wlist = clib.compile-worklist(floc, {})
  wlist.length() is 4
  print(torepr(wlist))
  wlist.get(3).locator is floc
  wlist.get(2).locator.uri() is "pyret-builtin://string-dict"
  wlist.get(1).locator is string-to-locator("bar")
  wlist.get(0).locator.uri() is "pyret-builtin://string-dict"
  wlist.get(0).locator.name() is "string-dict"

  ans = CL.compile-and-run-worklist(clib, wlist, R.make-runtime())
  ans satisfies L.is-success-result
end
