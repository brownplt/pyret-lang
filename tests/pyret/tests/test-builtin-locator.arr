import file("../../../src/arr/compiler/locators/builtin.arr") as B
import file("../../../src/arr/compiler/compile-lib.arr") as CL
import file("../../../src/arr/compiler/compile-structs.arr") as CM
import load-lib as L
import string-dict as SD
import namespace-lib as N
import runtime-lib as R

check:
  sd = B.make-builtin-locator("string-dict")
  sd.get-dependencies() is [list: CM.builtin("valueskeleton")]
  sd.get-compiled() satisfies is-some

  sd.get-compiled().value.provides.data-definitions.keys() is [tree-set:]
  sd.get-compiled().value.provides.values.keys() is [tree-set:]

  # TODO(joe): reinstate these once types come back
  # sd.get-compiled().value.provides.data-definitions.keys() is [tree-set: "MutableStringDict", "StringDict"]
  # sd.get-compiled().value.provides.values.keys() is [tree-set:
  #   "make-string-dict", "string-dict", "string-dict-of",
  #   "make-mutable-string-dict", "mutable-string-dict"]
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
  modules.set-now("baz",
    ```
    import string-dict as SD
    include image

    is-function(image-url)
    ```)

  fun string-to-locator(name :: String):
    CL.string-locator("protocol://" + name, modules.get-value-now(name)).{
      get-extra-imports(self): CM.minimal-imports end
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
  wlist.length() is 19
  wlist.get(0).locator.uri() is "builtin://valueskeleton"
  wlist.get(0).locator.name() is "valueskeleton"
  wlist.get(1).locator.uri() is "builtin://string-dict"
  wlist.get(1).locator.name() is "string-dict"
  wlist.get(17).locator.uri() is "protocol://bar"
  wlist.get(17).locator is string-to-locator("bar")
  wlist.get(18).locator is floc

  # TODO(joe): this requires some better eval support than we have
  # ans = CL.compile-and-run-worklist(wlist, R.make-runtime(), CM.default-compile-options)
  # ans.v satisfies L.is-success-result

  bazloc = string-to-locator("baz")
  wlist2 = CL.compile-worklist(dfind, bazloc, {})
  wlist2.length() is 21
  # TODO(joe): this requires some better eval support than we have
  # ans2 = CL.compile-and-run-worklist(wlist, R.make-runtime(), CM.default-compile-options)
  # ans2.v satisfies L.is-success-result
end
