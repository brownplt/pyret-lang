import string-dict as SD
import srcloc as SL
import load-lib as L
import runtime-lib as R
import ast as A
import builtin-modules as BM
import file("../../../src/arr/compiler/type-structs.arr") as T
import file("../../../src/arr/compiler/ast-util.arr") as AU
import file("../../../src/arr/compiler/compile-lib.arr") as CL
import file("../../../src/arr/compiler/cli-module-loader.arr") as CLI
import file("../../../src/arr/compiler/compile-structs.arr") as CM
import file("../../../src/arr/compiler/locators/builtin.arr") as BL

print("Running compile-lib tests: " + tostring(time-now()) + "\n")

fun worklist-contains-checker(wlist :: List<CM.ToCompile>):
  locs = wlist.map(_.locator)
  lam(loc :: CL.Locator): locs.member(loc) end
end


check "Worklist generation (simple)":
  modules = SD.make-mutable-string-dict()
  modules.set-now("foo",
    ```
    provide { f: f } end
    import file("bar") as B

    fun f(x): B.g(x) end
    f(42)
    ```)
  modules.set-now("bar",
    ```
    provide { g: g } end

    fun g(x): x end
    ```)

  fun string-to-locator(name :: String):
    {
      method needs-compile(self, provs): true end,
      method get-module(self): CL.pyret-string(modules.get-value-now(name)) end,
      method get-extra-imports(self): CM.minimal-imports end,
      method get-modified-time(self): 0 end,
      method get-options(self, options): options end,
      method get-native-modules(self): empty end,
      method get-dependencies(self): CL.get-dependencies(self.get-module(), self.uri()) end,
      method get-globals(self): CM.standard-globals end,
      method uri(self): "file://" + name end,
      method name(self): name end,
      method set-compiled(self, ctxt, provs): nothing end,
      method get-compiled(self): none end,
      method _equals(self, that, rec-eq): rec-eq(self.uri(), that.uri()) end
    }
  end

  fun dfind(ctxt, dep): CL.located(string-to-locator(dep.arguments.get(0)), nothing) end

  floc = string-to-locator("foo")
  CL.get-dependencies(floc.get-module(), floc.uri()) is [list: CM.dependency("file", [list: "bar"])]
  wlist = CL.compile-worklist(dfind, floc, {})
  wlist.length() is 2
  wlist.get(1).locator is floc
  wlist.get(0).locator is string-to-locator("bar")

  #ans = CL.compile-and-run-worklist(wlist, R.make-runtime(), CM.default-compile-options)
  #ans.v satisfies L.is-success-result
end

check "Worklist generation (DAG)":
  modules = SD.make-mutable-string-dict()
  modules.set-now("A",
    ```
    provide { f: f } end
    import file("B") as B
    import file("C") as C

    fun f(x): B.g(x) end
    ```)
  modules.set-now("B",
    ```
    provide { g: g } end
    import file("D") as D

    fun g(x): D.h(x) end
    ```)
  modules.set-now("C",
    ```
    provide { g: g } end
    import file("D") as D

    fun g(x): D.h(x) end
    ```)
  modules.set-now("D",
    ```
    provide { h: h } end

    fun h(x): x end
    ```)

  cresults = SD.make-mutable-string-dict()
  retrievals = SD.make-mutable-string-dict()
  fun string-to-locator(name :: String):
    {
      method needs-compile(self, provs): not(cresults.has-key-now(name)) end,
      method get-module(self) block:
        count = if retrievals.has-key-now(name): retrievals.get-value-now(name) else: 0 end
        retrievals.set-now(name, count + 1)
        CL.pyret-string(modules.get-value-now(name))
      end,
      method get-modified-time(self): 0 end,
      method get-options(self, options): options end,
      method get-native-modules(self): empty end,
      method get-extra-imports(self): CM.minimal-imports end,
      method get-dependencies(self): CL.get-dependencies(CL.pyret-string(modules.get-value-now(name)), self.uri()) end,
      method get-globals(self): CM.no-builtins.globals end,
      method uri(self): "file://" + name end,
      method name(self): name end,
      method set-compiled(self, cr, provs): cresults.set-now(name, cr) end,
      method get-compiled(self): if cresults.has-key-now(name): some(cresults.get-value-now(name)) else: none end end,
      method _equals(self, that, rec-eq): rec-eq(self.uri(), that.uri()) end
    }
  end

  fun dfind(ctxt, dep): CL.located(string-to-locator(dep.arguments.get(0)), nothing) end

  floc = string-to-locator("A")
  CL.get-dependencies(floc.get-module(), floc.uri()) is [list: CM.dependency("file", [list: "B"]), CM.dependency("file", [list: "C"])]

  wlist = CL.compile-worklist(dfind, floc, {})

  # Don't want to be too specific, just make sure they're in the checklist at least once.
  wlist-checker = worklist-contains-checker(wlist)
  string-to-locator("A") satisfies wlist-checker
  string-to-locator("B") satisfies wlist-checker
  string-to-locator("C") satisfies wlist-checker
  string-to-locator("D") satisfies wlist-checker

  # Reset retrievals, because get-provides calls get-module every time currently.
  # This way, we can make sure that we only get one retrieval during actual compilation of each module.
  for each(s from retrievals.keys-now().to-list()): retrievals.set-now(s, 0) end

  results = CL.compile-program(wlist, CM.default-compile-options)

  # Are we respecting needs-compile?
  retrievals.get-value-now("A") is 1
  retrievals.get-value-now("B") is 1
  retrievals.get-value-now("C") is 1
  retrievals.get-value-now("D") is 1
end

check "Worklist generation (Cycle)":
  modules = SD.make-mutable-string-dict()
  modules.set-now("A",
    ```
    provide { f: f } end
    import file("B") as B

    fun f(x): B.g(x) end
    ```)
  modules.set-now("B",
    ```
    provide { g: g } end
    import file("A") as A

    fun g(x): A.f(x) end
    ```)

  fun string-to-locator(name :: String):
    {
      method needs-compile(self, provs): true end,
      method get-module(self): CL.pyret-string(modules.get-value-now(name)) end,
      method get-extra-imports(self): CM.minimal-imports end,
      method get-modified-time(self): 0 end,
      method get-options(self, options): options end,
      method get-native-modules(self): empty end,
      method get-dependencies(self): CL.get-dependencies(self.get-module(), self.uri()) end,
      method get-globals(self): CM.standard-globals end,
      method uri(self): "file://" + name end,
      method name(self): name end,
      method set-compiled(self, ctxt, provs): nothing end,
      method get-compiled(self): none end,
      method _equals(self, that, rec-eq): rec-eq(self.uri(), that.uri()) end
    }
  end

  fun dfind(ctxt, dep): CL.located(string-to-locator(dep.arguments.get(0)), nothing) end

  floc = string-to-locator("A")
  CL.get-dependencies(floc.get-module(), floc.uri()) is [list: CM.dependency("file", [list: "B"])]
  gloc = string-to-locator("B")
  CL.get-dependencies(gloc.get-module(), gloc.uri()) is [list: CM.dependency("file", [list: "A"])]

  CL.compile-worklist(dfind, floc, {}) raises "cycle"
end


check "Multiple includes":
  modules = SD.make-mutable-string-dict()
  modules.set-now("A",
    ```
    provide *
    provide-types *

    data D:
      | d(x)
    end
    ```)
  modules.set-now("B",
    ```
    provide *
    import some-protocol("A") as A
    fun f():
      A.d(4)
    end
    ```)
  modules.set-now("C",
    ```
    import some-protocol("A") as A
    import some-protocol("B") as B1
    import some-protocol("B") as B2
    import some-protocol("B") as B3

    torepr([list:
      A.d(4) == B1.f(),
      B1.f() == B2.f(),
      B2.f() == B3.f()
    ])
    ```)

  fun string-to-locator(name :: String):
    {
      method needs-compile(self, provs): true end,
      method get-module(self): CL.pyret-string(modules.get-value-now(name)) end,
      method get-extra-imports(self): CM.standard-imports end,
      method get-modified-time(self): 0 end,
      method get-options(self, options): options end,
      method get-dependencies(self): CL.get-standard-dependencies(self.get-module(), self.uri()) end,
      method get-globals(self): CM.standard-globals end,
      method get-native-modules(self): empty end,
      method uri(self): "file://" + name end,
      method name(self): name end,
      method set-compiled(self, ctxt, provs): nothing end,
      method get-compiled(self): none end,
      method _equals(self, that, rec-eq): rec-eq(self.uri(), that.uri()) end
    }
  end

  fun dfind(ctxt, dep):
    cases(CM.Dependency) dep:
      | builtin(modname) =>
        CLI.module-finder(ctxt, dep)
      | else =>
        CL.located(string-to-locator(dep.arguments.get(0)), ctxt)
    end
  end

  start-loc = string-to-locator("C")
  wlist = CL.compile-worklist(dfind, start-loc, CLI.default-start-context)
  ans = CL.compile-and-run-locator(start-loc, dfind, CLI.default-test-context, L.empty-realm(), R.make-runtime(), [SD.mutable-string-dict:], CM.default-compile-options)

  ans.v satisfies L.is-success-result
  L.get-result-answer(ans.v) is some("[list: true, true, true]")
end

mt = [SD.string-dict:]
string-dict = SD.string-dict

check "raw-provide-syntax":
  mod = ```
  ({
    requires: [ ],
    provides: {
      shorthands: {
        "num-pred": ["arrow", ["Number", "Number"], "Boolean"],
        "I": ["local", "Ither"]
      },
      values: {
        "string-to-num": ["arrow", ["String"], ["Option", "Number"]],
        "num-greater": "num-pred"
      },
      datatypes: {
        "Ither": ["data", "Ither", ["a", "b"],
          [["left", [["value", ["tid", "a"]]]],
           ["right", [["value", ["tid", "b"]]]]],
          {
            "join-left": ["arrow", [["tid", "a"]], ["tyapp", "I", [["tid", "a"], ["tid", "b"]]]]
          }
        ]
      }
    },
    nativeRequires: [ ],
    theModule: function() {}
  })
  ```
  raw = BM.builtin-raw-locator-from-str(mod)

  bnr = lam(modname, name):
    {
      tag: "name",
      origin: {
        import-type: "uri",
        uri: "builtin://" + modname,
      },
      name: name
    }
  end
  gr = lam(name):
    bnr("global", name)
  end

  raw.get-raw-value-provides() is=~
    [raw-array:
      {
        name: "string-to-num",
        value: {
          bind: "let",
          typ: {
            tag: "arrow",
            args: [list: gr("String")],
            ret: {
              tag: "tyapp",
              onto: bnr("option", "Option"),
              args: [list: gr("Number")]
            }
          }
        }
      },
      {
        name: "num-greater",
        value: {
          bind: "let",
          typ: {
            tag: "arrow",
            args: [list: gr("Number"), gr("Number")],
            ret: gr("Boolean")
          }
        }
      }]

  ta = {
    tag: "tyvar",
    name: "a"
  }
  tb = {
    tag: "tyvar",
    name: "b"
  }

  raw.get-raw-datatype-provides() is=~
    [raw-array:
      {
        name: "Ither",
        typ: {
          tag: "data",
          name: "Ither",
          params: [list: "a", "b"],
          variants: [list:
            {
              tag: "variant",
              name: "left",
              vmembers: [list: {
                tag: "variant-member",
                name: "value",
                kind: "normal",
                typ: ta
              }]
            },
            {
              tag: "variant",
              name: "right",
              vmembers: [list: {
                tag: "variant-member",
                name: "value",
                kind: "normal",
                typ: tb
              }]
            }
          ],
          methods: [list: {
            name: "join-left",
            value: {
              tag: "arrow",
              args: [list: ta],
              ret: {
                tag: "tyapp",
                onto: {
                  tag: "name",
                  origin: { import-type: "$ELF" },
                  name: "Ither"
                },
                args: [list: ta, tb]
              }
            }
          }]
        }
      }
    ]

  provs = CM.provides-from-raw-provides("test-raw-provides", {
    uri: "test-raw-provides",
    modules: raw-array-to-list(raw.get-raw-module-provides()),
    values: raw-array-to-list(raw.get-raw-value-provides()),
    aliases: raw-array-to-list(raw.get-raw-alias-provides()),
    datatypes: raw-array-to-list(raw.get-raw-datatype-provides())
  })

  l = SL.builtin("test-raw-provides")

  bn = lam(modname, name):
    T.t-name(T.module-uri("builtin://" + modname), A.s-type-global(name), l, false)
  end
  g = lam(name):
    bn("global", name)
  end

  provs.values is
    [string-dict:
      "string-to-num",
      CM.v-just-type(T.t-arrow(
        [list: g("String")],
        T.t-app(
          bn("option", "Option"),
          [list: g("Number")],
          l, false),
        l, false)),
      "num-greater",
      CM.v-just-type(T.t-arrow(
        [list: g("Number"), g("Number")],
        g("Boolean"),
        l, false))
    ]

  #NOTE(joe): tough to test the case for Ither datatype because of generativity
  # in foralls

end

check:
  ps = CM.provides("test-provides1",
    # MARK(joe/ben): modules
    mt,
    [string-dict:
      "x", CM.v-just-type(T.t-name(T.dependency("builtin(global)"), A.s-global("Number"), A.dummy-loc, false))
    ],
    mt,
    mt)

  ce = CM.compile-env(CM.globals(mt, mt, mt),
    [SD.mutable-string-dict:
      "builtin://global",
      CM.module-as-string(
          # MARK(joe/ben): modules
          CM.provides("builtin://global", mt, mt, mt,
            [SD.string-dict:
              "Number", T.t-data("Number", empty, empty, SD.make-string-dict(), A.dummy-loc)]),
          CM.no-builtins,
          CM.computed-none,
          CM.ok("dummy")
        )
      ],
    [string-dict:
      "builtin(global)", "builtin://global"])

  canon = AU.canonicalize-provides(ps, ce)

  canon is CM.provides("test-provides1",
    mt, #MARK(joe/ben): modules
    [string-dict:
      "x", CM.v-just-type(T.t-name(T.module-uri("builtin://global"), A.s-global("Number"), A.dummy-loc, false))
    ],
    mt,
    mt)

  local = AU.localize-provides(canon, ce)

  local is ps
  print("Done running compile-lib tests: " + tostring(time-now()) + "\n")

end
