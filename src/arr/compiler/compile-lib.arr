provide *
provide-types *

import either as E
import parse-pyret as P
import ast as A
import load-lib as L
import namespace-lib as N
import render-error-display as RED
import runtime-lib as R
import sets as S
import string-dict as SD
import file("compile-structs.arr") as CS
import file("concat-lists.arr") as C
import file("gensym.arr") as G
import file("js-of-pyret.arr") as JSP
import file("js-ast.arr") as J
import file("ast-util.arr") as AU
import file("well-formed.arr") as W
import file("desugar.arr") as D
import file("desugar-post-tc.arr") as DP
import file("type-check.arr") as T
import file("desugar-check.arr") as CH
import file("resolve-scope.arr") as RS

data CompilationPhase:
  | start
  | phase(name :: String, result :: Any, prev :: CompilationPhase)
sharing:
  tolist(self):
    fun help(the-phase, acc):
      if is-start(the-phase): acc
      else: help(the-phase.prev, {name : the-phase.name, result : the-phase.result} ^ link(_, acc))
      end
    end
    help(self, empty)
  end
end

j-fun = J.j-fun
j-var = J.j-var
j-id = J.j-id
j-method = J.j-method
j-block = J.j-block
j-true = J.j-true
j-false = J.j-false
j-num = J.j-num
j-str = J.j-str
j-return = J.j-return
j-assign = J.j-assign
j-if = J.j-if
j-if1 = J.j-if1
j-new = J.j-new
j-app = J.j-app
j-list = J.j-list
j-obj = J.j-obj
j-dot = J.j-dot
j-bracket = J.j-bracket
j-field = J.j-field
j-dot-assign = J.j-dot-assign
j-bracket-assign = J.j-bracket-assign
j-try-catch = J.j-try-catch
j-throw = J.j-throw
j-expr = J.j-expr
j-binop = J.j-binop
j-and = J.j-and
j-lt = J.j-lt
j-eq = J.j-eq
j-neq = J.j-neq
j-geq = J.j-geq
j-unop = J.j-unop
j-decr = J.j-decr
j-incr = J.j-incr
j-not = J.j-not
j-instanceof = J.j-instanceof
j-ternary = J.j-ternary
j-null = J.j-null
j-parens = J.j-parens
j-switch = J.j-switch
j-case = J.j-case
j-default = J.j-default
j-label = J.j-label
j-break = J.j-break
j-while = J.j-while
j-for = J.j-for


left = E.left
right = E.right
type Either = E.Either

mtd = [SD.string-dict:]

# for re-export
standard-builtins = CS.standard-builtins
make-base-namespace = N.make-base-namespace

type URI = String

data PyretCode:
  | pyret-string(s :: String)
  | pyret-ast(ast :: A.Program)
end

data Loadable:
  | module-as-string(provides :: CS.Provides, compile-env :: CS.CompileEnvironment, result-printer)
end

type ModuleResult = Any

type Provides = CS.Provides

type Locator = {

  # In milliseconds-since-epoch format
  get-modified-time :: (-> Number),

  get-options :: (CS.CompileOptions -> CS.CompileOptions),

  # Could either have needs-provide be implicitly stateful, and cache
  # the most recent map, or use explicit interface below
  needs-compile :: (SD.StringDict<Provides> -> Boolean),

  # Pre-compile (skippable if get-compile returns something)
  get-module :: ( -> PyretCode),

  # Pre-compile (had better be known with no other help)
  get-dependencies :: ( -> List<CS.Dependency>),

  # Pre-compile (to find list of requirejs dependencies to include)
  get-native-modules :: ( -> List<CS.NativeModule>),

  # Pre-compile
  get-extra-imports :: ( -> CS.ExtraImports),

  # Post- or pre- compile
  # get-import-names :: ( -> List<String>),

  # Pre-compile, specification of available globals
  get-globals :: ( -> CS.Globals),

  uri :: (-> URI),
  name :: (-> String),

  set-compiled :: (Loadable, SD.StringDict<Provides> -> Nothing),

  # Pre-compile if needs-compile is false
  get-compiled :: ( -> Option<CS.CompileResult>),

  # _equals should compare uris for locators
  _equals :: Method
}



fun string-locator(uri :: URI, s :: String):
  {
    needs-compile(self, _): true end,
    get-modified-time(self): 0 end,
    get-options(self, options): options end,
    get-module(self): pyret-string(s) end,
    get-native-modules(self): [list:] end,
    get-dependencies(self): get-standard-dependencies(pyret-string(s), uri) end,
    get-extra-imports(self): CS.standard-imports end,
    get-globals(self): CS.standard-globals end,
    get-namespace(self, r): N.make-base-namespace(r) end,
    uri(self): uri end,
    name(self): uri end,
    set-compiled(self, _, _): nothing end,
    get-compiled(self): none end,
    _equals(self, other, rec-eq): rec-eq(other.uri(), self.uri()) end
  }
end

data Located<a>:
  | located(locator :: Locator, context :: a)
end

fun get-ast(p :: PyretCode, uri :: URI):
  cases(PyretCode) p:
    | pyret-string(s) => P.surface-parse(s, uri)
    | pyret-ast(a) => a
  end
end

fun get-import-type(i):
  cases(A.Import) i:
    | s-import(_, f, _) => f
    | s-import-types(_, f, _, _) => f
    | s-include(_, f) => f
    | s-import-complete(_, _, _, f, _, _) => f
    | s-import-fields(_, _, f) => f
  end
end

fun get-dependencies(p :: PyretCode, uri :: URI) -> List<CS.Dependency>:
  parsed = get-ast(p, uri)
  for map(s from parsed.imports.map(get-import-type)):
    AU.import-to-dep(s)
  end
end

fun get-standard-dependencies(p :: PyretCode, uri :: URI) -> List<CS.Dependency>:
  mod-deps = get-dependencies(p, uri)
  mod-deps + CS.standard-imports.imports.map(_.dependency)
end

fun const-dict<a>(strs :: List<String>, val :: a) -> SD.StringDict<a>:
  for fold(d from mtd, s from strs):
    d.set(s, val)
  end
end

fun get-provides(p :: PyretCode, uri :: URI) -> Provides:
  parsed = get-ast(p, uri)
  vals-part =
    cases (A.Provide) parsed._provide:
      | s-provide-none(l) => mtd
      | s-provide-all(l) =>
        const-dict(A.toplevel-ids(parsed).map(_.toname()), CS.v-just-there)
      | s-provide(l, e) =>
        cases (A.Expr) e:
          | s-obj(_, mlist) => const-dict(mlist.map(_.name), CS.v-just-there)
          | else => raise("Non-object expression in provide: " + l.format(true))
        end
    end
  types-part =
    cases(A.ProvideTypes) parsed.provided-types:
      | s-provide-types-none(l) => mtd
      | s-provide-types-all(l) =>
        type-ids = A.block-type-ids(parsed.block)
        type-strs = type-ids.map(lam(i): i.name.toname() end)
        const-dict(type-strs, CS.t-just-there)
      | s-provide-types(l, anns) =>
        const-dict(anns.map(_.name), CS.t-just-there)
    end
  CS.provides(vals-part, types-part)
end

type ToCompile = { locator :: Locator, dependency-map :: SD.MutableStringDict<Locator> }

fun dict-map<a, b>(sd :: SD.MutableStringDict, f :: (String, a -> b)):
  for fold(sd2 from mtd, k from sd.keys-now().to-list()):
    sd2.set(k, f(k, sd.get-value-now(k)))
  end
end

dummy-provides = lam(uri): CS.provides(uri, SD.make-string-dict(), SD.make-string-dict(), SD.make-string-dict()) end

fun compile-worklist<a>(dfind :: (a, CS.Dependency -> Located<a>), locator :: Locator, context :: a) -> List<ToCompile>:
  temp-marked = SD.make-mutable-string-dict()
  var topo = empty
  fun visit(shadow locator :: Locator, shadow context :: a, curr-path :: List<Locator>) block:
    cases(Option) temp-marked.get-now(locator.uri()) block:
      | some(mark) =>
        when mark:
          raise("Detected module cycle: " + curr-path.reverse().map(_.uri()).join-str(" => "))
        end
      | none =>
        # mark current locator temporarily
        temp-marked.set-now(locator.uri(), true)
        pmap = SD.make-mutable-string-dict()
        deps = locator.get-dependencies()
        found-mods = for map(d from deps) block:
          found = dfind(context, d)
          pmap.set-now(d.key(), found.locator)
          found
        end
        # visit all dependents
        for map(f from found-mods):
          visit(f.locator, f.context, link(f.locator, curr-path))
        end
        # add current locator to head of topo sort
        topo := {locator: locator, dependency-map: pmap} ^ link(_, topo)
        # mark current locator permanently
        temp-marked.set-now(locator.uri(), false)
    end
    topo
  end
  # our include edges are backwards to how the topological sort algorithm expects dependencies,
  # so reverse the result
  visit(locator, context, [list: locator]).reverse()
end

type CompiledProgram = {loadables :: List<Loadable>, modules :: SD.MutableStringDict<Loadable>}

fun compile-program-with(worklist :: List<ToCompile>, modules, options) -> CompiledProgram:
  cache = modules
  loadables = for map(w from worklist):
    uri = w.locator.uri()
    if not(cache.has-key-now(uri)) block:
      provide-map = dict-map(
          w.dependency-map,
          lam(_, v): cache.get-value-now(v.uri()).provides end
      )
      options.before-compile(w.locator)
      loadable = compile-module(w.locator, provide-map, cache, options)
      # I feel like here we want to generate two copies of the loadable:
      # - One local for calling on-compile with and serializing
      # - One canonicalized for the local cache
      cache.set-now(uri, loadable)
      local-loadable = cases(Loadable) loadable:
        | module-as-string(provides, env, result) =>
          module-as-string(AU.localize-provides(provides, env), env, result)
      end
      # allow on-compile to return a new loadable
      options.on-compile(w.locator, local-loadable)
    else:
      cache.get-value-now(uri)
    end
  end
  { loadables: loadables, modules: cache }
end

fun compile-program(worklist, options):
  compile-program-with(worklist, SD.make-mutable-string-dict(), options)
end

fun is-builtin-module(uri :: String) -> Boolean:
  string-index-of(uri, "builtin://") == 0
end

fun compile-module(locator :: Locator, provide-map :: SD.StringDict<CS.Provides>, modules, options) -> Loadable block:
  G.reset()
  A.global-names.reset()
  #print("Compiling module: " + locator.uri() + "\n")
  env = CS.compile-env(locator.get-globals(), provide-map)
  cases(Option<Loadable>) locator.get-compiled() block:
    | some(loadable) =>
      #print("Module is already compiled\n")
      cases(Loadable) loadable:
        | module-as-string(pvds, ce-unused, m) =>
          module-as-string(AU.canonicalize-provides(pvds, env), ce-unused, m)
      end
    | none =>
      #print("Module is being freshly compiled\n")
      shadow options = locator.get-options(options)
      libs = locator.get-extra-imports()
      mod = locator.get-module()
      ast = cases(PyretCode) mod:
        | pyret-string(module-string) =>
          P.surface-parse(module-string, locator.uri())
        | pyret-ast(module-ast) =>
          module-ast
      end
      var ret = start
      var ast-ended = AU.append-nothing-if-necessary(ast)
      when options.collect-all:
        when is-some(ast-ended): ret := phase("Added nothing", ast-ended.value, ret) end
      end
      var wf = W.check-well-formed(ast-ended.or-else(ast))
      ast-ended := nothing
      when options.collect-all: ret := phase("Checked well-formedness", wf, ret) end
      checker = if options.check-mode and not(is-builtin-module(locator.uri())):
        CH.desugar-check
      else:
        CH.desugar-no-checks
      end
      cases(CS.CompileResult) wf block:
        | ok(_) =>
          var wf-ast = wf.code
          wf := nothing
          var checked = checker(wf-ast)
          wf-ast := nothing
          when options.collect-all:
            ret := phase(if options.check-mode: "Desugared (with checks)" else: "Desugared (skipping checks)" end,
              checked, ret)
          end
          var imported = AU.wrap-extra-imports(checked, libs)
          checked := nothing
          when options.collect-all: ret := phase("Added imports", imported, ret) end
          var scoped = RS.desugar-scope(imported, env)
          imported := nothing
          when options.collect-all: ret := phase("Desugared scope", scoped, ret) end
          var named-result = RS.resolve-names(scoped, env)
          scoped := nothing
          when options.collect-all: ret := phase("Resolved names", named-result, ret) end
          var named-ast = named-result.ast
          named-errors = named-result.errors
          var provides = AU.get-named-provides(named-result, locator.uri(), env)
          named-result := nothing
          var desugared = D.desugar(named-ast)
          named-ast := nothing
          when options.collect-all: ret := phase("Fully desugared", desugared, ret) end
          var type-checked =
            if options.type-check:
              type-checked = T.type-check(desugared, env, modules)
              if CS.is-ok(type-checked) block:
                provides := AU.get-typed-provides(type-checked.code, locator.uri(), env)
                CS.ok(type-checked.code.ast)
              else:
                type-checked
              end
            else: CS.ok(desugared)
            end
          desugared := nothing
          when options.collect-all: ret := phase("Type Checked", type-checked, ret) end
          cases(CS.CompileResult) type-checked block:
            | ok(_) =>
              var tc-ast = type-checked.code
              type-checked := nothing
              any-errors = named-errors + AU.check-unbound(env, tc-ast) + AU.bad-assignments(env, tc-ast)
              var dp-ast = DP.desugar-post-tc(tc-ast, env)
              tc-ast := nothing
              var cleaned = dp-ast
              dp-ast := nothing
              cleaned := cleaned.visit(AU.merge-nested-blocks)
              cleaned := cleaned.visit(AU.flatten-single-blocks)
              cleaned := cleaned.visit(AU.link-list-visitor(env))
              cleaned := cleaned.visit(AU.letrec-visitor)
              when options.collect-all: ret := phase("Cleaned AST", cleaned, ret) end
              var inlined = cleaned.visit(AU.inline-lams)
              cleaned := nothing
              when options.collect-all: ret := phase("Inlined lambdas", inlined, ret) end
              cr = if is-empty(any-errors):
                if options.collect-all: JSP.trace-make-compiled-pyret(ret, phase, inlined, env, provides, options)
                else: phase("Result", CS.ok(JSP.make-compiled-pyret(inlined, env, provides, options)), ret)
                end
              else:
                if options.collect-all and options.ignore-unbound: JSP.trace-make-compiled-pyret(ret, phase, inlined, env, options)
                else: phase("Result", CS.err(any-errors), ret)
                end
              end
              inlined := nothing
              r = if options.collect-all: cr else: cr.result end
              mod-result = module-as-string(AU.canonicalize-provides(provides, env), env, r)
              mod-result
            | err(_) => module-as-string(dummy-provides(locator.uri()), env,
                if options.collect-all: phase("Result", type-checked, ret) else: type-checked end)
          end
        | err(_) => module-as-string(dummy-provides(locator.uri()), env,
            if options.collect-all: phase("Result", wf, ret) else: wf end)
      end
  end
end

type PyretAnswer = Any
type PyretMod = Any

fun compile-and-run-worklist(ws :: List<ToCompile>, runtime :: R.Runtime, options):
  compile-and-run-worklist-with(ws, runtime, SD.make-mutable-string-dict(), options)
end

fun is-error-compilation(cr):
  is-module-as-string(cr) and CS.is-err(cr.result-printer)
end

fun compile-and-run-worklist-with(ws :: List<ToCompile>, runtime :: R.Runtime, initial :: SD.MutableStringDict<Loadable>, options):
  compiled-mods = compile-program-with(ws, initial, options).loadables
  errors = compiled-mods.filter(is-error-compilation)
  cases(List) errors:
    | empty =>
      load-infos = for map2(tc from ws, cm from compiled-mods):
        { to-compile: tc, compiled-mod: cm }
      end
      right(load-worklist(load-infos, SD.make-string-dict(), L.make-loader(runtime), runtime))
    | link(_, _) =>
      left(errors.map(_.result-printer))
  end
end

fun run-program(ws :: List<ToCompile>, prog :: CompiledProgram, realm :: L.Realm, runtime :: R.Runtime, options):
  compiled-mods = prog.loadables
  errors = compiled-mods.filter(is-error-compilation)
  cases(List) errors block:
    | empty =>
      #print("Make standalone program\n")
      program = make-standalone(ws, prog, options)
      #print("Run program\n")
      ans = right(L.run-program(runtime, realm, program.js-ast.to-ugly-source()))
      #print("Done\n")
      ans
    | link(_, _) =>
      left(errors.map(_.result-printer))
  end
end


fun load-worklist(ws, modvals :: SD.StringDict<PyretMod>, loader, runtime) -> PyretAnswer:
  doc: "Assumes topo-sorted worklist in ws"
  cases(List) ws block:
    | empty =>
      raise("Didn't get anything to run in run-worklist")
    | link(load-info, r) =>
      depmap = load-info.to-compile.dependency-map
      dependencies = load-info.to-compile.locator.get-dependencies()
      depnames = dependencies.map(lam(d): d.key() end).sort()
      depvals = for map(d from depnames):
        { modval: modvals.get-value(depmap.get-value-now(d).uri()), key: d }
      end
      m = load-info.compiled-mod
      when is-module-as-string(m) and CS.is-err(m):
        raise(m.result-printer.problems)
      end
      ans = loader.load(m, depvals, load-info.to-compile.locator.get-namespace(runtime), load-info.to-compile.locator)
      modvals-new = modvals.set(load-info.to-compile.locator.uri(), ans)
      answer = loader.run(ans, m, load-info.to-compile.locator.uri())
      cases(List) r:
        | empty => answer
        | link(_, _) => load-worklist(r, modvals-new, loader, runtime)
      end
  end
end

fun _compile-and-run-locator(locator, finder, context, runtime, options):
  wl = compile-worklist(finder, locator, context)
  compile-and-run-worklist(wl, runtime, options)
end

fun compile-and-run-locator(locator, finder, context, realm, runtime, starter-modules, options) block:
  #print("Make worklist\n")
  wl = compile-worklist(finder, locator, context)
  #print("Compile program\n")

  compiled = compile-program-with(wl, starter-modules, options)
  compiled-mods = compiled.loadables
  errors = compiled-mods.filter(is-error-compilation)
  cases(List) errors block:
    | empty =>
      #print("Make standalone program\n")

      program = make-standalone(wl, compiled, options)
      #print("Run program\n")

      ans = right(L.run-program(runtime, realm, program.js-ast.to-ugly-source()))
      #print("Done\n")
      ans
    | link(_, _) =>
      left(errors.map(_.result-printer))
  end
end

fun compile-standalone(wl, starter-modules, options):
  compiled = compile-program-with(wl, starter-modules, options)
  make-standalone(wl, compiled, options)
end

# NOTE(joe): I strongly suspect options will be used in the future
fun make-standalone(wl, compiled, options) block:
  natives = for fold(natives from empty, w from wl):
    w.locator.get-native-modules().map(_.path) + natives
  end

  var failure = false
  static-modules = j-obj(for C.map_list(w from wl):
    loadable = compiled.modules.get-value-now(w.locator.uri())
    cases(Loadable) loadable:
      | module-as-string(_, _, rp) =>
        cases(CS.CompileResult) rp block:
          | ok(code) =>
            j-field(w.locator.uri(), J.j-raw-code(code.pyret-to-js-runnable()))
          | err(problems) =>
            for lists.each(e from problems):
              print-error(RED.display-to-string(e.render-reason(), torepr, empty))
            end
            failure := true
            j-field(w.locator.uri(), J.j-raw-code("\"error\""))
        end
    end
  end)
  when failure:
    raise("There were compilation errors")
  end

  depmap = j-obj(for C.map_list(w from wl):
    deps = w.dependency-map
    j-field(w.locator.uri(),
      j-obj(for C.map_list(k from deps.keys-now().to-list()):
        j-field(k, j-str(deps.get-value-now(k).uri()))
      end))
  end)

  to-load = j-list(false, for C.map_list(w from wl):
    j-str(w.locator.uri())
  end)

  program-as-js = j-obj([C.clist:
      j-field("staticModules", static-modules),
      j-field("depMap", depmap),
      j-field("toLoad", to-load)
    ])

  {
    js-ast: program-as-js,
    natives: natives
  }
end
