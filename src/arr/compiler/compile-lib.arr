provide *
provide-types *

import either as E
import parse-pyret as P
import ast as A
import load-lib as L
import namespace-lib as N
import runtime-lib as R
import sets as S
import string-dict as SD
import "compiler/compile.arr" as CM
import "compiler/compile-structs.arr" as CS
import "compiler/js-of-pyret.arr" as JSP
import "compiler/ast-util.arr" as AU
import "compiler/well-formed.arr" as W
import "compiler/desugar.arr" as D
import "compiler/desugar-post-tc.arr" as DP
import "compiler/type-check.arr" as T
import "compiler/desugar-check.arr" as CH
import "compiler/resolve-scope.arr" as RS

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
  | module-as-string(provides :: CS.Provides, compile-env :: CS.CompileEnvironment, result-printer :: CS.CompileResult<JSP.CompiledCodePrinter>)
  # Doesn't need compilation, just contains a JS closure
  | pre-loaded(provides :: CS.Provides, compile-env :: CS.CompileEnvironment, internal-mod :: Any)
end

type Provides = CS.Provides

type Locator = {
 
  # Could either have needs-provide be implicitly stateful, and cache
  # the most recent map, or use explicit interface below
  needs-compile :: (SD.StringDict<Provides> -> Boolean),

  # Pre-compile (skippable if get-compile returns something)
  get-module :: ( -> PyretCode),

  # Pre-compile (had better be known with no other help)
  get-dependencies :: ( -> List<CS.Dependency>),

  # Pre-compile
  get-extra-imports :: ( -> CS.ExtraImports),

  # Post- or pre- compile
  # get-import-names :: ( -> List<String>),

  # Pre-compile, specification of available globals
  get-globals :: ( -> CS.Globals),

  # Post-compile, on-run (maybe dynamic and new namespaces)
  get-namespace :: (R.Runtime -> N.Namespace),

  uri :: (-> URI),
  name :: (-> String),

  set-compiled :: (Loadable, SD.StringDict<Provides> -> Nothing),

  # Pre-compile if needs-compile is false
  get-compiled :: ( -> Option<Loadable>),

  # _equals should compare uris for locators
  _equals :: Method
}

fun string-locator(uri :: URI, s :: String):
  {
    needs-compile(self, _): true end,
    get-module(self): pyret-string(s) end,
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

type ToCompile = { locator :: Locator, dependency-map :: SD.MutableStringDict<Locator>, path :: List<Locator> }

fun dict-map<a, b>(sd :: SD.MutableStringDict, f :: (String, a -> b)):
  for fold(sd2 from mtd, k from sd.keys-now().to-list()):
    sd2.set(k, f(k, sd.get-value-now(k)))
  end
end

dummy-provides = lam(uri): CS.provides(uri, SD.make-string-dict(), SD.make-string-dict(), SD.make-string-dict()) end

# Use ConcatList if it's easy
fun compile-worklist<a>(dfind :: (a, CS.Dependency -> Located<a>), locator :: Locator, context :: a) -> List<ToCompile>:
  fun add-preds-to-worklist(shadow locator :: Locator, shadow context :: a, curr-path :: List<ToCompile>) -> List<ToCompile>:
    when is-some(curr-path.find(lam(tc): tc.locator == locator end)):
      raise("Detected module cycle: " + curr-path.map(_.locator).map(_.uri()).join-str(", "))
    end
    pmap = SD.make-mutable-string-dict()
    deps = locator.get-dependencies()
    found-mods = for map(d from deps):
      found = dfind(context, d)
      pmap.set-now(d.key(), found.locator)
      found
    end
    tocomp = {locator: locator, dependency-map: pmap, path: curr-path}
    for fold(ret from [list: tocomp], f from found-mods):
      pret = add-preds-to-worklist(f.locator, f.context, curr-path + [list: tocomp])
      pret + ret
    end
  end
  maybe-duplicated-preds = add-preds-to-worklist(locator, context, empty)
  fun remove-from-rest(l):
    cases(List) l:
      | empty => empty
      | link(f, r) =>
        link(f, remove-from-rest(r.filter(lam(d): d.locator.uri() <> f.locator.uri() end)))
    end
  end
  remove-from-rest(maybe-duplicated-preds)
end

type CompiledProgram = {loadables :: List<Loadable>, modules :: SD.MutableStringDict<Loadable>}

fun compile-program-with(worklist :: List<ToCompile>, modules, options) -> CompiledProgram:
  cache = modules
  loadables = for map(w from worklist):
    uri = w.locator.uri()
    if not(cache.has-key-now(uri)):
      provide-map = dict-map(
          w.dependency-map,
          lam(_, v): cache.get-value-now(v.uri()).provides
        end)
      loadable = compile-module(w.locator, provide-map, cache, options)
      cache.set-now(uri, loadable)
      loadable
    else:
      cache.get-value-now(uri)
    end
  end
  { loadables: loadables, modules: cache }
end

fun compile-program(worklist, options):
  compile-program-with(worklist, SD.make-mutable-string-dict(), options)
end

fun compile-module(locator :: Locator, provide-map :: SD.StringDict<CS.Provides>, modules, options) -> Loadable:
  if locator.needs-compile(provide-map):
    env = CS.compile-env(locator.get-globals(), provide-map)
    libs = locator.get-extra-imports()
    mod = locator.get-module()
    ast = cases(PyretCode) mod:
      | pyret-string(module-string) =>
        P.surface-parse(module-string, locator.uri())
      | pyret-ast(module-ast) =>
        module-ast
    end
    var ret = CM.start
    phase = CM.phase
    ast-ended = AU.append-nothing-if-necessary(ast)
    when options.collect-all:
      when is-some(ast-ended): ret := phase("Added nothing", ast-ended.value, ret) end
    end
    wf = W.check-well-formed(ast-ended.or-else(ast))
    when options.collect-all: ret := phase("Checked well-formedness", wf, ret) end
    checker = if options.check-mode: CH.desugar-check else: CH.desugar-no-checks;
    cases(CS.CompileResult) wf:
      | ok(wf-ast) =>
        checked = checker(wf-ast)
        when options.collect-all:
          ret := phase(if options.check-mode: "Desugared (with checks)" else: "Desugared (skipping checks)" end,
            checked, ret)
        end
        imported = AU.wrap-extra-imports(checked, libs)
        when options.collect-all: ret := phase("Added imports", imported, ret) end
        scoped = RS.desugar-scope(imported, env)
        when options.collect-all: ret := phase("Desugared scope", scoped, ret) end
        named-result = RS.resolve-names(scoped, env)
        when options.collect-all: ret := phase("Resolved names", named-result, ret) end
        named-ast = named-result.ast
        named-errors = named-result.errors
        var provides = AU.get-named-provides(named-result, locator.uri(), env)
        desugared = D.desugar(named-ast)
        when options.collect-all: ret := phase("Fully desugared", desugared, ret) end
        type-checked =
          if options.type-check:
            type-checked = T.type-check(desugared, env, modules)
            if CS.is-ok(type-checked):
              provides := AU.get-typed-provides(type-checked.code, locator.uri(), env)
              CS.ok(type-checked.code.ast)
            else:
              type-checked
            end
          else: CS.ok(desugared);
        when options.collect-all: ret := phase("Type Checked", type-checked, ret) end
        cases(CS.CompileResult) type-checked:
          | ok(tc-ast) =>
            any-errors = named-errors + AU.check-unbound(env, tc-ast) + AU.bad-assignments(env, tc-ast)
            dp-ast = DP.desugar-post-tc(tc-ast, env)
            cleaned = dp-ast.visit(AU.merge-nested-blocks)
                            .visit(AU.flatten-single-blocks)
                            .visit(AU.link-list-visitor(env))
                            .visit(AU.letrec-visitor)
            when options.collect-all: ret := phase("Cleaned AST", cleaned, ret) end
            inlined = cleaned.visit(AU.inline-lams)
            when options.collect-all: ret := phase("Inlined lambdas", inlined, ret) end
            cr = if is-empty(any-errors):
              if options.collect-all: JSP.trace-make-compiled-pyret(ret, phase, inlined, env, options)
              else: phase("Result", CS.ok(JSP.make-compiled-pyret(inlined, env, options)), ret)
              end
            else:
              if options.collect-all and options.ignore-unbound: JSP.trace-make-compiled-pyret(ret, phase, inlined, env, options)
              else: phase("Result", CS.err(any-errors), ret)
              end
            end
            mod-result = module-as-string(provides, env, cr.result)
            locator.set-compiled(mod-result, provide-map)
            mod-result
          | err(_) => module-as-string(dummy-provides(locator.uri()), env, type-checked) #phase("Result", type-checked, ret)
        end
      | err(_) => module-as-string(dummy-provides(locator.uri()), env, wf) #phase("Result", wf, ret)
    end
  else:
    cases(Option) locator.get-compiled():
      | none => raise("No precompiled module found for " + locator.uri())
      | some(v) => v
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

fun run-program(ws :: List<ToCompile>, prog :: CompiledProgram, runtime :: R.Runtime, options):
  compiled-mods = prog.loadables
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


fun load-worklist(ws, modvals :: SD.StringDict<PyretMod>, loader, runtime) -> PyretAnswer:
  doc: "Assumes topo-sorted worklist in ws"
  cases(List) ws:
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
      ans = loader.load(m, depvals, load-info.to-compile.locator.get-namespace(runtime))
      modvals-new = modvals.set(load-info.to-compile.locator.uri(), ans)
      answer = loader.run(ans, m, load-info.to-compile.locator.uri())
      cases(List) r:
        | empty => answer
        | link(_, _) => load-worklist(r, modvals-new, loader, runtime)
      end
  end
end

fun compile-and-run-locator(locator, finder, context, runtime, options):
  wl = compile-worklist(finder, locator, context)
  compile-and-run-worklist(wl, runtime, options)
end
