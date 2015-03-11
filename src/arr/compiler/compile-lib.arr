provide *
provide-types *

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

type URI = String

data PyretCode:
  | pyret-string(s :: String)
  | pyret-ast(ast :: A.Program)
end

data Loadable:
  | module-as-string(result-printer :: CS.CompileResult<JSP.CompiledCodePrinter>)
  # Doesn't need compilation, just contains a JS closure
  | pre-loaded(include-base-libs :: Boolean, internal-mod :: Any)
end

type Provides = Set<String>

type CompileContext = Any

type Locator = {
 
  # Could either have needs-provide be implicitly stateful, and cache
  # the most recent map, or use explicit interface below
  needs-compile :: (SD.MutableStringDict<Provides> -> Boolean),

  get-module :: ( -> PyretCode),
  get-dependencies :: ( -> List<CS.Dependency>),
  get-provides :: ( -> Provides),
  get-compile-env :: ( -> CS.CompileEnv),
  get-namespace :: (R.Runtime -> N.Namespace),

  uri :: (-> URI),
  name :: (-> String),

  # Note that CompileResults can contain both errors and successful
  # compilations
  set-compiled :: (Loadable, SD.MutableStringDict<Provides> -> Nothing),
  get-compiled :: ( -> Option<Loadable>),

  # _equals should compare uris for locators
  _equals :: Method
}

fun get-ast(p :: PyretCode, uri :: URI):
  cases(PyretCode) p:
    | pyret-string(s) => P.surface-parse(s, uri)
    | pyret-ast(a) => a
  end
end

fun get-dependencies(p :: PyretCode, uri :: URI) -> List<CS.Dependency>:
  parsed = get-ast(p, uri)
  for map(s from parsed.imports.map(_.file)):
    cases(A.ImportType) s:
      # crossover compatibility
      | s-file-import(l, path) => CS.dependency("legacy-path", [list: path])
      | s-const-import(l, modname) => CS.builtin(modname)
      | s-special-import(l, kind, args) => CS.dependency(kind, args)
    end
  end
end

fun get-dependencies-with-env(p :: PyretCode, uri :: URI, env :: CS.CompileEnvironment) -> List<CS.Dependency>:
  mod-deps = get-dependencies(p, uri)
  env-deps = for map(e from env.bindings.filter(CS.is-module-bindings)):
    CS.builtin(e.name)
  end
  mod-deps.append(env-deps)
end

fun get-provides(p :: PyretCode, uri :: URI) -> Provides:
  parsed = get-ast(p, uri)
  cases (A.Provide) parsed._provide:
    | s-provide-none(l) => S.empty-list-set
    | s-provide-all(l) => S.list-to-list-set(A.toplevel-ids(parsed).map(_.toname()))
    | s-provide(l, e) =>
      cases (A.Expr) e:
        | s-obj(_, mlist) => S.list-to-list-set(mlist.map(_.name))
        | else => raise("Non-object expression in provide: " + l.format(true))
      end
  end
end

type ToCompile = { locator: Locator, dependency-map: SD.MutableStringDict<Locator>, path :: List<Locator> }

fun dict-map<a, b>(sd :: SD.MutableStringDict, f :: (String, a -> b)):
  sd2 = SD.make-mutable-string-dict()
  for each(k from sd.keys-now().to-list()):
    sd2.set-now(k, f(k, sd.get-value-now(k)))
  end
  sd2
end

fun make-compile-lib(dfind :: (CompileContext, CS.Dependency -> Locator)) -> { compile-worklist: Function, compile-program: Function }:

  fun compile-worklist(locator :: Locator, context :: CompileContext) -> List<ToCompile>:
    fun add-preds-to-worklist(shadow locator :: Locator, curr-path :: List<ToCompile>) -> List<ToCompile>:
      when is-some(curr-path.find(lam(tc): tc.locator == locator end)):
        raise("Detected module cycle: " + curr-path.map(_.locator).map(_.uri()).join-str(", "))
      end
      pmap = SD.make-mutable-string-dict()
      deps = locator.get-dependencies()
      dlocs = for map(d from deps):
        dloc = dfind(context, d)
        pmap.set-now(d.key(), dloc)
        dloc
      end
      tocomp = {locator: locator, dependency-map: pmap, path: curr-path}
      for fold(ret from [list: tocomp], dloc from dlocs):
        pret = add-preds-to-worklist(dloc, curr-path + [list: tocomp])
        pret + ret
      end
    end
    add-preds-to-worklist(locator, empty)
  end

  fun compile-program(worklist :: List<ToCompile>) -> List<Loadable>:
    cache = SD.make-mutable-string-dict()
    for map(w from worklist):
      uri = w.locator.uri()
      if not(cache.has-key-now(uri)):
        cr = compile-module(w.locator, w.dependency-map)
        cache.set-now(uri, cr)
        cr
      else:
        cache.get-value-now(uri)
      end
    end
  end

  rec options = {
    check-mode : true,
    allow-shadowed : false,
    collect-all: false,
    type-check: false,
    ignore-unbound: false
  }

  fun compile-module(locator :: Locator, dependencies :: SD.MutableStringDict<Locator>) -> Loadable:
    provide-map = dict-map(dependencies, lam(_, v): v.get-provides() end)
    if locator.needs-compile(provide-map):
      mod = locator.get-module()
      cr = cases(PyretCode) mod:
        | pyret-string(module-string) =>
          CM.compile-js(
            CM.start,
            "Pyret",
            module-string,
            locator.uri(),
            locator.get-compile-env(),
            options
            ).result
        | pyret-ast(module-ast) =>
          CM.compile-js-ast(
            CM.start,
            module-ast,
            locator.uri(),
            locator.get-compile-env(),
            options
            ).result
      end
      locator.set-compiled(module-as-string(cr), provide-map)
      module-as-string(cr)
    else:
      locator.get-compiled().value
    end
  end

  {compile-worklist: compile-worklist, compile-program: compile-program}
end

type PyretAnswer = Any
type PyretMod = Any

fun compile-and-run-worklist(cl, ws :: List<ToCompile>, runtime :: R.Runtime):
  compile-and-run-worklist-with(cl, ws, runtime, SD.make-string-dict())
end

fun compile-and-run-worklist-with(cl, ws :: List<ToCompile>, runtime :: R.Runtime, initial :: SD.StringDict<PyretMod>):
  compiled-mods = cl.compile-program(ws)
  load-infos = for map2(tc from ws, cm from compiled-mods):
    { to-compile: tc, compiled-mod: cm }
  end
  load-worklist(load-infos, initial, L.make-loader(runtime), runtime)
end

fun load-worklist(ws, modvals :: SD.StringDict<PyretMod>, loader, runtime) -> PyretAnswer:
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
      answer = loader.run(ans, load-info.to-compile.locator.uri())
      cases(List) r:
        | empty => answer
        | link(_, _) => load-worklist(r, modvals-new, loader, runtime)
      end
  end
end

