provide *
provide-types *

import parse-pyret as P
import ast as A
import sets as S
import string-dict as SD
import "compiler/compile.arr" as CM
import "compiler/compile-structs.arr" as CS

type URI = String

data PyretCode:
  | pyret-string(s :: String)
  | pyret-ast(ast :: A.Program)
end

type Provides = Set<String>

type CompileContext = Any

type Locator = {
 
  # Could either have needs-provide be implicitly stateful, and cache
  # the most recent map, or use explicit interface below
  needs-compile :: (SD.MutableStringDict<Provides> -> Boolean),

  get-module :: ( -> PyretCode),
  get-dependencies :: ( -> Set<CS.Dependency>),
  get-provides :: ( -> Provides),
  get-compile-env :: ( -> CS.CompileEnv),

  # e.g. create a new CompileContext that is at the base of the directory
  # this Locator is in.  The CC holds the current working directory
  update-compile-context :: (CompileContext -> CompileContext),

  uri :: (-> URI),
  name :: (-> String),

  # Note that CompileResults can contain both errors and successful
  # compilations
  set-compiled :: (CS.CompileResult, SD.MutableStringDict<Provides> -> Nothing),
  get-compiled :: ( -> Option<CS.CompileResult>),

  # _equals should compare uris for locators
  _equals :: Method
}

fun get-ast(p :: PyretCode, uri :: URI):
  cases(PyretCode) p:
    | pyret-string(s) => P.surface-parse(s, uri)
    | pyret-ast(a) => a
  end
end

fun get-dependencies(p :: PyretCode, uri :: URI) -> Set<CS.Dependency>:
  parsed = get-ast(p, uri)
  special-imports = parsed.imports.map(_.file).filter(A.is-s-special-import)
  dependency-list = for map(s from special-imports):
    CS.dependency(s.kind, s.args)
  end
  S.list-to-list-set(dependency-list)
end

fun get-provides(p :: PyretCode, uri :: URI) -> Provides:
  parsed = get-ast(p, uri)
  cases (A.Provides) parsed._provide:
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
    fun add-preds-to-worklist(shadow locator :: Locator, shadow context :: CompileContext, curr-path :: List<ToCompile>) -> List<ToCompile>:
      when is-some(curr-path.find(lam(tc): tc.locator == locator end)):
        raise("Detected module cycle: " + curr-path.map(_.locator).map(_.uri()).join-str(", "))
      end
      pmap = SD.make-mutable-string-dict()
      deps = locator.get-dependencies().to-list()
      dlocs = for map(d from deps):
        dloc = dfind(context, d)
        pmap.set-now(d.key(), dloc)
        dloc
      end
      tocomp = {locator: locator, dependency-map: pmap, path: curr-path}
      for fold(ret from [list: tocomp], dloc from dlocs):
        pret = add-preds-to-worklist(dloc, dloc.update-compile-context(context), curr-path + [list: tocomp])
        pret + ret
      end
    end
    add-preds-to-worklist(locator, context, empty)
  end

  fun compile-program(worklist :: List<ToCompile>) -> List<CS.CompileResult>:
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

  fun compile-module(locator :: Locator, dependencies :: SD.MutableStringDict<Locator>) -> CS.CompileResult:
    provide-map = dict-map(dependencies, lam(_, v): v.get-provides() end)
    if locator.needs-compile(provide-map):
      mod = locator.get-module()
      cr = cases(PyretCode) mod:
        | pyret-string(module-string) =>
          CM.compile-js(
            CM.start,
            "Pyret",
            module-string,
            locator.name(),
            locator.get-compile-env(),
            options
            ).result
        | pyret-ast(module-ast) =>
          CM.compile-js-ast(
            CM.start,
            "Pyret",
            module-ast,
            locator.name(),
            locator.get-compile-env(),
            options
            ).result
      end
      locator.set-compiled(cr, provide-map)
      cr
    else:
      locator.get-compiled().value
    end
  end

  {compile-worklist: compile-worklist, compile-program: compile-program}
end

