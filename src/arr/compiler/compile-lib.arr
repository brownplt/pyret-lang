provide *
provide-types *

import parse-pyret as P
import ast as A
import sets as S
import string-dict as SD
import "compiler/compile.arr" as CM
import "compiler/compile-structs.arr" as CS

data Dependency:
  | dependency(protocol :: String, arguments :: List<String>)
    with:
    key(self): self.protocol + "(" + self.arguments.join-str(", ") + ")" end
end

type URI = String

type PyretCode = String

type Provides = Set<String>

type CompileContext = Any

type Locator = {
 
  # Could either have needs-provide be implicitly stateful, and cache
  # the most recent map, or use explicit interface below
  needs-compile :: (SD.StringDict<Provides> -> Boolean),

  get-module :: ( -> PyretCode),

  # e.g. create a new CompileContext that is at the base of the directory
  # this Locator is in.  The CC holds the current working directory
  update-compile-context :: (CompileContext -> CompileContext),

  uri :: (-> URI),
  name :: (-> String),

  # Note that CompileResults can contain both errors and successful
  # compilations
  set-compiled :: (CS.CompileResult, SD.StringDict<Provides> -> Nothing),
  get-compiled :: ( -> Option<CS.CompileResult>),

  # _equals should compare uris for locators
  _equals :: Method
}

fun get-dependencies(loc :: Locator) -> Set<Dependency>:
  parsed = P.surface-parse(loc.get-module(), loc.uri())
  special-imports = parsed.imports.map(_.file).filter(A.is-s-special-import)
  dependency-list = for map(s from special-imports):
    dependency(s.kind, s.args)
  end
  S.list-to-list-set(dependency-list)
end

fun get-provides(loc :: Locator) -> Provides:
  parsed = P.surface-parse(loc.get-module(), loc.uri())
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

type ToCompile = { locator: Locator, provide-map: SD.StringDict<Provides>, path :: List<Locator> }

fun make-compile-lib(dfind :: (CompileContext, Dependency -> Locator)) -> { compile-worklist: Function, compile-program: Function }:

  fun compile-worklist(locator :: Locator, context :: CompileContext) -> List<ToCompile>:
    fun add-preds-to-worklist(shadow locator :: Locator, shadow context :: CompileContext, curr-path :: List<ToCompile>) -> List<ToCompile>:
      when is-some(curr-path.find(lam(tc): tc.locator == locator end)):
        raise("Detected module cycle: " + curr-path.map(_.locator).map(_.uri()).join-str(", "))
      end
      pmap = SD.string-dict()
      deps = get-dependencies(locator).to-list()
      dlocs = for map(d from deps):
        dloc = dfind(context, d)
        pmap.set(d.key(), get-provides(dloc))
        dloc
      end
      tocomp = {locator: locator, provide-map: pmap, path: curr-path}
      for fold(ret from [list: tocomp], dloc from dlocs):
        pret = add-preds-to-worklist(dloc, dloc.update-compile-context(context), curr-path + [list: tocomp])
        pret + ret
      end
    end
    add-preds-to-worklist(locator, context, empty)
  end

  fun compile-program(worklist :: List<ToCompile>) -> List<CS.CompileResult>:
    cache = SD.string-dict()
    for map(w from worklist):
      uri = w.locator.uri()
      if not(cache.has-key(uri)):
        cr = compile-module(w.locator, w.provide-map)
        cache.set(uri, cr)
        cr
      else:
        cache.get(uri)
      end
    end
  end

  fun compile-module(locator :: Locator, dependencies :: SD.StringDict<Provides>) -> CS.CompileResult:
    if locator.needs-compile(dependencies):
      cr = CM.compile-js(
        CM.start,
        "Pyret",
        locator.get-module(),
        locator.name(),
        CS.standard-builtins,
        {
          check-mode : true,
          allow-shadowed : false,
          collect-all: false,
          type-check: false,
          ignore-unbound: false
        }
        ).result
      locator.set-compiled(cr, dependencies)
      cr
    else:
      locator.get-compiled().value
    end
  end

  {compile-worklist: compile-worklist, compile-program: compile-program}
end

