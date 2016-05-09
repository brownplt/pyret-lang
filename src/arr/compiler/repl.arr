
provide { make-repl: make-repl } end
import base as _
import either as E
import load-lib as L
import namespace-lib as N
import string-dict as SD
import runtime-lib as R
import ast as A
import file("./ast-util.arr") as U
import file("./resolve-scope.arr") as RN
import file("./compile-structs.arr") as CS
import file("./compile-lib.arr") as CL
import file("./repl-support.arr") as RS
import file("./type-structs.arr") as TS
import file("./ast-util.arr") as AU

type Either = E.Either

fun filter-env-by-imports(env :: CS.CompileEnvironment, l :: CL.Locator, dep :: String, g :: CS.Globals) -> {new-globals:: CS.Globals, new-extras:: List<CS.ExtraImport>}:
  cases(A.Program) CL.get-ast(l.get-module(), l.uri()):
    | s-program(_, _, _, imports, _) =>
      for fold(res from {new-globals: g, new-extras: empty}, i from imports):
        shadow g = res.new-globals
        cases(A.Import) RN.expand-import(i, env):
          | s-import-complete(_, values, types, file, vals-name, type-name) =>
            depname = AU.import-to-dep(file).key()
            new-vals = for fold(vs from g.values, k from values.map(_.toname())):
              vs.set(k, depname)
            end
            new-types = for fold(ts from g.types, k from types.map(_.toname())):
              ts.set(k, depname)
            end
            ng = CS.globals(
              new-vals.set(vals-name.toname(), dep),
              new-types.set(type-name.toname(), dep))
            ne = link(CS.extra-import(AU.import-to-dep(file), "_", empty, empty), res.new-extras)
            { new-globals: ng, new-extras: ne }
        end
      end
  end
end

fun make-repl<a>(
    runtime :: R.Runtime,
    defs-locator :: CL.Locator,
    compile-context :: a,
    finder :: (a, CS.Dependency -> CL.Located<a>)):

  var nspace = defs-locator.get-namespace(runtime)
  var globals = defs-locator.get-globals()
  var modules = SD.make-mutable-string-dict()
  var current-type-check = false
  var extra-imports = CS.standard-imports
  var locator-cache = SD.make-mutable-string-dict()

  shadow finder = lam(context, dep):
    if CS.is-dependency(dep) and (dep.protocol == "repl"):
      cases(Option) locator-cache.get-now(dep.arguments.first):
        | some(l) => CL.located(l, context)
        | none => raise("Cannot find module: " + torepr(dep))
      end
    else:
      finder(context, dep)
    end
  end

  fun update-env(result, loc, cr):
    dep = CS.dependency("repl", [list: loc.uri()])

    globs-and-imports = filter-env-by-imports(cr.compile-env, loc, dep.key(), globals)
    globals := globs-and-imports.new-globals
    extra-imports := CS.extra-imports(
      globs-and-imports.new-extras +
      link(
        CS.extra-import(dep, "_", [list:], [list:]),
        extra-imports.imports))

    provided = cr.provides.values.keys-list()
    new-vals = for fold(vs from globals.values, provided-name from provided):
      vs.set(provided-name, dep.key())
    end
    tprovided = cr.provides.aliases.keys-list()
    new-types = for fold(ts from globals.types, provided-name from tprovided):
      ts.set(provided-name, dep.key())
    end
    globals := CS.globals(new-vals, new-types)

    locator-cache.set-now(loc.uri(), loc)


    #provided = loc.get-provides().values.keys-list()
    #new-vals = for fold(vs from globals.values, provided-name from provided):
    #  vs.set(provided-name, CS.v-just-there)
    #end
    #tprovided = loc.get-provides().types.keys-list()
    #new-types = for fold(ts from globals.types, provided-name from tprovided):
    #  ts.set(provided-name, CS.t-just-there)
    #end
  end

  fun restart-interactions(type-check :: Boolean):
    current-type-check := type-check
    modules := SD.make-mutable-string-dict()
    locator-cache := SD.make-mutable-string-dict()
    extra-imports := CS.standard-imports
    globals := defs-locator.get-globals()
    worklist = CL.compile-worklist(finder, defs-locator, compile-context)
    compiled = CL.compile-program-with(worklist, modules, CS.default-compile-options.{type-check: current-type-check, compile-module: true})
    modules.set-now(defs-locator.uri(), compiled.loadables.last())
    result = CL.run-program(worklist, compiled, runtime, CS.default-compile-options.{type-check: current-type-check})
    cases(Either) result:
      | right(answer) =>
        when L.is-success-result(answer):
          update-env(answer, defs-locator, compiled.loadables.last())
        end
      | left(err) =>
        nothing
    end
    result
  end

  fun run-interaction(repl-locator :: CL.Locator):
    worklist = CL.compile-worklist(finder, repl-locator, compile-context)
    compiled = CL.compile-program-with(worklist, modules, CS.default-compile-options.{type-check: current-type-check, compile-module: true})
    modules.set-now(repl-locator.uri(), compiled.loadables.last())
    result = CL.run-program(worklist, compiled, runtime, CS.default-compile-options.{type-check: current-type-check, compile-module: true})
    cases(Either) result:
      | right(answer) =>
        when L.is-success-result(answer):
          update-env(answer, repl-locator, compiled.loadables.last())
        end
      | left(err) =>
        nothing
    end
    result
  end

  {
    restart-interactions: restart-interactions,
    run-interaction: run-interaction,
    runtime: runtime,
    get-current-namespace: lam(): nspace end,
    get-current-globals: lam(): globals end,
    get-current-extra-imports: lam(): extra-imports end
  }
end
