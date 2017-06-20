
provide *
import ast as A
import base as _
import either as E
import load-lib as L
import parse-pyret as P
import string-dict as SD
import runtime-lib as R
import file("./ast-util.arr") as U
import file("./resolve-scope.arr") as RN
import file("./compile-structs.arr") as CS
import file("./compile-lib.arr") as CL
import file("./type-structs.arr") as TS
import file("./ast-util.arr") as AU

type Either = E.Either

fun add-global-binding(env :: CS.CompileEnvironment, name :: String):
  CS.compile-env(
    CS.globals(env.globals.values.set(name, TS.t-top), env.globals.types),
    env.mods)
end

fun add-global-type-binding(env :: CS.CompileEnvironment, name :: String):
  CS.compile-env(
    CS.globals(env.globals.values, env.globals.types.set(name, TS.t-top)),
    env.mods)
end

fun get-special-imports(program):
  cases(A.Program) program:
    | s-program(l, _, _, imports, _) =>
      special-imps = for filter(i from imports):
        A.is-s-special-import(i.file)
      end
      special-imps.map(_.file)
  end
end

fun get-imp-dependency(imp):
  cases(A.Import) imp:
    | s-include(_, mod) => mod
    | else => imp.file
  end
end

fun get-defined-ids(p, imports, body):
  ids = A.toplevel-ids(p)
  import-names = for fold(names from empty, imp from imports):
    cases(A.Import) imp:
      | s-import(_, _, name) => link(name, names)
      | s-import-fields(_, imp-names, _) => names + imp-names
      | s-include(_, _) => names
      | else => raise("Unknown import type: " + torepr(imp))
    end
  end
  ids-plus-import-names = import-names + ids
  type-ids = A.block-type-ids(body)
  import-type-names = for fold(names from empty, imp from imports):
    cases(A.Import) imp:
      | s-import(_, _, name) => link(name, names)
      | s-import-fields(_, imp-names, _) => names
      | s-include(_, _) => names
      | else => raise("Unknown import type: " + torepr(imp))
    end
  end
  type-ids-plus-import-names = import-type-names + type-ids.map(_.name)
  {
    imports: imports,
    ids: ids-plus-import-names.filter(lam(id): not(A.is-s-underscore(id)) end),
    type-ids: type-ids-plus-import-names.filter(lam(id): not(A.is-s-underscore(id)) end)
  }
end

fun df(l, name):
  A.s-data-field(l, name.toname(), A.s-id(l, name))
end

fun af(l,  name):
  A.a-field(l, name.toname(), A.a-name(l, name))
end

fun make-provide-for-repl(p :: A.Program):
  cases(A.Program) p:
    | s-program(l, _, _, imports, body) =>
      defined-ids = get-defined-ids(p, imports, body)
      repl-provide = for map(n from defined-ids.ids): df(l, n) end
      repl-type-provide = for map(n from defined-ids.type-ids): af(l, n) end
      A.s-program(l,
          A.s-provide(l, A.s-obj(l, repl-provide)),
          A.s-provide-types(l, repl-type-provide),
          defined-ids.imports,
          body)
  end
end

fun make-provide-for-repl-main-env(p :: A.Program, env :: CS.CompileEnvironment):
  make-provide-for-repl-main(p, env.globals)
end

fun make-provide-for-repl-main(p :: A.Program, globals :: CS.Globals):
  doc: "Make the program simply provide all (for the repl)"
  cases(A.Program) p:
    | s-program(l, _, _, imports, body) =>
      defined-ids = get-defined-ids(p, imports, body)
      repl-provide = for map(n from defined-ids.ids): df(l, n) end
      repl-type-provide = for map(n from defined-ids.type-ids): af(l, n) end
      env-provide = for SD.fold-keys(flds from repl-provide, name from globals.values):
        link(df(l, A.s-name(l, name)), flds)
      end
      env-type-provide = for SD.fold-keys(flds from repl-type-provide, name from globals.types):
        link(af(l, A.s-name(l, name)), flds)
      end
      A.s-program(l,
          A.s-provide(l, A.s-obj(l, env-provide)),
          A.s-provide-types(l, env-type-provide),
          defined-ids.imports,
          body)
  end
end


fun make-definitions-finder(import-types :: SD.StringDict, make-builtin):
  fun definitions-finder(context, dep):
    l = cases(CS.Dependency) dep:
      | builtin(name) => make-builtin(name)
      | dependency(protocol, arguments) =>
        cases(Option) import-types.get(protocol):
          | none => raise("Cannot find module: " + torepr(dep))
          | some(handler) => handler(context, arguments)
        end
    end
    CL.located(l, context)
  end
  definitions-finder
end


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
    modules :: SD.MutableStringDict<CL.Loadable>,
    realm :: L.Realm,
    compile-context :: a,
    make-finder :: (-> (a, CS.Dependency -> CL.Located<a>))):

  var globals = CS.standard-globals
  var current-compile-options = CS.default-compile-options
  var extra-imports = CS.standard-imports
  var current-modules = modules
  var current-realm = realm
  var locator-cache = SD.make-mutable-string-dict()
  var current-interaction = 0
  var current-finder = make-finder()

  finder = lam(context, dep):
    if CS.is-dependency(dep):
      cases(Option) locator-cache.get-now(dep.arguments.first):
        | some(l) => CL.located(l, context)
        | none => current-finder(context, dep)
      end
    else:
      current-finder(context, dep)
    end
  end

  fun update-env(result, loc, cr) block:
    dep = CS.dependency("repl", [list: loc.uri()])

    globs-and-imports = filter-env-by-imports(cr.compile-env, loc, dep.key(), globals)
    globals := globs-and-imports.new-globals
    extra-imports := CS.extra-imports(
      globs-and-imports.new-extras +
      link(
        CS.extra-import(dep, "_", [list:], [list:]),
        extra-imports.imports))

    new-vals = for SD.fold-keys(vs from globals.values, provided-name from cr.provides.values):
      vs.set(provided-name, dep.key())
    end
    new-types = for SD.fold-keys(ts from globals.types, provided-name from cr.provides.aliases):
      ts.set(provided-name, dep.key())
    end
    globals := CS.globals(new-vals, new-types)

    locator-cache.set-now(loc.uri(), loc)
    current-realm := L.get-result-realm(result)

  end
  fun restart-interactions(defs-locator :: CL.Locator, options :: CS.CompileOptions) block:
    current-interaction := 0
    current-compile-options := options
    current-realm := realm
    locator-cache := SD.make-mutable-string-dict()
    current-modules := SD.make-mutable-string-dict()
    extra-imports := CS.standard-imports
    current-finder := make-finder()
    globals := defs-locator.get-globals()
    worklist = CL.compile-worklist(finder, defs-locator, compile-context)
    compiled = CL.compile-program-with(worklist, current-modules, current-compile-options)
    for SD.each-key-now(k from compiled.modules):
      current-modules.set-now(k, compiled.modules.get-value-now(k))
    end
    result = CL.run-program(worklist, compiled, current-realm, runtime, current-compile-options)
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

  fun run-interaction(repl-locator :: CL.Locator) block:
    worklist = CL.compile-worklist(finder, repl-locator, compile-context)
    compiled = CL.compile-program-with(worklist, current-modules, current-compile-options)
    for SD.each-key-now(k from compiled.modules) block:
      m = compiled.modules.get-value-now(k)
      current-modules.set-now(k, compiled.modules.get-value-now(k))
    end
    result = CL.run-program(worklist, compiled, current-realm, runtime, current-compile-options)
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

  fun make-interaction-locator(get-interactions) block:
    current-interaction := current-interaction + 1
    this-interaction = current-interaction
    uri = "interactions://" + num-to-string(this-interaction)
    var ast = nothing
    fun get-ast() block:
      when ast == nothing block:
        interactions = get-interactions()
        parsed = P.surface-parse(interactions, uri)
        ast := make-provide-for-repl(parsed)
      end
      ast
    end
    # Strip names from these, since they will be provided
    extras-now = CS.extra-imports(for lists.map(ei from extra-imports.imports):
      CS.extra-import(ei.dependency, "_", ei.values, ei.types)
    end)
    globals-now = globals
    {
      method needs-compile(self, provs): true end,
      method get-modified-time(self): 0 end,
      method get-options(self, options): options end,
      method get-native-modules(self): [list:] end,
      method get-module(self): CL.pyret-ast(get-ast()) end,
      method get-extra-imports(self): extras-now end,
      method get-dependencies(self):
        mod-deps = CL.get-dependencies(self.get-module(), self.uri())
        mod-deps + self.get-extra-imports().imports.map(_.dependency)
      end,
      method get-globals(self): globals-now end,
      method update-compile-context(self, ctxt): ctxt end,
      method uri(self): uri end,
      method name(self): "interactions" + num-to-string(this-interaction) end,
      method set-compiled(self, env, result): nothing end,
      method get-compiled(self): none end,
      method _equals(self, that, rec-eq): rec-eq(self.uri(), that.uri()) end
    }
  end

  fun make-definitions-locator(get-defs, shadow globals):
    var ast = nothing
    fun get-ast() block:
      when ast == nothing block:
        initial-definitions = get-defs()
        parsed = P.surface-parse(initial-definitions, "definitions://")
        provided = make-provide-for-repl-main(parsed, globals)
        ast := provided
      end
      ast
    end
    {
      method needs-compile(self, provs): true end,
      method get-modified-time(self): 0 end,
      method get-options(self, options): options end,
      method get-native-modules(self): [list:] end,
      method get-module(self): CL.pyret-ast(get-ast()) end,
      method get-extra-imports(self):
        CS.standard-imports
      end,
      method get-dependencies(self):
        CL.get-standard-dependencies(self.get-module(), self.uri())
      end,
      method get-globals(self): globals end,
      method uri(self): "definitions://" end,
      method name(self): "definitions" end,
      method set-compiled(self, env, result): nothing end,
      method get-compiled(self): none end,
      method _equals(self, that, rec-eq): rec-eq(self.uri(), that.uri()) end
    }
  end

  {
    restart-interactions: restart-interactions,
    make-interaction-locator: make-interaction-locator,
    make-definitions-locator: make-definitions-locator,
    run-interaction: run-interaction,
    runtime: runtime
  }
end
