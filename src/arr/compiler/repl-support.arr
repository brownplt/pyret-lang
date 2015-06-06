#;lang pyret

provide *
import srcloc as S
import "compiler/compile-structs.arr" as C
import "compiler/compile-lib.arr" as CL
import "compiler/locators/builtin.arr" as B
import ast as A
import error as E
import parse-pyret as P
import string-dict as SD
import namespace-lib as N


fun add-global-binding(env :: C.CompileEnvironment, name :: String):
  C.compile-env(
    C.globals(env.globals.values.set(name, C.v-just-there), env.globals.types),
    env.mods)
end

fun add-global-type-binding(env :: C.CompileEnvironment, name :: String):
  C.compile-env(
    C.globals(env.globals.values, env.globals.types.set(name, C.t-just-there)),
    env.mods)
end

ok-imports = [list:
  "world",
  "image",
  "image-structs",
  "string-dict",
  "checkers",
  "lists",
  "error",
  "option",
  "pick",
  "either",
  "sets",
  "arrays",
  "contracts",
  "ast",
  "parse-pyret",
  "s-exp",
  "s-exp-structs",
  "pprint",
  "srcloc",
  "format",
  "equality",
  "valueskeleton"
]

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

fun make-safe-imports(imps):
  imps.each(lam(i):
    d = get-imp-dependency(i)
    cases(A.ImportType) d:
      | s-special-import(_, _, _) => nothing
      | s-file-import(l, f) =>
        raise(E.module-load-failure([list: f]))
      | s-const-import(l, m) =>
        when not(ok-imports.member(m)):
          raise(E.module-load-failure([list: m]))
        end
    end
  end)
  imps
end

fun wrap-for-special-import(p :: A.Program):
  cases(A.Program) p:
    | s-program(l, _, _, imports, _) =>
      make-safe-imports(imports)
      p
  end
end

fun get-defined-ids(p, imports, body):
  ids = A.toplevel-ids(p)
  safe-imports = make-safe-imports(imports)
  import-names = for fold(names from empty, imp from safe-imports):
    cases(A.Import) imp:
      | s-import(_, _, name) => link(name, names)
      | s-import-fields(_, imp-names, _) => names + imp-names
      | s-include(_, _) => names
      | else => raise("Unknown import type: " + torepr(imp))
    end
  end
  ids-plus-import-names = import-names + ids
  type-ids = A.block-type-ids(body)
  import-type-names = for fold(names from empty, imp from safe-imports):
    cases(A.Import) imp:
      | s-import(_, _, name) => link(name, names)
      | s-import-fields(_, imp-names, _) => names
      | s-include(_, _) => names
      | else => raise("Unknown import type: " + torepr(imp))
    end
  end
  type-ids-plus-import-names = import-type-names + type-ids.map(_.name)
  {
    imports: safe-imports.filter(lam(id): not(A.is-s-underscore(id)) end),
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

fun make-provide-for-repl-main-env(p :: A.Program, env :: C.CompileEnvironment):
  make-provide-for-repl-main(p, env.globals)
end

fun make-provide-for-repl-main(p :: A.Program, globals :: C.Globals):
  doc: "Make the program simply provide all (for the repl)"
  cases(A.Program) p:
    | s-program(l, _, _, imports, body) =>
      defined-ids = get-defined-ids(p, imports, body)
      repl-provide = for map(n from defined-ids.ids): df(l, n) end
      repl-type-provide = for map(n from defined-ids.type-ids): af(l, n) end
      env-provide = for fold(flds from repl-provide, name from globals.values.keys-list()):
        link(df(l, A.s-name(l, name)), flds)
      end
      env-type-provide = for fold(flds from repl-type-provide, name from globals.types.keys-list()):
        link(af(l, A.s-name(l, name)), flds)
      end
      A.s-program(l,
          A.s-provide(l, A.s-obj(l, env-provide)),
          A.s-provide-types(l, env-type-provide),
          defined-ids.imports,
          body)
  end
end

fun make-repl-definitions-locator(name, uri, get-definitions, globals):
  fun get-ast():
    initial-definitions = get-definitions()
    parsed = P.surface-parse(initial-definitions, name)
    make-provide-for-repl-main(parsed, globals)
  end
  {
    needs-compile(self, provs): true end,
    get-module(self): CL.pyret-ast(get-ast()) end,
    get-extra-imports(self):
      C.standard-imports
    end,
    get-dependencies(self):
      CL.get-standard-dependencies(self.get-module(), self.uri())
    end,
    get-provides(self): CL.get-provides(self.get-module(), self.uri()) end,
    get-globals(self): globals end,
    get-namespace(self, runtime): N.make-base-namespace(runtime) end,
    update-compile-context(self, ctxt): ctxt end,
    uri(self): uri end,
    name(self): name end,
    set-compiled(self, ctxt, provs): nothing end,
    get-compiled(self): none end,
    _equals(self, that, rec-eq): rec-eq(self.uri(), that.uri()) end
  }
end

fun make-repl-interaction-locator(name, uri, get-interactions, repl):
  fun get-ast():
    interactions = get-interactions()
    parsed = P.surface-parse(interactions, name)
    make-provide-for-repl(parsed)
  end
  {
    needs-compile(self, provs): true end,
    get-module(self): CL.pyret-ast(get-ast()) end,
    get-extra-imports(self):
      C.standard-imports
    end,
    get-dependencies(self):
      CL.get-standard-dependencies(self.get-module(), self.uri())
    end,
    get-provides(self): CL.get-provides(self.get-module(), self.uri()) end,
    get-globals(self): repl.get-current-globals() end,
    get-namespace(self, runtime): repl.get-current-namespace() end,
    update-compile-context(self, ctxt): ctxt end,
    uri(self): uri end,
    name(self): name end,
    set-compiled(self, ctxt, provs): nothing end,
    get-compiled(self): none end,
    _equals(self, that, rec-eq): rec-eq(self.uri(), that.uri()) end
  }
end

fun make-definitions-finder(import-types :: SD.StringDict):
  fun definitions-finder(context, dep):
    l = cases(C.Dependency) dep:
      | builtin(name) => B.make-builtin-locator(name)
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

