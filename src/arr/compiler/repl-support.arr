#;lang pyret

provide *
import srcloc as S
import "compiler/compile-structs.arr" as C
import ast as A
import error as E

fun drop-module-bindings(env :: C.CompileEnvironment):
  fun negate(f): lam(x): not(f(x));;
  C.compile-env(
    env.bindings.filter(negate(C.is-module-bindings)),
    env.types.filter(negate(C.is-type-module-bindings))
  )
where:
  drop-module-bindings(C.compile-env([list:
    C.builtin-id("x"),
    C.module-bindings("list", [list:])
  ], [list:
    C.type-id("Number"),
    C.type-module-bindings("lists", [list: "List"])
  ])) is
    C.compile-env([list: C.builtin-id("x")], [list: C.type-id("Number")])

end

fun add-global-binding(env :: C.CompileEnvironment, name :: String):
  cases(C.CompileEnvironment) env:
    | compile-env(bindings, types) =>
      C.compile-env(link(C.builtin-id(name), bindings), types)
  end
end

fun add-global-type-binding(env :: C.CompileEnvironment, name :: String):
  cases(C.CompileEnvironment) env:
    | compile-env(bindings, types) =>
      C.compile-env(bindings, link(C.type-id(name), types))
  end
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

fun make-safe-imports(imps):
  imps.each(lam(i):
    cases(A.ImportType) i.file:
      | s-special-import(_, _, _) => nothing
      | s-file-import(l, f) =>
        raise(E.module-load-failure([list: f]))
      | s-const-import(l, m) =>
        when not(ok-imports.member(i.file.mod)):
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
      | else => raise("Unknown import type: " + torepr(imp))
    end
  end
  ids-plus-import-names = import-names + ids
  type-ids = A.block-type-ids(body)
  import-type-names = for fold(names from empty, imp from safe-imports):
    cases(A.Import) imp:
      | s-import(_, _, name) => link(name, names)
      | s-import-fields(_, imp-names, _) => names
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

fun make-provide-for-repl-main(p :: A.Program, compile-env :: C.CompileEnvironment):
  doc: "Make the program simply provide all (for the repl)"
  cases(A.Program) p:
    | s-program(l, _, _, imports, body) =>
      defined-ids = get-defined-ids(p, imports, body)
      repl-provide = for map(n from defined-ids.ids): df(l, n) end
      repl-type-provide = for map(n from defined-ids.type-ids): af(l, n) end
      env-provide = for fold(flds from repl-provide, elt from compile-env.bindings):
        cases(C.CompileBinding) elt:
          | builtin-id(name) =>
            shadow l = S.builtin(name)
            link(df(l, A.s-name(l, name)), flds)
          | module-bindings(name, fields) =>
            shadow l = S.builtin(name)
            [list: df(l, A.s-name(l, name))] +
              for map(f from fields):
                df(l, A.s-name(l, f))
              end +
              flds
        end
      end
      env-type-provide = for fold(flds from repl-type-provide, elt from compile-env.types):
        cases(C.CompileTypeBinding) elt:
          | type-id(name) =>
            shadow l = S.builtin(name)
            link(af(l, A.s-name(l, name)), flds)
          | type-module-bindings(name, fields) =>
            shadow l = S.builtin(name)
            [list: af(l, A.s-name(l, name))] +
              for map(f from fields):
                af(l, A.s-name(l, f))
              end +
              flds
        end
      end
      A.s-program(l,
          A.s-provide(l, A.s-obj(l, env-provide)),
          A.s-provide-types(l, env-type-provide),
          defined-ids.imports,
          body)
  end
end

