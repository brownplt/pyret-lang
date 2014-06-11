#lang pyret

provide *
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
  "either",
  "sets",
  "arrays",
  "contracts",
  "ast",
  "parse-pyret",
  "pprint",
  "srcloc",
  "format"
]
fun make-safe-imports(imps):
  imps.each(lam(i):
    cases(A.ImportType) i.file:
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

fun make-provide-for-repl(p :: A.Program):
  cases(A.Program) p:
    | s-program(l, _, _, imports, body) =>
      fun df(name):
        A.s-data-field(l, A.s-str(l, name.toname()), A.s-id(l, name))
      end
      fun af(name):
        A.a-field(l, name.toname(), A.a-name(l, name))
      end
      ids = A.toplevel-ids(p)
      safe-imports = make-safe-imports(imports)
      ids-plus-import-names = safe-imports.map(_.name) + ids
      repl-provide = for map(id from ids-plus-import-names):
        df(id)
      end
      type-ids = A.block-type-ids(body)
      type-ids-plus-import-names = safe-imports.map(_.name) + type-ids.map(_.name)
      repl-type-provide = for map(id from type-ids-plus-import-names):
        af(id)
      end
      
      A.s-program(l,
          A.s-provide(l, A.s-obj(l, repl-provide)),
          A.s-provide-types(l, repl-type-provide),
          safe-imports,
          body)
  end
end

fun make-provide-for-repl-main(p :: A.Program, compile-env :: C.CompileEnvironment):
  doc: "Make the program simply provide all (for the repl)"
  cases(A.Program) p:
    | s-program(l, _, _, imports, body) =>
      fun df(name):
        A.s-data-field(l, A.s-str(l, name.toname()), A.s-id(l, name))
      end
      fun af(name):
        A.a-field(l, name.toname(), A.a-name(l, name))
      end
      safe-imports = make-safe-imports(imports)
      ids = A.toplevel-ids(p)
      ids-plus-import-names = safe-imports.map(_.name) + ids
      repl-provide = for map(id from ids-plus-import-names):
        df(id)
      end
      env-provide = for fold(flds from repl-provide, elt from compile-env.bindings):
        cases(C.CompileBinding) elt:
          | builtin-id(name) => link(df(A.s-name(l, name)), flds)
          | module-bindings(name, fields) =>
            [list: df(A.s-name(l, name))] +
              for map(f from fields):
                df(A.s-name(l, f))
              end +
              flds
        end
      end
      type-ids = A.block-type-ids(body)
      type-ids-plus-import-names = safe-imports.map(_.name) + type-ids.map(_.name)
      repl-type-provide = for map(id from type-ids-plus-import-names):
        af(id)
      end
      env-type-provide = for fold(flds from repl-type-provide, elt from compile-env.types):
        cases(C.CompileTypeBinding) elt:
          | type-id(name) => link(af(A.s-name(l, name)), flds)
          | type-module-bindings(name, fields) =>
            [list: af(A.s-name(l, name))] +
              for map(f from fields):
                af(A.s-name(l, f))
              end +
              flds
        end
      end
      
      A.s-program(l,
          A.s-provide(l, A.s-obj(l, env-provide)),
          A.s-provide-types(l, env-type-provide),
          safe-imports,
          body)
  end
end

