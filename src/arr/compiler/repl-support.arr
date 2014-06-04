#lang pyret

provide *
import "compiler/compile-structs.arr" as C
import ast as A



fun add-global-binding(env :: C.CompileEnvironment, name :: String):
  cases(C.CompileEnvironment) env:
    | compile-env(bindings, type-bindings) =>
      C.compile-env(link(C.builtin-id(name), bindings), type-bindings)
  end
end

fun add-global-type-binding(env :: C.CompileEnvironment, name :: String):
  cases(C.CompileEnvironment) env:
    | compile-env(bindings, type-bindings) =>
      C.compile-env(bindings, link(C.type-id(name), type-bindings))
  end
end

fun make-provide-all(p :: A.Program):
  doc: "Make the program simply provide all (for the repl)"
  cases(A.Program) p:
    | s-program(l, _, _, imports, body) =>
      ids = A.toplevel-ids(p)
      ids-plus-import-names = imports.map(_.name) + ids
      repl-provide = for map(id from ids-plus-import-names):
        A.s-data-field(l, A.s-str(l, id.toname()), A.s-id(l, id))
      end
      type-ids = A.block-type-ids(body)
      type-ids-plus-import-names = imports.map(_.name) + type-ids.map(_.name)
      repl-type-provide = for map(id from type-ids-plus-import-names):
        A.a-field(l, id.toname(), A.a-name(l, id))
      end
      
      A.s-program(l,
          A.s-provide(l, A.s-obj(l, repl-provide)),
          A.s-provide-types(l, repl-type-provide),
          imports,
          body)
  end
end

