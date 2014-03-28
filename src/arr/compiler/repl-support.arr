#lang pyret

provide *
import "./compile-structs.arr" as C
import ast as A



fun add-global-binding(env :: C.CompileEnvironment, name :: String):
  cases(C.CompileEnvironment) env:
    | compile-env(bindings) =>
      C.compile-env(link(C.builtin-id(name), bindings))
  end
end

fun make-provide-all(p :: A.Program):
  doc: "Make the program simply provide all (for the repl)"
  fun not-provide(h):
    cases(A.Header) h:
      | s_provide_all(_) => false
      | s_provide(_, _) => false
      | s_import(_, _, _) => true
    end
  end
  cases(A.Program) p:
    | s_program(l, headers, body) =>
      A.s_program(l, link(A.s_provide_all(l), headers.filter(not-provide)), body)
  end
end

