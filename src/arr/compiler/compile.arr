#lang pyret

provide *
import file as F
import ast as A
import parse-pyret as PP
import "./js-of-pyret.arr" as P
import "./compile-structs.arr" as C
import "./well-formed.arr" as W
import "./ast-util.arr" as U
import "./desugar.arr" as D
import "./desugar-check.arr" as CH

fun compile-js(code, name, libs, options) -> C.CompileResult<P.CompiledCodePrinter, Any>:
  ast = PP.surface-parse(code, name)
  ast-ended = U.append-nothing-if-necessary(ast)
  wf = W.check-well-formed(ast-ended)
  cases(C.CompileResult) wf:
    | ok(wf-ast) =>
      checked = wf-ast.visit(CH.check-visitor)
      desugared = D.desugar(checked, libs)
      cleaned = desugared.visit(U.merge-nested-blocks)
                         .visit(U.flatten-single-blocks)
                         .visit(U.link-list-visitor(libs))
      unbound-ids = U.check-unbound(libs, cleaned)
      cases(List) unbound-ids:
        | empty => C.ok(P.make-compiled-pyret(cleaned, libs))
        | link(_, _) => C.err([C.unbound-ids(unbound-ids)])
      end
    | err(_) => wf
  end
end

fun compile-runnable-js(code, name, libs, options) -> C.CompileResult<P.CompiledCodePrinter, Any>:
  compile-js(code, name, libs, options).pyret-to-js-runnable()
end

fun compile-runnable-js-file(js-file, libs, options) -> C.CompileResult<P.CompiledCodePrinter, Any>:
  code = F.file-to-string(js-file)
  compile-runnable-js(code, js-file, libs, options)
end

fun compile-standalone-js-file(js-file, libs, options) -> C.CompileResult<P.CompiledCodePrinter, Any>:
  code = F.file-to-string(js-file)
  compile-standalone-js(code, js-file, libs, options)
end

fun compile-standalone-js(code, name, libs, options) -> C.CompileResult<String, Any>:
  result = compile-js(code, name, libs, options)
  cases (C.CompileResult) result:
    | ok(comp) => C.ok(comp.pyret-to-js-standalone())
    | err(_) => result
  end
end

