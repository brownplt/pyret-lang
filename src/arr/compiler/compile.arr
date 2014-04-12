#lang pyret

provide *
import file as F
import ast as A
import parse-pyret as PP
import "./js-of-pyret.arr" as P
import "./compile-structs.arr" as C
import "./well-formed.arr" as W
import "./ast-util.arr" as U
import "./resolve-scope.arr" as R
import "./desugar.arr" as D
import "./desugar-check.arr" as CH

fun compile-js-ast(ast, name, libs, options):
  ast-ended = U.append-nothing-if-necessary(ast)
  wf = W.check-well-formed(ast-ended)
  checker = if options.check-mode: CH.desugar-check else: CH.desugar-no-checks;
  cases(C.CompileResult) wf:
    | ok(wf-ast) =>
      checked = checker(wf-ast)
      scoped = R.desugar-scope(checked, libs)
      named-result = R.resolve-names(scoped, libs)
      named-ast = named-result.ast
      named-shadow-errors = named-result.shadowed
      desugared = D.desugar(named-ast, libs)
      cleaned = desugared.visit(U.merge-nested-blocks)
                     .visit(U.flatten-single-blocks)
                     .visit(U.link-list-visitor(libs))
      any-errors = named-shadow-errors + U.check-unbound(libs, cleaned) + U.bad-assignments(libs, cleaned)
      if is-empty(any-errors): C.ok(P.make-compiled-pyret(cleaned, libs))
      else: C.err(any-errors)
      end
    | err(_) => wf
  end
end

fun compile-js(dialect, code, name, libs, options) -> C.CompileResult<P.CompiledCodePrinter, Any>:
  ast = PP.parse-dialect(dialect, code, name)
  compile-js-ast(ast, name, libs, options)
end

fun compile-runnable-js(dialect, code, name, libs, options) -> C.CompileResult<P.CompiledCodePrinter, Any>:
  compile-js(dialect, code, name, libs, options).pyret-to-js-runnable()
end

fun compile-runnable-js-file(dialect, js-file, libs, options) -> C.CompileResult<P.CompiledCodePrinter, Any>:
  code = F.file-to-string(js-file)
  compile-runnable-js(dialect, code, js-file, libs, options)
end

fun compile-standalone-js-file(dialect, js-file, libs, options) -> C.CompileResult<P.CompiledCodePrinter, Any>:
  code = F.file-to-string(js-file)
  compile-standalone-js(dialect, code, js-file, libs, options)
end

fun compile-standalone-js(code, name, libs, options) -> C.CompileResult<String, Any>:
  result = compile-js(code, name, libs, options)
  cases (C.CompileResult) result:
    | ok(comp) => C.ok(comp.pyret-to-js-standalone())
    | err(_) => result
  end
end

