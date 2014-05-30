#lang pyret

provide *
import ast as A
import parse-pyret as PP
import "compiler/js-of-pyret.arr" as P
import "compiler/compile-structs.arr" as C
import "compiler/well-formed.arr" as W
import "compiler/ast-util.arr" as U
import "compiler/resolve-scope.arr" as R
import "compiler/desugar.arr" as D
import "compiler/desugar-check.arr" as CH

fun compile-js(dialect, code, name, libs, safe-stack):
  ast = PP.surface-parse(code, name)
  ast-ended = U.append-nothing-if-necessary(ast)
  wf = W.check-well-formed(ast-ended)
  cases(C.CompileResult) wf:
    | ok(wf-ast) =>
      checked = CH.desugar-check(wf-ast)
      scoped = R.desugar-scope(checked, libs)
      named = R.resolve-names(scoped, libs).ast
      desugared = D.desugar(named, libs)
      cleaned = desugared.visit(U.merge-nested-blocks)
                     .visit(U.flatten-single-blocks)
                     .visit(U.link-list-visitor(libs))
      any-errors = U.check-unbound(libs, cleaned)
      if is-empty(any-errors):
        compiled = if safe-stack:
          P.make-compiled-pyret(cleaned, libs)
        else:
          P.make-unsafe-compiled-pyret(cleaned, libs)
        end
        C.ok(compiled)
      else: C.err(any-errors)
      end
    | err(_) => wf
  end
end

fun compile-runnable-js(dialect, code, name, libs, options):
  compile-js(dialect, code, name, libs, options).pyret-to-js-runnable()
end

fun compile-standalone-js(dialect, code, name, libs, options):
  compile-js(dialect, code, name, libs, options).pyret-to-js-standalone()
end

