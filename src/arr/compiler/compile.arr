#lang pyret

provide *
provide-types *
import file as F
import ast as A
import parse-pyret as PP
import "compiler/js-of-pyret.arr" as P
import "compiler/compile-structs.arr" as C
import "compiler/well-formed.arr" as W
import "compiler/ast-util.arr" as U
import "compiler/resolve-scope.arr" as R
import "compiler/desugar.arr" as D
import "compiler/desugar-post-tc.arr" as DP
import "compiler/type-check.arr" as T
import "compiler/desugar-check.arr" as CH
import "compiler/stepper.arr" as ST

data CompilationPhase:
  | start
  | phase(name :: String, result :: Any, prev :: CompilationPhase)
sharing:
  tolist(self):
    fun help(the-phase, acc):
      if is-start(the-phase): acc
      else: help(the-phase.prev, {name : the-phase.name, result : the-phase.result} ^ link(_, acc))
      end
    end
    help(self, empty)
  end
end

fun compile-js-ast(phases, ast, name, libs, options) -> CompilationPhase:
  var ret = phases
  ast-ended = U.append-nothing-if-necessary(ast)
  when options.collect-all:
    when is-some(ast-ended): ret := phase("Added nothing", ast-ended.value, ret) end
  end
  wf = W.check-well-formed(ast-ended.or-else(ast))
  when options.collect-all: ret := phase("Checked well-formedness", wf, ret) end
  checker = if options.check-mode: CH.desugar-check else: CH.desugar-no-checks;
  cases(C.CompileResult) wf:
    | ok(wf-ast) =>
      checked = checker(wf-ast)
      when options.collect-all:
        ret := phase(if options.check-mode: "Desugared (with checks)" else: "Desugared (skipping checks)" end,
          checked, ret)
      end
      shadow checked =
        # Tell the stepper where the actionable part of the program is.
        if options.show-steps: ST.wrap-body(checked)
        else: checked;
      scoped = R.desugar-scope(checked, libs)
      when options.collect-all: ret := phase("Desugared scope", scoped, ret) end
      named-result = R.resolve-names(scoped, libs)
      when options.collect-all: ret := phase("Resolved names", named-result, ret) end
      named-ast = named-result.ast
      named-errors = named-result.errors
      desugared = D.desugar(named-ast, libs)
      when options.collect-all: ret := phase("Fully desugared", desugared, ret) end
      type-checked =
        if options.type-check: T.type-check(desugared, libs)
        else: C.ok(desugared);
      when options.collect-all: ret := phase("Type Checked", type-checked, ret) end
      stepped =
        if options.show-steps: ST.stepify(type-checked)
        else: type-checked;
      cases(C.CompileResult) stepped:
        | ok(tc-ast) =>
          dp-ast = DP.desugar-post-tc(tc-ast, libs)
          cleaned = dp-ast.visit(U.merge-nested-blocks)
                          .visit(U.flatten-single-blocks)
                          .visit(U.link-list-visitor(libs))
                          .visit(U.letrec-visitor)
          when options.collect-all: ret := phase("Cleaned AST", cleaned, ret) end
          inlined = cleaned.visit(U.inline-lams)
          when options.collect-all: ret := phase("Inlined lambdas", inlined, ret) end
          any-errors = named-errors + U.check-unbound(libs, inlined) + U.bad-assignments(libs, inlined)
          if is-empty(any-errors):
            if options.collect-all: P.trace-make-compiled-pyret(ret, phase, inlined, libs)
            else: phase("Result", C.ok(P.make-compiled-pyret(inlined, libs)), ret)
            end
          else:
            if options.collect-all and options.ignore-unbound: P.trace-make-compiled-pyret(ret, phase, inlined, libs)
            else: phase("Result", C.err(any-errors), ret)
            end
          end
        | err(_) => phase("Result", type-checked, ret)
      end
    | err(_) => phase("Result", wf, ret)
  end
end

fun compile-js(trace, dialect, code, name, libs, options)
  -> CompilationPhase<C.CompileResult<P.CompiledCodePrinter, Any>>:
  var ret = trace
  ast = PP.parse-dialect(dialect, code, name)
  when options.collect-all: ret := phase("Parsed (" + dialect + " dialect)", ast, ret) end
  compile-js-ast(ret, ast, name, libs, options)
end

fun compile-runnable-js(dialect, code, name, libs, options) -> C.CompileResult<P.CompiledCodePrinter, Any>:
  compile-js(start, dialect, code, name, libs, options.{collect-all: false, ignore-unbound: false}).result.pyret-to-js-runnable()
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
  result = compile-js(start, code, name, libs, options.{collect-all: false, ignore-unbound: false}).result
  cases (C.CompileResult) result:
    | ok(comp) => C.ok(comp.pyret-to-js-standalone())
    | err(_) => result
  end
end

