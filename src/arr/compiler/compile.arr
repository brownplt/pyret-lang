#lang pyret

provide *
import file as F
import ast as A
import parse-pyret as PP
import "./js-of-pyret.arr" as P
import "./compile-structs.arr" as C
import "./well-formed.arr" as W

fun compile-js(code, name, libs, options) -> C.CompileResult<P.CompiledCodePrinter, Any>:
  ast = PP.surface-parse(code, name)
  wf = W.check-well-formed(ast)
  cases(C.CompileResult) wf:
    | ok(checked-ast) => C.ok(P.make-compiled-pyret(checked-ast, libs))
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
    | ok(comp) => comp.pyret-to-js-standalone()
    | err(_) => result
  end
end

