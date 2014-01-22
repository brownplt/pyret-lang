#lang pyret

provide *
import file as F
import ast as A
import "js-of-pyret.arr" as P
import "compile-structs.arr" as C

fun compile-js(code, name, libs, options):
  ast = A.surface-parse(code, name)
  P.make-compiled-pyret(ast, libs)
end

fun compile-runnable-js(code, name, libs, options):
  compile-js(code, name, libs, options).pyret-to-js-runnable()
end

fun compile-runnable-js-file(js-file, libs, options):
  code = F.file-to-string(js-file)
  compile-runnable-js(code, js-file, libs, options)
end

fun compile-standalone-js-file(js-file, libs, options):
  code = F.file-to-string(js-file)
  compile-standalone-js(code, js-file, libs, options)
end

fun compile-standalone-js(code, name, libs, options) -> C.CompileResult:
  compile-js(code, name, libs, options).pyret-to-js-standalone()
end

