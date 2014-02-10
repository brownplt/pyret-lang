#lang pyret

provide *
import ast as A
import parse-pyret as PP
import "./js-of-pyret.arr" as P
import "./compile-structs.arr" as C

fun compile-js(code, name, libs, options):
  ast = PP.surface-parse(code, name)
  P.make-compiled-pyret(ast, libs)
end

fun compile-runnable-js(code, name, libs, options):
  compile-js(code, name, libs, options).pyret-to-js-runnable()
end

fun compile-standalone-js(code, name, libs, options):
  compile-js(code, name, libs, options).pyret-to-js-standalone()
end

