#lang pyret

provide { get-pretty-str: get-pretty-str } end
import ast as A

fun get-pretty-str(raw-ast):
  this-ast = A.to-pyret(raw-ast)
  this-ast.tosource().pretty(80)
end

