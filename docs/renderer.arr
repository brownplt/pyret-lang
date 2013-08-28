#lang pyret

import "sourceAST.arr" as SA
import ast as A
provide { get-pretty-str: get-pretty-str } end

fun get-pretty-str(raw-ast):
  this-ast = A.to-pyret(raw-ast)
  printing-ast = SA.from-ast(this-ast)
  printing-ast.tosource().pretty(80)
end

