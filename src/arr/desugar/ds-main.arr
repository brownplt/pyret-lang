provide {
    desugar: desugar,
    resugar: resugar,
    desugar-expr: desugar-expr
} end

import ast as AST
import file as F
import file("conversion-visitor.arr") as CONV
import file("ds-sugar.arr") as DS
import file("ds-parse.arr") as P
include file("debugging.arr")
import file("ds-metafunctions.arr") as _

# nothing ^ push-time("reading")
desugaring-rules = block:
  file = F.input-file("src/arr/desugar/pyret.sugar")
  ds-rules = P.parse-ds-rules(file.read-file())
  file.close-file()
  ds-rules
end # ^ pop-time

fun desugar-expr(e :: AST.Expr) -> AST.Expr:
  e.visit(CONV.ast-to-term-visitor)
    ^ DS.desugar(desugaring-rules, _)
    ^ CONV.term-to-ast
end

fun desugar(e :: AST.Program) -> AST.Program block:
  #nothing ^ push-time("to term")
  e.visit(CONV.ast-to-term-visitor)
    #^ pop-time
    #^ push-time("desugar")
    #^ push-time("subdesugar")
    ^ DS.desugar(desugaring-rules, _)
    #^ pop-time
    #^ pop-time
    #^ push-time("to ast")
    ^ CONV.term-to-ast
    #^ pop-time
end

fun resugar(e :: AST.Program) -> Option<AST.Program>:
  # FILL
  e
end
