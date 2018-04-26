provide {
    desugar: desugar,
    resugar: resugar,
    desugar-expr: desugar-expr
} end

import global as _
include option
import ast as AST
import file as F
import conversion-visitor as CONV
import ds-desugar as DS
import ds-resugar as RS
import ds-parse as P
import ds-structs as ST
include debugging
import ds-metafunctions as _

nothing ^ push-time("reading")
desugaring-rules = block:
  file = F.input-file("src/arr/trove/pyret.sugar")
  ds-rules = P.parse-ds-rules(file.read-file())
  file.close-file()
  ds-rules
end ^ pop-time

fun desugar-expr(e :: AST.Expr) -> AST.Expr:
  e.visit(CONV.ast-to-term-visitor)
    ^ DS.desugar(desugaring-rules, _)
    ^ CONV.term-to-ast
end

fun desugar(e :: AST.Program) -> AST.Program block:
  e.visit(CONV.ast-to-term-visitor)
    ^ push-time("desugar")
    ^ DS.desugar(desugaring-rules, _)
    #^ ST.strip-tags
    #^ my-print("after desugar")
    ^ pop-time
    ^ CONV.term-to-ast
end

fun resugar(e :: AST.Program) -> Option<AST.Program>:
  # FILL
  e
end
