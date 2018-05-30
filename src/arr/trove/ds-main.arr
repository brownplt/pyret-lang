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
import pyret-sugar-rules as PST
include debugging
import ds-metafunctions as _

desugaring-rules = block:
  ds-rules = P.parse-ds-rules(PST.sugar-rules)
  ds-rules
end

stepify-rules = block:
  file = F.input-file("/home/preston/pyret-lang/src/arr/trove/stepify.sugar")
  step-rules = P.parse-ds-rules(file.read-file())
  file.close-file()
  step-rules
end

fun desugar-expr(e :: AST.Expr) -> AST.Expr:
  e.visit(CONV.ast-to-term-visitor)
    ^ DS.desugar(desugaring-rules, _)
    ^ CONV.term-to-ast
end

fun desugar(e :: AST.Program, trace :: Boolean) -> AST.Program block:
  rules = if trace:
    ST.rules-union(desugaring-rules, stepify-rules)
  else:
    desugaring-rules
  end
  e.visit(CONV.ast-to-term-visitor)
    ^ push-time("desugar")
    ^ DS.desugar(rules, _)
    ^ pop-time
    ^ CONV.term-to-ast
end

fun resugar(e :: AST.Program) -> Option<AST.Program>:
  # FILL
  e
end
