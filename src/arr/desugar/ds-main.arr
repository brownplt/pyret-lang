provide {
    desugar: desugar,
    resugar: resugar
} end

import ast as AST
import ast-visitors as AV
import file as F
import file("conversion-visitor.arr") as CONV
import file("ds-structs.arr") as S
import file("ds-sugar.arr") as DS
import file("ds-parse.arr") as P

desugaring-rules = block:
  file = F.input-file("src/arr/desugar/pyret.sugar")
  ds-rules = P.parse-ds-rules(file.read-file())
  file.close-file()
  ds-rules
end

fun desugar(e :: AST.Program) -> AST.Program block:
  e.visit(CONV.ast-to-term-visitor)
    ^ DS.desugar(desugaring-rules, _)
    ^ CONV.term-to-ast
end

fun resugar(e :: AST.Program) -> Option<AST.Program>:
  # FILL
  e
end
