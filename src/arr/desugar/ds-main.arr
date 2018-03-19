provide {
    desugar: desugar,
    resugar: resugar
} end

import ast as AST
import file as F
#import file("term-visitor.arr")
import file("ds-structs.arr") as S
import file("ds-sugar.arr") as DS
import file("ds-parse.arr") as P

desugaring-rules = block:
  file = F.input-file("src/arr/desugar/pyret.sugar")
  ds-rules = P.parse-ds-rules(file.read-file())
  file.close-file()
  ds-rules
end

fun desugar(e :: AST.Program) -> AST.Program:
  # FILL
  e
end

fun resugar(e :: AST.Program) -> Option<AST.Program>:
  # FILL
  e
end
