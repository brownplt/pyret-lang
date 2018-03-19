

import ast as AST
import file("term-visitor.arr")
import file("ds-structs.arr") as S
import file("ds-sugar.arr") as DS



fun desugar(rules :: List<DsRule>, e :: Term) -> Term:
fun resugar(e :: Term) -> Option<Term>:
