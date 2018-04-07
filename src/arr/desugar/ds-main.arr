provide {
    desugar: desugar,
    resugar: resugar,
    desugar-expr: desugar-expr
} end

import ast as AST
import file as F
import file("conversion-visitor.arr") as CONV
import file("ds-desugar.arr") as DS
import file("ds-resugar.arr") as RS
import file("ds-parse.arr") as P
import file("ds-structs.arr") as ST
include file("debugging.arr")
import file("ds-metafunctions.arr") as _

nothing ^ push-time("reading")
desugaring-rules = block:
  file = F.input-file("src/arr/desugar/pyret.sugar")
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

check:
  rules = P.parse-ds-rules(
    ```
    sugar or:
    | (or a:Expr b) => (fresh [x] (let (bind x a) (if x x b)))
    end
    sugar bind:
    | (bind x a) => {bind x a}
    end
    sugar let:
    | (let {bind x a} body) => (apply (lambda x body) a)
    end
    ```)

  e = P.parse-ast("(or p q)")
  RS.resugar(DS.desugar(rules, e)) is some(e)

  # e1 = parse-ast("(bind 1 (+))")
  # resugar(desugar(rules, e1)) is some(e1)

  # e2 = parse-ast("(let (bind x (+ 1 2)) (- x 3))")
  # resugar(desugar(rules, e2)) is some(e2)
end
