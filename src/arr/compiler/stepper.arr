#lang pyret

provide {
  stepify: stepify,
  stepify-prog: stepify-prog
} end

import ast as A
import "compiler/compile-structs.arr" as C

# NOTE:
# desugar-post-tc.arr has a comment about invariants

fun stepify(res :: C.CompileResult<A.Program, Any>)
  -> C.CompileResult<A.Program, Any>:
  cases(C.CompileResult<A.Program, Any>) res:
    | err(err) => C.err(err)
    | ok(prog) => C.ok(stepify-prog(prog))
  end
end

fun gid(l, id):
  A.s-id(l, A.s-global(id))
end

fun stepify-prog(prog :: A.Program) -> A.Program:
  cases(A.Program) prog:
    | s-program(l, prov, prov-ty, imp, block) =>
      A.s-program(l, prov, prov-ty, imp, stepify-expr(block))
  end
end

fun stepify-expr(expr :: A.Expr) -> A.Expr:
  l = expr.l
  A.s-block(l, [list:
      A.s-app(l, gid(l, "print"), [list: A.s-str(l, "Gonna step")]),
      expr])
end
