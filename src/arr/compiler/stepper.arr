#lang pyret

provide {
  stepify: stepify-compile-result,
  stepify-prog: stepify-prog
} end

import ast as A
import "compiler/compile-structs.arr" as C

fun gid(l, id):
  A.s-id(l, A.s-global(id))
end

fun nid(l, id):
  A.s-id(l, A.s-name(l, id))
end

fun constant(l, name :: String):
  #R.node(name, [list:])
  A.s-app(l, A.s-dot(l, gid(l, "R"), "node"),
      [list: A.s-str(l, name), gid(l, "_empty")])
end

adorn-visitor = A.default-map-visitor.{
  s-bool(self, l, bool):
    if bool:
      constant(l, "true")
    else:
      constant(l, "false")
    end
  end
}

fun adorn(expr):
  expr.visit(adorn-visitor)
end

fun step(l, adorned, ast):
  A.s-block(l, [list:
      A.s-app(l, gid(l, "print"), [list: adorned]),
      ast])
end

stepify-visitor = A.default-map-visitor.{
  s-bool(self, l, bool):
    t = A.s-bool(l, bool)
    step(l, adorn(t), t)
  end
}

fun stepify(expr):
  expr.visit(stepify-visitor)
end

fun stepify-compile-result(res :: C.CompileResult<A.Program, Any>)
  -> C.CompileResult<A.Program, Any>:
  cases(C.CompileResult<A.Program, Any>) res:
    | err(err) => C.err(err)
    | ok(prog) => C.ok(stepify-prog(prog))
  end
end

fun stepify-prog(prog :: A.Program) -> A.Program:
  cases(A.Program) prog:
    | s-program(l, prov, prov-ty, imp, block) =>
      A.s-program(l, prov, prov-ty, imp, stepify-expr(block))
  end
end

fun stepify-expr(expr :: A.Expr) -> A.Expr:
  doc: ```Translate an expression into another one that does the same thing
  but shows its evaluation steps.
  Precondition: The expression must be desugared, as described in
                desugar-post-tc in desugar-post-tc.arr.```
  # - contains no s-var, s-fun, s-data, s-check, or s-check-test
  # - contains no s-provide in headers
  # - all where blocks are none
  # - contains no s-name (e.g. call resolve-names first)
  # - contains no s-for, s-if, s-op, s-method-field,
  #               s-not, s-when, s-if-pipe, s-paren
  # - contains no s-underscore in expression position (but it may
  #   appear in binding positions as in s-let-bind, s-letrec-bind)
  
  l = expr.l
  A.s-block(l, [list:
      A.s-app(l, gid(l, "print"), [list: A.s-str(l, "Gonna step")]),
      A.s-app(l, gid(l, "print"), [list: gid(l, "_node")]),
      stepify(expr)])
end
