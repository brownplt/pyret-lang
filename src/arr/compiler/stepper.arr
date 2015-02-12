#lang pyret

provide {
  stepify: stepify-compile-result,
  stepify-prog: stepify-prog,
  wrap-body: wrap-body
} end

import ast as A
import "compiler/compile-structs.arr" as C
import convert as CV
import srcloc as SL

dummy-loc = SL.builtin("dummy location")

fun gid(l, id):
  A.s-id(l, A.s-global(id))
end

fun nid(l, id):
  A.s-id(l, A.s-name(l, id))
end

fun ast-srcloc(l):
  A.s-prim-app(l, "makeSrcloc", [list: A.s-srcloc(l, l)])
end

fun ast-list(lst):
  cases(List) lst:
    | empty =>
      gid(dummy-loc, "_empty")
    | link(first, rest) =>
      A.s-app(first.l, gid(first.l, "_link"), [list: first, ast-list(rest)])
  end
end

fun bool-value(l, bool):
  A.s-app(l, gid(l, "_value"), [list: A.s-bool(l, bool)])
end

fun str-value(l, str):
  A.s-app(l, gid(l, "_value"), [list: A.s-str(l, str)])
end

fun num-value(l, num):
  A.s-app(l, gid(l, "_value"), [list: A.s-num(l, num)])
end

fun node(l, name, childs):
  A.s-app(l, gid(l, "_node"),
    [list: A.s-str(l, name), ast-srcloc(l), ast-list(childs)])
end

fun BLOCK(l, stmts):     A.s-block(l, stmts) end
fun APP0(l, func):       A.s-app(l, func, [list:]) end
fun APP1(l, func, arg):  A.s-app(l, func, [list: arg]) end
fun DOT(l, expr, field): A.s-dot(l, expr, field) end

fun NODE1(l, name, child): node(l, name, [list: child]) end

fun CALL0(l, expr, field):      APP0(l, DOT(l, expr, field)) end
fun CALL1(l, expr, field, arg): APP1(l, DOT(l, expr, field), arg) end
fun PRINT(l, arg):              APP1(l, gid(l, "print"), arg) end
fun TO_AST(l, arg):             APP1(l, gid(l, "_to-ast"), arg) end

fun PRETTY_AST(l, ast):
  lines = CALL1(l, CALL0(l, ast, "tosource"), "pretty", A.s-num(l, 80))
  CALL1(l, lines, "join-str", A.s-str(l, "\n"))
end

fun step(l, adorned, ast):
  BLOCK(l, [list:
      PRINT(l, PRETTY_AST(l, TO_AST(l, adorned))),
      ast])
end

adorn-visitor = A.default-map-visitor.{
  s-bool(self, l, bool): NODE1(l, "s-bool", bool-value(l, bool)) end,
  s-num(self, l, num):   NODE1(l, "s-num", num-value(l, num))    end,
  s-str(self, l, str):   NODE1(l, "s-str", str-value(l, str))    end
}

fun adorn(expr):
  expr.visit(adorn-visitor)
end

stepify-visitor = A.default-map-visitor.{
  s-bool(self, l, bool):
    t = A.s-bool(l, bool)
    step(l, adorn(t), t)
  end,
  s-num(self, l, num):
    t = A.s-num(l, num)
    step(l, adorn(t), t)
  end,
  s-str(self, l, str):
    t = A.s-str(l, str)
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

fun wrap-body(prog :: A.Program) -> A.Program:
  cases(A.Program) prog:
    | s-program(l, prov, provt, imp, body) =>
      A.s-program(l, prov, provt, imp, A.s-hint-exp(l, [list: A.h-stepper-body], body))
  end
end

fun stepify-prog(prog :: A.Program) -> A.Program:
  fun find-body(expr):
    var body = none
    expr.visit(A.default-iter-visitor.{
        s-hint-exp(self, _, hints, shadow expr):
          if hints == [list: A.h-stepper-body]:
            body := some(expr)
            true
          else:
            expr.visit(self)
          end
        end
      })
    cases(Option) body:
      | some(shadow body) => body
      | none              =>
        raise(```stepify-prog: expected to find a h-prog-body tag around the
          part of the program that needs to be traced.```)
    end
  end
  fun replace-body(expr, replacement):
    # Assumes there is exactly one body tag.
    expr.visit(A.default-map-visitor.{
        s-hint-exp(self, l, hints, shadow expr):
          if hints == [list: A.h-stepper-body]:
            replacement
          else:
            A.s-hint-exp(l, hints, expr.visit(self))
          end
        end
      })
  end
  cases(A.Program) prog:
    | s-program(l, prov, prov-ty, imp, block) =>
      body = find-body(block)
      shadow body = replace-body(block, stepify-expr(body))
      A.s-program(l, prov, prov-ty, imp, body)
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

  print("[stepper] Desugared program:")
  print(expr.tosource().pretty(80).join-str("\n"))
  l = expr.l
  A.s-block(l, [list:
      A.s-app(l, gid(l, "print"), [list: A.s-str(l, "Gonna step")]),
      stepify(expr)])
end
