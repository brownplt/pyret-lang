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

var global-stack = [list:]

fun gid(l, id):
  A.s-id(l, A.s-global(id))
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

fun NODE(l, name, childs):
  A.s-app(l, gid(l, "_node"),
    [list: A.s-str(l, name), ast-srcloc(l), ast-list(childs)])
end

fun VALUE(l, val):
  A.s-app(l, gid(l, "_value"), [list: val])
end

fun BOOL_VALUE(l, bool): VALUE(l, A.s-bool(l, bool)) end
fun NUM_VALUE(l, num):   VALUE(l, A.s-num(l, num))   end
fun STR_VALUE(l, str):   VALUE(l, A.s-str(l, str))   end
fun FRAC_VALUE(l, numer, denom): VALUE(l, A.s-frac(l, numer, denom)) end

fun BLOCK(l, stmts):     A.s-block(l, stmts) end
fun APP0(l, func):       A.s-app(l, func, [list:]) end
fun APP1(l, func, arg):  A.s-app(l, func, [list: arg]) end
fun APP2(l, f, x, y):    A.s-app(l, f, [list: x, y]) end
fun DOT(l, expr, field): A.s-dot(l, expr, field) end

fun NODE0(l, name):        NODE(l, name, [list:]) end
fun NODE1(l, name, child): NODE(l, name, [list: child]) end
fun NODE2(l, name, x, y):  NODE(l, name, [list: x, y]) end

fun CALL0(l, expr, field):      APP0(l, DOT(l, expr, field)) end
fun CALL1(l, expr, field, arg): APP1(l, DOT(l, expr, field), arg) end
fun PRINT(l, arg):              APP1(l, gid(l, "print"), arg) end
fun TO_AST(l, arg):             APP1(l, gid(l, "_to-ast"), arg) end
fun PLUS(l, x, y):              APP2(l, gid(l, "_plus"), x, y) end

fun ID(l, v):
  A.s-id(l, v)
end

fun BIND(l, v):
  A.s-bind(l, false, v, A.a-blank)
end

fun LET(l, v, arg, body):
  binding = A.s-let-bind(l, BIND(l, v), arg)
  A.s-let-expr(l, [list: binding], body)
end

fun LAM(l, v, body):
  A.s-lam(l, [list:], [list: BIND(l, v)], A.a-blank, "", body, none)
end

fun LIST(l, lst):
  cases(List) lst:
    | empty =>
      NODE0(l, "Empty")
    | link(first, rest) =>
      NODE2(l, "Link", first, LIST(l, rest))
  end
end

fun PRETTY_AST(l, ast):
  lines = CALL1(l, CALL0(l, ast, "tosource"), "pretty", A.s-num(l, 80))
  PLUS(l, A.s-str(l, "    "), CALL1(l, lines, "join-str", A.s-str(l, "\n    ")))
end

fun PRINT_AST(l,  ast):
  PRINT(l, PRETTY_AST(l, TO_AST(l, ast)))
end

fun PUSH(l, frame): APP1(l, gid(l, "_push"), frame) end
fun POP(l):         APP0(l, gid(l, "_pop")) end
fun WRAP(l, expr):  APP1(l, gid(l, "_wrap"), expr) end

adorn-visitor = A.default-map-visitor.{
  s-block(self, l, stmts):
    NODE1(l, "s-block", LIST(l, map(_.visit(self), stmts)))
  end,
  s-bool(self, l, bool): NODE1(l, "s-bool", BOOL_VALUE(l, bool)) end,
  s-num(self, l, num):   NODE1(l, "s-num", NUM_VALUE(l, num))    end,
  s-str(self, l, str):   NODE1(l, "s-str", STR_VALUE(l, str))    end,
  s-frac(self, l, numer, denom): NODE1(l, "s-frac", FRAC_VALUE(l, numer, denom)) end,
  s-atom(self, base, serial): A.s-atom(base, serial) end
}

fun ADORN(expr):
  expr.visit(adorn-visitor)
end

fun STEP(l, ast, cont):
  BLOCK(l, [list:
      PRINT(l, A.s-str(l, "->")),
      PRINT_AST(l, WRAP(l, ast)),
      cont])
end

fun FRAME(l):
  lam(frame-wrapper, ast):
    v = A.global-names.make-atom("v-frame")
    h = A.global-names.make-atom("h-frame")
    frame = LAM(l, h, ADORN(frame-wrapper(A.s-id(l, h))))
    BLOCK(l, [list:
        PUSH(l, frame),
        LET(l, v, ast, BLOCK(l, [list:
              POP(l),
              ID(l, v)
            ]))
      ])
  end
end

stepify-visitor = A.default-map-visitor.{
  s-bool(self, l, bool):
    t = A.s-bool(l, bool)
    STEP(l, BOOL_VALUE(l, bool), t)
  end,
  s-num(self, l, num):
    t = A.s-num(l, num)
    STEP(l, NUM_VALUE(l, num), t)
  end,
  s-str(self, l, str):
    t = A.s-str(l, str)
    STEP(l, STR_VALUE(l, str), t)
  end,
  s-frac(self, l, numer, denom):
    t = A.s-frac(l, numer, denom)
    STEP(l, FRAC_VALUE(l, numer, denom), t)
  end,
  s-block(self, l, stmts):
    if stmts.length() <= 1:
      A.s-block(l, map(_.visit(self), stmts))
    else:
      head = stmts.first
      tail = stmts.rest
      t = A.s-block(l, tail)
      BLOCK(l, [list:
          for FRAME(l)(h from head.visit(self)):
            A.s-block(l, link(h, tail))
          end,
          STEP(l, ADORN(t), t.visit(self))
        ])
    end
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

  l = expr.l
  A.s-block(l, [list:
      PRINT(l, A.s-str(l, "Gonna step")),
      PRINT_AST(l, ADORN(expr)),
      stepify(expr)])
end
