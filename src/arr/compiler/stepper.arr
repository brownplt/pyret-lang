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

# NOTES:
# * To add new magic global names, add them to
#       src/arr/compiler/compile-structs.arr
#   and src/js/base/runtime-anf.js

dummy-loc = SL.builtin("dummy location")

fun pretty-ast(ast):
  "    " + ast.tosource().pretty(80).join-str("\n    ")
end

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

fun BLOCK(l, stmts):     A.s-block(l, stmts);
fun APP0(l, func):       A.s-app(l, func, [list:]);
fun APP1(l, func, arg):  A.s-app(l, func, [list: arg]);
fun APP2(l, f, x, y):    A.s-app(l, f, [list: x, y]);
fun DOT(l, expr, field): A.s-dot(l, expr, field);
fun ID(l, v):            A.s-id(l, v);

fun CALL0(l, expr, field):      APP0(l, DOT(l, expr, field));
fun CALL1(l, expr, field, arg): APP1(l, DOT(l, expr, field), arg);
fun PRINT(l, arg):              APP1(l, gid(l, "print"), arg);
fun TO_AST(l, arg):             APP1(l, gid(l, "_to-ast"), arg);
fun PLUS(l, x, y):              APP2(l, gid(l, "_plus"), x, y);

fun BIND(l, v): A.s-bind(l, false, v, A.a-blank);

fun LET(l):
  v = A.global-names.make-atom("v-let")
  lam(body, arg):
    binding = A.s-let-bind(l, BIND(l, v), arg)
    A.s-let-expr(l, [list: binding], body(v))
  end
end

fun LAM(l):
  v = A.global-names.make-atom("v-lam")
  lam(body, _):
    A.s-lam(l, [list:], [list: BIND(l, v)], A.a-blank, "", body(v), none)
  end
end

fun IF(l, _cond, _then, _else):
  A.s-if-else(l, [list: A.s-if-branch(l, _cond, _then)], _else)
end

fun PRETTY_AST(l, ast):
  lines = CALL1(l, CALL0(l, ast, "tosource"), "pretty", A.s-num(l, 80))
  PLUS(l, A.s-str(l, "    "), CALL1(l, lines, "join-str", A.s-str(l, "\n    ")))
end

fun PRINT_AST(l,  ast):
  PRINT(l, PRETTY_AST(l, ast))
end

fun PUSH(l, frame): APP1(l, gid(l, "_push"), frame) end
fun POP(l):         APP0(l, gid(l, "_pop")) end
fun WRAP(l, expr):  APP1(l, gid(l, "_wrap"), expr) end

fun STEP_TO_VALUE(l, ast):
  VAL = A.s-app(l,
    A.s-dot(l, gid(l, "_ast"), "s-value"),
    [list: ast])
  BLOCK(l, [list:
      PRINT(l, A.s-str(l, "->")),
      PRINT_AST(l, WRAP(l, VAL)),
      ast])
end

fun STEP(l, self, ast):
  BLOCK(l, [list:
      PRINT(l, A.s-str(l, "->")),
      PRINT_AST(l, WRAP(l, CV.ast-to-constr(ast))),
      ast.visit(self)])
end

fun FRAME(l, self):
  lam(frame-wrapper, ast):
    frame = for LAM(l)(H from nothing):
      CV.ast-to-constr(frame-wrapper(A.s-escape(A.s-id(l, H))))
    end
    BLOCK(l, [list:
        PUSH(l, frame),
        for LET(l)(V from ast.visit(self)):
          BLOCK(l, [list:
              POP(l),
              ID(l, V)
            ])
        end
      ])
  end
end

fun FRAMES(l, self, frame-wrapper):
  fun make-frames(body, asts):
    if asts.length() == 0:
      body([list:])
    else:
      for LET(l)(V from
          for FRAME(l, self)(H from asts.first):
            frame-wrapper(link(H, asts.rest))
          end):
        wrapper = lam(shadow asts):
          frame-wrapper(link(A.s-value(ID(l, V)), asts))
        end
        for FRAMES(l, self, wrapper)(rest from asts.rest):
          body(link(ID(l, V), rest))
        end
      end
    end
  end
  make-frames
end

stepify-visitor = A.default-map-visitor.{
  s-bool(self, l, bool):
    STEP_TO_VALUE(l, A.s-bool(l, bool))
  end,
  s-num(self, l, num):
    STEP_TO_VALUE(l, A.s-num(l, num))
  end,
  s-str(self, l, str):
    STEP_TO_VALUE(l, A.s-str(l, str))
  end,
  s-frac(self, l, numer, denom):
    STEP_TO_VALUE(l, A.s-frac(l, numer, denom))
  end,
  s-block(self, l, stmts):
    if stmts.length() <= 1:
      A.s-block(l, map(_.visit(self), stmts))
    else:
      BLOCK(l, [list:
          for FRAME(l, self)(H from stmts.first):
            A.s-block(l, link(H, stmts.rest))
          end,
          STEP(l, self, A.s-block(l, stmts.rest))
        ])
    end
  end,
  s-if-else(self, l, branches, _else):
    test = branches.first.test
    body = branches.first.body
    shadow branches = branches.rest
    for LET(l)(V from
        for FRAME(l, self)(H from test):
          A.s-if-else(l, link(A.s-if-branch(l, H, body), branches), _else)
        end):
      IF(l, ID(l, V),
        STEP(l, self, body),
        STEP(l, self,
          if is-empty(branches): _else
          else:                  A.s-if-else(l, branches, _else)
          end))
    end
  end,
  s-prim-app(self, l, _fun, args):
    for FRAMES(l, self, A.s-prim-app(l, _fun, _))(ARGS from args):
      for LET(l)(V from A.s-prim-app(l, _fun, ARGS)):
        STEP_TO_VALUE(l, ID(l, V))
      end
    end
  end,
  s-app(self, l, _fun, args):
    for LET(l)(F from
        for FRAME(l, self)(F from _fun):
          A.s-app(l, F, args)
        end):
      for FRAMES(l, self, A.s-app(l, A.s-value(ID(l, F)), _))(ARGS from args):
        for LET(l)(V from A.s-app(l, ID(l, F), ARGS)):
          STEP_TO_VALUE(l, ID(l, V))
        end
      end
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
  print("desugared-program:")
  print(pretty-ast(expr))
  print("end")
  print("")
  stepified = A.s-block(l, [list:
      PRINT_AST(l, CV.ast-to-constr(expr)),
      stepify(expr)])
  print("-------")
  stepified
end
