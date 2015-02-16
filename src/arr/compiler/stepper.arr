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

fun _node(l, name, childs):
  A.s-app(l, gid(l, "_node"),
    [list: A.s-str(l, name), ast-srcloc(l), ast-list(childs)])
end
fun NODE0(l, name):        _node(l, name, [list:]);
fun NODE1(l, name, child): _node(l, name, [list: child]);
fun NODE2(l, name, x, y):  _node(l, name, [list: x, y]);

fun _value(l, val): A.s-app(l, gid(l, "_value"), [list: val]);
fun BOOL(l, bool):  _value(l, A.s-bool(l, bool));
fun STR(l,  str):   _value(l, A.s-str(l, str));
fun NUM(l,  num):   _value(l, A.s-num(l, num));
fun FRAC(l,  n, d): _value(l, A.s-num(l, n), A.s-num(l, d));

fun VALUE(l, val):       NODE1(l, "Value", _value(l, val));
fun BOOL_VALUE(l, bool): VALUE(l, A.s-bool(l, bool));
fun STR_VALUE(l, str):   VALUE(l, A.s-str(l, str));
fun NUM_VALUE(l, num):   VALUE(l, A.s-str(l, num));
fun FRAC_VALUE(l, n, d): VALUE(l, A.s-frac(l, n, d));

fun BOOL_EXPR(l, bool):  NODE1(l, "s-bool", BOOL(l, bool));
fun STR_EXPR(l, str):    NODE1(l, "s-str", STR(l, str));
fun NUM_EXPR(l, num):    NODE1(l, "s-num", NUM(l, num));
fun FRAC_EXPR(l, n, d):  NODE1(l, "s-frac", FRAC(l, n, d));

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
  s-if-else(self, l, branches, _else):
    NODE2(l, "s-if-else", LIST(l, map(_.visit(self), branches)), _else.visit(self))
  end,
  s-if-branch(self, l, test, body):
    NODE2(l, "s-if-branch", test.visit(self), body.visit(self))
  end,
  s-prim-app(self, l, _fun, args):
    NODE2(l, "s-prim-app", STR(l, _fun), LIST(l, map(_.visit(self), args)))
  end,
  s-bool(self, l, bool): BOOL_EXPR(l, bool) end,
  s-str(self, l, str):   STR_EXPR(l, str) end,
  s-num(self, l, num):   NUM_EXPR(l, num) end,
  s-frac(self, l, n, d): FRAC_EXPR(l, n, d) end,
  s-atom(self, base, serial): A.s-atom(base, serial) end
}

fun ADORN(expr):
  expr.visit(adorn-visitor)
end

fun STEP_TO_VALUE(l, val, ast):
  BLOCK(l, [list:
      PRINT(l, A.s-str(l, "->")),
      PRINT_AST(l, WRAP(l, val)),
      ast])
end

fun STEP(l, self, ast):
  BLOCK(l, [list:
      PRINT(l, A.s-str(l, "->")),
      PRINT_AST(l, WRAP(l, ADORN(ast))),
      ast.visit(self)])
end

fun FRAME(l, self):
  lam(frame-wrapper, ast):
    frame = for LAM(l)(H from nothing):
      ADORN(frame-wrapper(A.s-id(l, H)))
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
        wrapper = lam(shadow asts): frame-wrapper(link(ID(l, V), asts)) end
        for make-frames(rest from asts.rest):
          body(link(ID(l, V), rest))
        end
      end
    end
  end
  make-frames
end

stepify-visitor = A.default-map-visitor.{
  s-bool(self, l, bool):
    t = A.s-bool(l, bool)
    STEP_TO_VALUE(l, BOOL_VALUE(l, bool), t)
  end,
  s-num(self, l, num):
    t = A.s-num(l, num)
    STEP_TO_VALUE(l, NUM_VALUE(l, num), t)
  end,
  s-str(self, l, str):
    t = A.s-str(l, str)
    STEP_TO_VALUE(l, STR_VALUE(l, str), t)
  end,
  s-frac(self, l, numer, denom):
    t = A.s-frac(l, numer, denom)
    STEP_TO_VALUE(l, FRAC_VALUE(l, numer, denom), t)
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
        STEP_TO_VALUE(l, VALUE(l, ID(l, V)), ID(l, V))
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
      PRINT_AST(l, ADORN(expr)),
      stepify(expr)])
  print("-------")
  stepified
end
