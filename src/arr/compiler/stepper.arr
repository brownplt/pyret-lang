#lang pyret

provide {
  stepify: stepify-compile-result,
  stepify-prog: stepify-prog,
  wrap-body: wrap-body
} end

import ast as A
import "compiler/compile-structs.arr" as C
import quote as Q
import srcloc as SL

# NOTES:
# * To add new magic global names, add them to
#       src/arr/compiler/compile-structs.arr
#   and src/js/base/runtime-anf.js

# TODO:
# * Builtin functions like _plus don't get wrapped;
#   this results in missing steps.
# * Remove s-escape?

# TYPES:
#         Core    -- A Pyret core-language ast
#   `q-`  Quoted  -- A Pyret ast that, when run, produces and ast
#   `v-`  Visited -- A Pyret ast that's been augmented to show evaluation steps
#
#   quote         :: Core   -> Quoted
#   _.visit       :: Core   -> Visited
#   PRINT_AST     :: Quoted -> Visited

dummy-loc = SL.builtin("dummy location")

fun quote(ast):
  ### Core -> Quoted
  Q.quote-ast(ast)
end

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

fun DEBUG(msg, expr):
  l = dummy-loc
  BLOCK(l, [list:
      PRINT(l, A.s-str(l, msg)),
      expr])
end

fun BIND(l, v): A.s-bind(l, false, v, A.a-blank);

fun LET(l):
  ### for LET(X :: Visited): Visited end -> Visited
  v = A.global-names.make-atom("r$let")
  lam(v-body, v-arg):
    binding = A.s-let-bind(l, BIND(l, v), v-arg)
    A.s-let-expr(l, [list: binding], v-body(v))
  end
end

fun LAM(l):
  v = A.global-names.make-atom("r$lam")
  lam(body, _):
    A.s-lam(l, [list:], [list: BIND(l, v)], A.a-blank, "", body(v), none)
  end
end

fun IF(l, _cond, _then, _else):
  A.s-if-else(l, [list: A.s-if-branch(l, _cond, _then)], _else)
end

fun PRETTY_AST(l, q-ast):
  lines = CALL1(l, CALL0(l, q-ast, "tosource"), "pretty", A.s-num(l, 80))
  PLUS(l, A.s-str(l, "    "), CALL1(l, lines, "join-str", A.s-str(l, "\n    ")))
end

fun PRINT_AST(l, q-ast):
  ### Quoted -> Visited
  PRINT(l, PRETTY_AST(l, q-ast))
end

fun PUSH(l, frame): APP1(l, gid(l, "_push"), frame) end
fun POP(l):         APP0(l, gid(l, "_pop")) end
fun WRAP(l, expr):  APP1(l, gid(l, "_wrap"), expr) end

fun STEP_TO(l, ast, v-ast):
  ### Core -> Visited -> Visited
  BLOCK(l, [list:
      PRINT(l, A.s-str(l, "->")),
      PRINT_AST(l, WRAP(l, quote(ast))),
      v-ast])
end

fun STEP(l, self, ast):
  ### Core -> Visited
  STEP_TO(l, ast, ast.visit(self))
end

fun STEP_TO_VALUE(l, ast):
  ### Core/Visited -> Visited
  STEP_TO(l, A.s-value(ast), ast)
end

fun FRAME(l, self):
  ### for FRAME(H :: Core): Core end -> Visited
  lam(frame-wrapper, ast):
    frame = for LAM(l)(H from nothing):
      quote(frame-wrapper(A.s-id(l, H)))
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

fun get-field-value(field):
  cases(A.Member) field:
    | s-data-field(_, _, v)       => v
    | s-mutable-field(_, _, _, v) => v
    | else => raise("Stepper: s-method-field NYI")
  end
end

fun replace-field-value(field, val):
  cases(A.Member) field:
    | s-data-field(_l, _n, _)        => A.s-data-field(_l, _n, val)
    | s-mutable-field(_l, _n, _a, _) => A.s-mutable-field(_l, _n, _a, val)
    | else => raise("Stepper: s-method-field NYI")
  end
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
  s-id(self, l, id):
    STEP_TO_VALUE(l, A.s-id(l, id))
  end,
  s-var-id(self, l, id):
    STEP_TO_VALUE(l, A.s-var-id(l, id))
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
  s-array(self, l, vals):
    for FRAMES(l, self, A.s-array(l, _))(VALS from vals):
      for LET(l)(V from A.s-array(l, VALS)):
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
          ID(l, V)
        end
      end
    end
  end,
  s-let-expr(self, l, binds, body):
    fun replace-bind-value(bind, val):
      cases(A.LetBind) bind:
        | s-let-bind(_l, _b, _) => A.s-let-bind(_l, _b, val)
        | s-var-bind(_l, _b, _) => A.s-var-bind(_l, _b, val)
      end
    end
    fun stepify-let(remaining-binds, Vs):
      cases(List) remaining-binds:
        | empty =>
          shadow binds = map2(replace-bind-value, binds, Vs.reverse())
          A.s-let-expr(l, binds, body.visit(self))
        | link(bind, shadow remaining-binds) =>
          for LET(l)(V from
              for FRAME(l, self)(V from bind.value):
                shadow bind = replace-bind-value(bind, V)
                A.s-let-expr(l, link(bind, remaining-binds), body)
              end):
            T = if is-empty(remaining-binds): body
                else: A.s-let-expr(l, remaining-binds, body)
                end
            STEP_TO(l, T,
              stepify-let(remaining-binds, link(ID(l, V), Vs)))
          end
      end
    end
    stepify-let(binds, empty)
  end,
  s-dot(self, l, obj, field):
    for LET(l)(O from
        for FRAME(l, self)(O from obj):
          A.s-dot(l, O, field)
        end):
      STEP_TO_VALUE(l, A.s-dot(l, ID(l, O), field))
    end
  end,
  s-obj(self, l, mems):
    fun stepify-obj(mems-f, mems-v, shadow mems):
      cases(List) mems:
        | empty =>
          STEP_TO_VALUE(l, A.s-obj(l, mems-v))
        | link(mem, shadow mems) =>
          cases(A.Member) mem:
            | s-data-field(_l, _name, _val) =>
              for LET(l)(V from
                  for FRAME(l, self)(H from _val):
                    mem-h = A.s-data-field(_l, _name, H)
                    A.s-obj(l, mems-f + [list: mem-h] + mems)
                  end):
                mem-v = A.s-data-field(_l, _name, ID(l, V))
                mem-f = A.s-data-field(_l, _name, A.s-value(ID(l, V)))
                stepify-obj(link(mem-f, mems-f), link(mem-v, mems-v), mems)
              end
            | s-mutable-field(_l, _name, _ann, _val) =>
              for LET(l)(V from
                  for FRAME(l, self)(H from _val):
                    mem-h = A.s-mutable-field(_l, _name, _ann, H)
                    A.s-obj(l, mems-f + [list: mem-h] + mems)
                  end):
                mem-v = A.s-mutable-field(_l, _name, _ann, ID(l, V))
                mem-f = A.s-mutable-field(_l, _name, _ann, A.s-value(ID(l, V)))
                stepify-obj(link(mem-f, mems-f), link(mem-v, mems-v), mems)
              end
            | else => error("s-obj: method-fields NYI")
          end
      end
    end
    stepify-obj(empty, empty, mems)
  end,
  s-lam(self, l, params, args, ann, doc, body, _check):
    shadow body = STEP(l, self, body)
    A.s-lam(l, params, args, ann, doc, body, _check)
  end,
  s-method(self, l, params, args, ann, doc, body, _check):
    shadow body = STEP(l, self, body)
    A.s-method(l, params, args, ann, doc, body, _check)
  end,
  s-update(self, l, supe, fields):
    for LET(l)(S from
        for FRAME(l, self)(H from supe):
          A.s-update(l, H, fields)
        end):
      fun make-update(_s, _vals):
        A.s-update(l, _s, map2(replace-field-value, fields, _vals))
      end
      for FRAMES(l, self, make-update(A.s-value(ID(l, S)), _))(
          VALS from map(get-field-value, fields)):
        for LET(l)(V from make-update(ID(l, S), VALS)):
          ID(l, V)
        end
      end
    end
  end,
  s-extend(self, l, supe, fields):
    for LET(l)(S from
        for FRAME(l, self)(H from supe):
          A.s-extend(l, H, fields)
        end):
      fun make-extend(_s, _vals):
        A.s-extend(l, _s, map2(replace-field-value, fields, _vals))
      end
      for FRAMES(l, self, make-extend(A.s-value(ID(l, S)), _))(
          VALS from map(get-field-value, fields)):
        for LET(l)(V from make-extend(ID(l, S), VALS)):
          ID(l, V)
        end
      end
    end
  end,
  #
  # Would be nice to figure out how to have recursive cases like this:
  #
  # s-data-field(self, l, name, val):
  #   for LET(l)(V from
  #       for FRAME(l, self)(H from val):
  #         A.s-data-field(l, name, H)
  #       end):
  #     STEP_TO(l,
  #       A.s-data-field(l, name, ID(l, V)),
  #       A.s-data-field(l, name, ID(l, V)))
  #   end
  # end,
  s-assign(self, l, id, val):
    for FRAME(l, self)(H from val):
      A.s-assign(l, id, H)
    end
  end,
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
      PRINT_AST(l, quote(expr)),
      stepify(expr)])
  print("-------")
  stepified
end
