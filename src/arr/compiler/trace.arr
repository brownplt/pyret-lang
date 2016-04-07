#lang pyret

provide *
provide-types *
import ast as A
import srcloc as S
import parse-pyret as PP
import string-dict as SD
import "compiler/compile-structs.arr" as C
import "compiler/ast-util.arr" as U


### AST Construction ###

names = A.global-names
dummy-loc = S.builtin("dummy location")

fun V_DEPTH(l):   A.s-name(l, "$trace_depth") end
fun ID_DEPTH(l):  A.s-id(l, V_DEPTH(l)) end
fun V_ANSWER(l):  A.s-name(l, "$trace_answer") end
fun ID_ANSWER(l): A.s-id(l, V_ANSWER(l)) end
fun V_ARG(l, arg, n):  A.s-name(l, "$trace_arg" + torepr(n)) end
fun ID_ARG(l, arg, n): A.s-id(l, V_ARG(l, arg, n)) end

#fun gid(l, id): A.s-id(l, A.s-global(id));
fun gid(l, id): A.s-id(l, A.s-name(l, id)); # hack

fun PRINT(l, msg):
  A.s-app(l, gid(l, "print"), [list: msg])
end

fun PRINT_STR(l, msg):
  PRINT(l, A.s-str(l, msg))
end

fun PRINT_EXPR(l, msg, expr):
  A.s-block(l, [list: PRINT(l, msg), expr])
end

fun EMPTY_LIST(l):
  gid(l, "list")
end

fun LET(l, v, expr):
  A.s-let(l, A.s-bind(l, false, v, A.a-blank), expr, false)
end

fun LET_VAR(l, v, expr):
  A.s-var(l, A.s-bind(l, false, v, A.a-blank), expr)
end

fun PLUS(l, a, b):  A.s-op(l, "op+", a, b) end
fun PLUS3(l, a, b, c): PLUS(l, a, PLUS(l, b, c)) end
fun PLUS4(l, a,b ,c ,d): PLUS(l, a, PLUS(l, b, PLUS(l, c, d))) end

fun INDENT(l):
  A.s-assign(l, V_DEPTH(l), PLUS(l, ID_DEPTH(l), A.s-num(l, 1)))
end

fun DEDENT(l):
  A.s-assign(l, V_DEPTH(l), PLUS(l, ID_DEPTH(l), A.s-num(l, -1)))
end

fun REPEAT_STR(l, a, b):
  A.s-app(l, gid(l, "string-repeat"), [list: a, b])
end

fun TO_REPR(l, a):
  A.s-app(l, gid(l, "torepr"), [list: a])
end

fun PRINT_INDENTED(l, msg):
  PRINT(l, PLUS(l,
      REPEAT_STR(l, A.s-str(l, " "), ID_DEPTH(l)),
      msg))
end

fun LET_ARGS(l, args):
  for map_n(n from 0, arg from args):
    LET(l, V_ARG(l, arg, n), args.get(n))
  end
end

fun ARG_IDS(l, args):
  for map_n(n from 0, arg from args):
    ID_ARG(l, arg, n)
  end
end

fun SHOW_ARGS(l, args):
  cases(List) args:
    | empty => A.s-str(l, "")
    | link(first-arg, shadow args) =>
      for fold(expr from TO_REPR(l, first-arg), arg from args):
        PLUS3(l, expr, A.s-str(l, ", "), TO_REPR(l, arg))
      end
  end
#  A.s-str(l, "")
end

### Rewrite an AST to trace its execution ###

fun wrap-app(name, l, func, args):
  shadow name = cases(A.Name) name:
    | s-global(str)  => str
    | s-atom(str, _) => str
    | s-name(_, str) => str
    | else =>
      raise("Error when tracing: unexpected variable type: " + torepr(name))
  end
  A.s-block(l,
    LET_ARGS(l, args) +
    [list:
      PRINT_INDENTED(l,
        PLUS3(l,
          A.s-str(l, name + "("),
          SHOW_ARGS(l, args),
          A.s-str(l, ")"))),
      INDENT(l),
      LET(l, V_ANSWER(l), A.s-app(l, func, ARG_IDS(l, args))),
      DEDENT(l),
      PRINT_INDENTED(l,
        PLUS4(l,
          A.s-str(l, name + "("),
          SHOW_ARGS(l, args),
          A.s-str(l, ") = "),
          TO_REPR(l, ID_ANSWER(l)))),
      ID_ANSWER(l)
    ])
end

trace-visitor =
  A.default-map-visitor.{
    s-app(self, l, func, args):
      cases(A.Expr) func:
        | s-id(_, v)           => wrap-app(v, l, func, args)
        | s-id-var(_, v)       => wrap-app(v, l, func, args)
        | s-id-letrec(_, v, _) => wrap-app(v, l, func, args)
        | else => A.s-app(l, func, args)
      end
    end
  }

fun wrap-program(l, stmts :: List<A.Expr>):
  shadow stmts = for map(stmt from stmts):
    stmt.visit(trace-visitor)
  end
  A.s-block(l,
    link(PRINT_STR(l, "Begin trace."),
      link(LET_VAR(l, V_DEPTH(l), A.s-num(l, 0)),
        stmts)))
end

fun trace(program :: A.Program):
  cases(A.Program) program:
    | s-program(l, _prov, _provty, _imp, body) =>
      cases(A.Expr) body:
        | s-block(shadow l, block) =>
          A.s-program(l, _prov, _provty, _imp, wrap-program(l, block))
        | else =>
          raise("Error when tracing - found non-block: " +
            torepr(body))
      end
    | else => raise("Error when tracing - found non-program: " +
        torepr(program))
  end
end
