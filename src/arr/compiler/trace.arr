#lang pyret

provide *
provide-types *
import ast as A
import srcloc as S
import parse-pyret as PP
import string-dict as SD
import "compiler/gensym.arr" as G
import "compiler/compile-structs.arr" as C
import "compiler/ast-util.arr" as U


### AST Construction ###

# Security through unspellability
fun V_TRACER(l):  A.s-name(l, "$tracer") end # Defined in arr/compiler/compile-structs.arr
fun ID_TRACER(l): A.s-id(l, V_TRACER(l)) end
fun V_ANSWER(l):  A.s-name(l, "$trace_answer") end
fun V_ID(l): A.s-name(l, "$trace_id") end
fun ID_ID(l): A.s-id(l, V_ID(l)) end
fun ID_ANSWER(l): A.s-id(l, V_ANSWER(l)) end
fun V_ARG(l, arg, n):  A.s-name(l, "$trace_arg" + torepr(n)) end
fun ID_ARG(l, arg, n): A.s-id(l, V_ARG(l, arg, n)) end

fun gid(l, id): A.s-id(l, A.s-name(l, id)) end

fun TRACER(l, func, args):
  A.s-app(l, A.s-dot(l, ID_TRACER(l), func), args)
end

fun SHOW_TRACE(l):
  TRACER(l, "show-trace", [list:])
end

fun LIST(l, elems):
  A.s-construct(l, A.s-construct-normal, gid(l, "list"), elems)
end

fun LOG_CALL(l, func, args):
  TRACER(l, "log-call", [list: func, LIST(l, args)])
end

fun LOG_RETURN(l, return-val, id):
  TRACER(l, "log-return", [list: return-val, id])
end

fun PRINT(l, msg):
  A.s-app(l, gid(l, "print"), [list: msg])
end

fun IMPORT(l, module-str, name):
  A.s-import(l, A.s-const-import(l, module-str), name)
end

fun LET(l, v, expr):
  A.s-let(l, A.s-bind(l, false, v, A.a-blank), expr, false)
end

fun PLUS(l, a, b):  A.s-op(l, "op+", a, b) end
fun PLUS3(l, a, b, c): PLUS(l, a, PLUS(l, b, c)) end

fun TO_REPR(l, a):
  A.s-app(l, gid(l, "torepr"), [list: a])
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
  for map(arg from args):
    TO_REPR(l, arg)
  end
end


### Rewrite an AST to trace its execution ###

fun xform-app(name, l, func, args):
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
      LET(l, V_ID(l), LOG_CALL(l, A.s-str(l, name), SHOW_ARGS(l, args))),
      LET(l, V_ANSWER(l), A.s-app(l, func, ARG_IDS(l, args))),
      LOG_RETURN(l, TO_REPR(l, ID_ANSWER(l)), ID_ID(l)),
      ID_ANSWER(l)
    ])
end

trace-visitor =
  A.default-map-visitor.{
    s-app(self, l, func, args):
      cases(A.Expr) func:
        | s-id(_, v)           => xform-app(v, l, func, args)
        | s-id-var(_, v)       => xform-app(v, l, func, args)
        | s-id-letrec(_, v, _) => xform-app(v, l, func, args)
        | else => A.s-app(l, func, args)
      end
    end
  }

fun xform-program(l, stmts :: List<A.Expr>):
  shadow stmts = for map(stmt from stmts):
    stmt.visit(trace-visitor)
  end
  id-result = A.s-name(l, G.make-name("result-after-trace"))
  last-expr = stmts.last()
  A.s-block(l,
    stmts.take(stmts.length() - 1) +
    [list:
      LET(l, id-result, last-expr),
#      SHOW_TRACE(l),
      A.s-id(l, id-result)])
end

fun trace(program :: A.Program):
  cases(A.Program) program:
    | s-program(l, _prov, _provty, _imp, body) =>
      cases(A.Expr) body:
        | s-block(shadow l, block) =>
          A.s-program(l, _prov, _provty, _imp, xform-program(l, block))
        | else =>
          raise("Error when tracing - found non-block: " +
            torepr(body))
      end
    | else => raise("Error when tracing - found non-program: " +
        torepr(program))
  end
end
