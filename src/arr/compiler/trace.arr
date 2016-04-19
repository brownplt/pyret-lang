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
fun V_ARG(l, n):  A.s-name(l, "$trace_arg" + torepr(n)) end
fun ID_ARG(l, n): A.s-id(l, V_ARG(l, n)) end

fun gid(l, id): A.s-id(l, A.s-name(l, id)) end

fun AST(l, srcloc):
  A.s-prim-app(l, "makeSrcloc", [list: A.s-srcloc(l, srcloc)])
end

fun TRACER(l, func, args):
  A.s-app(l, A.s-dot(l, ID_TRACER(l), func), args)
end

fun SHOW_TRACE(l):
  TRACER(l, "show-trace", [list:])
end

fun ENTER(l, srcloc):
  TRACER(l, "enter", [list: AST(l, srcloc)])
end

fun EXIT(l, srcloc):
  TRACER(l, "exit", [list: AST(l, srcloc)])
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
  A.s-let(l, A.s-bind(l, true, v, A.a-blank), expr, false)
end

fun PLUS(l, a, b):  A.s-op(l, "op+", a, b) end
fun PLUS3(l, a, b, c): PLUS(l, a, PLUS(l, b, c)) end

fun TO_REPR(l, a):
  A.s-app(l, gid(l, "torepr"), [list: a])
end

fun LET_ARGS(l, args):
  for map_n(n from 0, arg from args):
    LET(l, V_ARG(l, n), args.get(n))
  end
end

fun ARG_IDS(l, n):
  for map(i from range(0, n)):
    ID_ARG(l, i)
  end
end

fun SHOW_ARGS(l, n):
  for map(i from range(0, n)):
    TO_REPR(l, ID_ARG(l, i))
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
      LET(l, V_ID(l), LOG_CALL(l, A.s-str(l, name), SHOW_ARGS(l, args.length()))),
      LET(l, V_ANSWER(l), A.s-app(l, func, ARG_IDS(l, args.length()))),
      LOG_RETURN(l, TO_REPR(l, ID_ANSWER(l)), ID_ID(l)),
      ID_ANSWER(l)
    ])
end

fun xform-check(l, block :: A.Expr):
  A.s-block(l, [list:
      ENTER(l, l),
      LET(l, V_ANSWER(l), block),
#      block,
      EXIT(l, l),
      ID_ANSWER(l)
    ])
end

fun xform-opt-check(l, _check :: Option<A.Expr>):
  cases(Option) _check:
    | none => none
    | some(block) => some(xform-check(l, block))
  end
end

trace-visitor =
  A.default-map-visitor.{
    s-app(self, l, func, args):
      shadow func = func.visit(self)
      shadow args = args.map(_.visit(self))
      cases(A.Expr) func:
        | s-id(_, v)           => xform-app(v, l, func, args)
        | s-id-var(_, v)       => xform-app(v, l, func, args)
        | s-id-letrec(_, v, _) => xform-app(v, l, func, args)
        | else => A.s-app(l, func, args)
      end
    end,
    s-check(self, l, name, body, kw):
      shadow body = body.visit(self)
      A.s-check(l, name, xform-check(l, body), kw)
    end,
    s-fun(self, l, name, params, args, ann, doc, body, _check):
      shadow args   = args.map(_.visit(self))
      shadow ann    = ann.visit(self)
      shadow body   = body.visit(self)
      shadow _check = self.option(_check)
      A.s-fun(l, name, params, args, ann, doc, body, xform-opt-check(l, _check))
    end,
    s-data(self, l, name, params, mixins, variants, mems, _check):
      shadow mixins   = mixins.map(_.visit(self))
      shadow variants = variants.map(_.visit(self))
      shadow mems     = mems.map(_.visit(self))
      shadow _check   = self.option(_check)
      A.s-data(l, name, params, mixins, variants, mems, xform-opt-check(l, _check))
    end,
    s-check-test(self, l, op, refinement, left, right):
      shadow refinement = self.option(refinement)
      shadow left       = left.visit(self)
      shadow right      = self.option(right)
      xform-check(l, A.s-check-test(l, op, refinement, left, right))
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
