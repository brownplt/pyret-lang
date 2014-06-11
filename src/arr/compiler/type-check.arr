#lang pyret

provide *
provide-types *
import ast as A
import string-dict as SD
import "compiler/compile-structs.arr" as C
import "compiler/ast-util.arr" as U

data Type:
  | t-name  (id :: A.Name)
  | t-arrow (args :: List<Type>, ret :: Type)
  | t-any
end

fun tc-expr(e :: A.Expr, typs :: SD.StringDict, data-exprs :: SD.StringDict) -> C.CompileResult<A.Expr>:
  cases(A.Expr) e:
    | s-module(l, answer, provides, types, checks) =>
      raise("s-module not yet handled")
    | s-type-let-expr(l, binds, body) =>
      raise("s-type-let-expr not yet handled")
    | s-let-expr(l, binds, body) =>
      raise("s-let-expr not yet handled")
    | s-letrec(l, binds, body) =>
      raise("s-letrec not yet handled")
    | s-hint-exp(l, hints, exp) =>
      raise("s-hint-exp not yet handled")
    | s-instantiate(l, expr, params) =>
      raise("s-instantiate not yet handled")
    | s-block(l, stmts) =>
      raise("s-block not yet handled")
    | s-user-block(l, body) =>
      raise("s-user-block not yet handled")
    | s-fun(l, name,
        params, # Type parameters
        args, # Value parameters
        ann, # return type
        doc, body, _check) =>
      raise("s-fun not yet handled")
    | s-type(l, name, ann) =>
      raise("s-type not yet handled")
    | s-newtype(l, name, namet) =>
      raise("s-newtype not yet handled")
    | s-var(l, name, value) =>
      raise("s-var not yet handled")
    | s-let(l, name, value, keyword-val) =>
      raise("s-let not yet handled")
    | s-graph(l, bindings) =>
      raise("s-graph not yet handled")
    | s-contract(l, name, ann) =>
      raise("s-contract not yet handled")
    | s-when(l, test, block) =>
      raise("s-when not yet handled")
    | s-assign(l, id, value) =>
      raise("s-assign not yet handled")
    | s-if-pipe(l, branches) =>
      raise("s-if-pipe not yet handled")
    | s-if-pipe-else(l, branches, _else) =>
      raise("s-if-pipe-else not yet handled")
    | s-if(l, branches) =>
      raise("s-if not yet handled")
    | s-if-else(l, branches, _else) =>
      raise("s-if-else not yet handled")
    | s-cases(l, typ, val, branches) =>
      raise("s-cases not yet handled")
    | s-cases-else(l, typ, val, branches, _else) =>
      raise("s-cases-else not yet handled")
    | s-try(l, body, id, _except) =>
      raise("s-try not yet handled")
    | s-op(l, op, left, right) =>
      raise("s-op not yet handled")
    | s-check-test(l, op, left, right) =>
      raise("s-check-test not yet handled")
    | s-paren(l, expr) =>
      raise("s-paren not yet handled")
    | s-lam(l,
          params, # Type parameters
          args, # Value parameters
          ann, # return type
          doc, body, _check) =>
      raise("s-lam not yet handled")
    | s-method(l,
        args, # Value parameters
        ann, # return type
        doc, body, _check) =>
      raise("s-method not yet handled")
    | s-extend(l, supe, fields) =>
      raise("s-extend not yet handled")
    | s-update(l, supe, fields) =>
      raise("s-update not yet handled")
    | s-obj(l, fields) =>
      raise("s-obj not yet handled")
    | s-array(l, values) =>
      raise("s-array not yet handled")
    | s-construct(l, modifier, constructor, values) =>
      raise("s-construct not yet handled")
    | s-confirm(l, expr, typ) =>
      raise("s-confirm not yet handled")
    | s-bless(l, expr, typ) =>
      raise("s-bless not yet handled")
    | s-app(l, _fun, args) =>
      raise("s-app not yet handled")
    | s-prim-app(l, _fun, args) =>
      raise("s-prim-app not yet handled")
    | s-prim-val(l, name) =>
      raise("s-prim-val not yet handled")
    | s-id(l, id) =>
      raise("s-id not yet handled")
    | s-id-var(l, id) =>
      raise("s-id-var not yet handled")
    | s-id-letrec(l, id, safe) =>
      raise("s-id-letrec not yet handled")
    | s-undefined(l) =>
      raise("s-undefined not yet handled")
    | s-srcloc(l, loc) =>
      raise("s-srcloc not yet handled")
    | s-num(l, n) =>
      raise("s-num not yet handled")
    | s-frac(l, num, den) =>
      raise("s-frac not yet handled")
    | s-bool(l, b) =>
      raise("s-bool not yet handled")
    | s-str(l, s) =>
      raise("s-str not yet handled")
    | s-dot(l, obj, field) =>
      raise("s-dot not yet handled")
    | s-get-bang(l, obj, field) =>
      raise("s-get-bang not yet handled")
    | s-bracket(l, obj, field) =>
      raise("s-bracket not yet handled")
    | s-data(l, name,
        params, # type params
        mixins, variants, shared-members, _check) =>
      raise("s-data not yet handled")
    | s-data-expr(l,
        name,
        namet,
        params, # type params
        mixins, variants, shared-members, _check) =>
      raise("s-data-expr not yet handled")
    | s-for(l, iterator, bindings, ann, body) =>
      raise("s-for not yet handled")
    | s-check(l, name, body, keyword-check) =>
      raise("s-check not yet handled")
  end
end

fun type-check(program :: A.Program, compile-env :: C.CompileEnvironment) -> C.CompileResult<A.Program>:
  cases(A.Program) program:
    | s-program(l, _provide, provided-types, imports, body) =>
      result = tc-expr(body, SD.string-dict(), SD.string-dict())
      cases(C.CompileResult<A.Expr>) result:
        | ok(c) =>
          C.ok(A.s-program(l, _provide, provided-types, imports, c))
        | err(problems) =>
          C.err(problems)
      end
    | else => raise("Attempt to desugar non-program: " + torepr(program))
  end
end
