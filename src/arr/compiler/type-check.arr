#lang pyret

provide *
provide-types *
import ast as A
import string-dict as SD
import "compiler/compile-structs.arr" as C
import "compiler/ast-util.arr" as U

data Pair:
  | pair(left, right)
end

fun <I> identity(i :: I) -> I: i;

data Type:
  | t-name(l :: A.Loc, module-name :: Option<String>, id :: String)
  | t-arrow(l :: A.Loc, args :: List<Type>, ret :: Type)
  | t-top
  | t-bot
sharing:
  satisfies-type(self, other :: Type) -> Boolean:
    cases(Type) self:
      | t-name(_, module-name, id) =>
        cases(Type) other:
          | t-top => true
          | t-name(_, other-module, other-id) =>
            (module-name == other-module) and (id == other-id)
          | else => false
        end
      | t-arrow(_, args, ret) =>
        cases(Type) other:
          | t-top => true
          | t-arrow(_, other-args, other-ret) =>
            for lists.fold2(res from true, this-arg from self.args, other-arg from other-args):
              res and other-arg.satisfies-type(this-arg)
            end and ret.satisfies-type(other-ret)
          | else => false
        end
      | t-top => is-t-top(other)
      | t-bot => true
    end
  end
end

t-number  = t-name(A.dummy-loc, none, "tglobal#Number")
t-string  = t-name(A.dummy-loc, none, "tglobal#String")
t-boolean = t-name(A.dummy-loc, none, "tglobal#Boolean")

data TCInfo:
  | tc-info(typs       :: SD.StringDict,
            aliases    :: SD.StringDict,
            data-exprs :: SD.StringDict)
end

fun to-type(in-ann :: A.Ann, info :: TCInfo) -> Type:
  cases(A.Ann) in-ann:
    | a-blank =>
      # TODO(cody): At some point, this will probably need to be some other
      # type, to accommodate gradual typing. For now, just make it t-top.
      t-top
    | a-any =>
      t-top
    | a-name(l, id) =>
      t-name(l, none, id.key())
    | a-arrow(l, args, ret, use-parens) =>
      t-arrow(l, args.map(to-type(_, info)), to-type(ret, info))
    | a-method(l, args, ret) =>
      raise("a-method not yet handled:" + torepr(in-ann))
    | a-record(l, fields) =>
      raise("a-record not yet handled:" + torepr(in-ann))
    | a-app(l, ann, args) =>
      raise("a-app not yet handled:" + torepr(in-ann))
    | a-pred(l, ann, exp) =>
      raise("a-pred not yet handled:" + torepr(in-ann))
    | a-dot(l, obj, field) =>
      t-name(l, some(obj.key()), field)
    | a-checked(checked, residual) =>
      raise("a-checked should not be appearing before type checking!")
  end
end

fun to-type-or(ann :: A.Ann, alt :: Type, info :: TCInfo) -> Type:
  if A.is-a-blank(ann):
    alt
  else:
    to-type(ann, info)
  end
end


fun handle-type-let-binds(binds :: List<A.TypeLetBind>, info :: TCInfo):
  for each(bind from binds):
    cases(A.TypeLetBind) bind:
      | s-type-bind(_, name, ann) =>
        info.aliases.set(name.key(), to-type(ann, info))
      | s-newtype-bind(_, name, namet) =>
        raise("newtype not yet handled!")
    end
  end
end

fun synthesis(e :: A.Expr, info :: TCInfo) -> Type:
  cases(A.Expr) e:
    | s-module(l, answer, provides, types, checks) =>
      raise("s-module not yet handled")
    | s-type-let-expr(_, binds, body) =>
      handle-type-let-binds(binds, info)
      synthesis(body, info)
    | s-let-expr(l, binds, body) =>
      for each(bind from binds):
        synthesis-let-bind(bind, info)
      end
      synthesis(body, info)
    | s-letrec(l, binds, body) =>
      raise("s-letrec not yet handled")
    | s-hint-exp(l, hints, exp) =>
      raise("s-hint-exp not yet handled")
    | s-instantiate(l, expr, params) =>
      raise("s-instantiate not yet handled")
    | s-block(l, stmts) =>
      for fold(base from t-top, stmt from stmts):
        synthesis(stmt, info)
      end
    | s-user-block(l, body) =>
      raise("s-user-block not yet handled")
    | s-fun(l, name,
        params, # Type parameters
        args, # Value parameters
        ann, # return type
        doc, body, _check) =>
      arg-typs = args.foldr(lam(arg, base):
                              arg-typ = to-type(arg.ann, info)
                              info.typs.set(arg.id, arg-typ)
                              link(arg-typ, base)
                            end, empty)
      body-typ = synthesis(body, info)
      ret-typ  = to-type(ann, info)
      t-arrow(arg-typs, ret-typ)
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
      id-key = id.key()
      if info.typs.has-key(id-key):
        info.typs.get(id-key)
      else:
        raise("Identifier not found in environment! Tried to find: " + id-key)
      end
    | s-id-var(l, id) =>
      id-key = id.key()
      if info.typs.has-key(id-key):
        info.typs.get(id-key)
      else:
        raise("Identifier not found in environment! Tried to find: " + id-key)
      end
    | s-id-letrec(l, id, safe) =>
      id-key = id.key()
      if info.typs.has-key(id-key):
        info.typs.get(id-key)
      else:
        raise("Identifier not found in environment! Tried to find: " + id-key)
      end
    | s-undefined(l) =>
      raise("s-undefined not yet handled")
    | s-srcloc(l, loc) =>
      raise("s-srcloc not yet handled")
    | s-num(l, n) =>
      t-number
    | s-frac(l, num, den) =>
      t-number
    | s-bool(l, b) =>
      t-boolean
    | s-str(l, s) =>
      t-string
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

fun synthesis-let-bind(binding :: A.LetBind, info :: TCInfo) -> Type:
  cases(A.LetBind) binding:
    | s-let-bind(l, b, value) =>
      var typ = synthesis(value, info)
      expect-ann = b.ann
      when not(A.is-a-blank(expect-ann)):
        expect-typ = to-type(expect-ann, info)
        if typ.satisfies-type(expect-typ):
          typ := expect-typ
        else:
          raise(torepr(typ) + " does not satisfy the specified type: " + torepr(expect-typ))
        end
      end
      info.typs.set(b.id.key(), typ)
    | s-var-bind(l, b, value) =>
      raise("s-var-bind not yet handled")
  end
end

fun check-fun(body :: A.Expr, args :: List<A.Bind>, ret-ann :: A.Ann, expect-typ :: Type, info :: TCInfo) -> Boolean:
  arg-typs = cases(Type) expect-typ:
               | t-arrow(_, expect-args, _) =>
                 for map2(arg from args, expect-arg from expect-args):
                   arg-typ = to-type-or(arg, expect-arg, info)
                   info.typs.set(arg.id.key(), arg-typ)
                   arg-typ
                 end
               | else =>
                 for map(arg from args):
                   arg-typ = to-type(arg, info)
                   info.typs.set(arg.id.key(), arg-typ)
                   arg-typ
                 end
             end
  ret-typ  = cases(Type) expect-typ:
               | t-arrow(_, _, expect-ret) =>
                 to-type-or(ret-ann, expect-ret, info)
               | else =>
                 to-type(ret-ann, info)
             end
  arrow-typ = t-arrow(A.dummy-loc, arg-typs, ret-typ)
  checking(body, ret-typ, info) and arrow-typ.satisfies-type(expect-typ)
end

fun checking(e :: A.Expr, expect-typ :: Type, info :: TCInfo) -> Boolean:
  cases(A.Expr) e:
    | s-module(l, answer, provides, types, checks) =>
      checking(answer, expect-typ, info)
    | s-type-let-expr(l, binds, body) =>
      handle-type-let-binds(binds, info)
      checking(body, expect-typ, info)
    | s-let-expr(l, binds, body) =>
      for each(bind from binds):
        synthesis-let-bind(bind, info)
      end
      checking(body, expect-typ, info)
    | s-letrec(l, binds, body) =>
      raise("s-letrec not yet handled")
    | s-hint-exp(l, hints, exp) =>
      raise("s-hint-exp not yet handled")
    | s-instantiate(l, expr, params) =>
      raise("s-instantiate not yet handled")
    | s-block(l, stmts) =>
      stmts.foldr(lam(curr-stmt, fun-typ):
                    pair(lam(status):
                           result = checking(curr-stmt, fun-typ.right, info)
                           fun-typ.left(status and result)
                         end, t-top)
                  end, pair(identity, expect-typ)).left(true)
    | s-user-block(l, body) =>
      raise("s-user-block not yet handled")
    | s-fun(l, name,
        params, # Type parameters
        args, # Value parameters
        ann, # return type
        doc, body, _check) =>
      check-fun(body, args, ann, expect-typ, info)
    | s-type(l, name, ann) =>
      type-alias   = name.key()
      type-aliased = to-type(ann, info)
      info.typs.set(type-alias, type-aliased)
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
      check-fun(body, args, ann, expect-typ, info)
    | s-method(l,
        args, # Value parameters
        ann, # return type
        doc, body, _check) =>
      check-fun(body, args, ann, expect-typ, info)
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
      id-key = id.key()
      if info.typs.has-key(id-key):
        info.typs.get(id-key).satisfies-type(expect-typ)
      else:
        raise("Identifier not found in environment! Tried to find: " + id-key)
      end
    | s-id-var(l, id) =>
      id-key = id.key()
      if info.typs.has-key(id-key):
        info.typs.get(id-key).satisfies-type(expect-typ)
      else:
        raise("Identifier not found in environment! Tried to find: " + id-key)
      end
    | s-id-letrec(l, id, safe) =>
      id-key = id.key()
      if info.typs.has-key(id-key):
        info.typs.get(id-key).satisfies-type(expect-typ)
      else:
        raise("Identifier not found in environment! Tried to find: " + id-key)
      end
    | s-undefined(l) =>
      raise("s-undefined not yet handled")
    | s-srcloc(l, loc) =>
      raise("s-srcloc not yet handled")
    | s-num(l, n) =>
      t-number.satisfies-type(expect-typ)
    | s-frac(l, num, den) =>
      t-number.satisfies-type(expect-typ)
    | s-bool(l, b) =>
      t-boolean.satisfies-type(expect-typ)
    | s-str(l, s) =>
      t-string.satisfies-type(expect-typ)
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


default-typs = SD.string-dict()
default-typs.set("global#nothing", t-name(A.dummy-loc, none, "tglobal#Nothing"))

fun type-check(program :: A.Program, compile-env :: C.CompileEnvironment) -> C.CompileResult<A.Program>:
  cases(A.Program) program:
    | s-program(l, _provide, provided-types, imports, body) =>
      info = tc-info(default-typs, SD.string-dict(), SD.string-dict())
      result = checking(body, t-top, info)
      if result:
        C.ok(program)
      else:
        raise("Didn't type-check!")
      end
      # cases(C.CompileResult<A.Expr>) result:
      #   | ok(c) =>
      #     C.ok(A.s-program(l, _provide, provided-types, imports, c))
      #   | err(problems) =>
      #     C.err(problems)
      # end
    | else => raise("Attempt to desugar non-program: " + torepr(program))
  end
end
