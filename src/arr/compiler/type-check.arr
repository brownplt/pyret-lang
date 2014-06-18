#lang pyret

provide *
provide-types *
import ast as A
import string-dict as SD
import "compiler/compile-structs.arr" as C
import "compiler/ast-util.arr" as U

data Pair<L,R>:
  | pair(left :: L, right :: R)
sharing:
  on-left(self, f :: (L -> L)) -> Pair<L,R>:
    pair(f(self.left), self.right)
  end,
  on-right(self, f :: (R -> R)) -> Pair<L,R>:
    pair(self.left, f(self.right))
  end
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
  end,
  tostring(self) -> String:
    cases(Type) self:
      | t-name(l, module-name, id) =>
        cases(Option<String>) module-name:
          | none    => id
          | some(m) => m + "." + id
        end
      | t-arrow(l, args, ret) =>
        "(" + args.map(_.tostring()).join-str(", ") + " -> " + ret + ")"
      | t-top =>
        "Top"
      | t-bot =>
        "Bot"
    end
  end,
  toloc(self) -> A.Loc:
    cases(Type) self:
      | t-name(l, module-name, id) => l
      | t-arrow(l, args, ret) => l
      | t-top => A.dummy-loc
      | t-bot => A.dummy-loc
    end
  end
end

t-number  = t-name(A.dummy-loc, none, "tglobal#Number")
t-string  = t-name(A.dummy-loc, none, "tglobal#String")
t-boolean = t-name(A.dummy-loc, none, "tglobal#Boolean")

data TCInfo:
  | tc-info(typs       :: SD.StringDict,
            aliases    :: SD.StringDict,
            data-exprs :: SD.StringDict,
            errors     :: { insert :: (C.CompileError -> List<C.CompileError>), get :: (-> List<C.CompileError>)})
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

fun ensure-satisfies(typ :: Type, ann :: A.Ann, info :: TCInfo) -> Type:
  if A.is-a-blank(ann):
    typ
  else:
    expect-typ = to-type(ann, info)
    check-and-return(typ, expect-typ, expect-typ, info)
  end
end

fun handle-type-let-binds(binds :: List<A.TypeLetBind>, info :: TCInfo):
  for map(bind from binds):
    cases(A.TypeLetBind) bind:
      | s-type-bind(_, name, ann) =>
        info.aliases.set(name.key(), to-type(ann, info))
      | s-newtype-bind(_, name, namet) =>
        raise("newtype not yet handled!")
    end
  end
end


fun synthesis-fun(l :: A.Loc, body :: A.Expr, args :: List<A.Bind>, ret-ann :: A.Ann, recreate :: (List<A.Bind>, A.Ann, A.Expr -> A.Expr), info :: TCInfo) -> Pair<A.Expr, Type>:
  arg-typs = args.foldr(lam(arg :: A.Bind, base :: List<Type>):
                          arg-typ = to-type(arg.ann, info)
                          info.typs.set(arg.id.key(), arg-typ)
                          link(arg-typ, base)
                        end, empty)
  new-body = synthesis(body, info)
  ret-typ  = ensure-satisfies(new-body.right, ret-ann, info)
  new-fun  = recreate(args, ret-ann, new-body.left)
  fun-typ  = t-arrow(l, arg-typs, ret-typ)
  pair(new-fun, fun-typ)
end

fun lookup-id(id :: A.Name, info :: TCInfo) -> Type:
  id-key = id.key()
  if info.typs.has-key(id-key):
    info.typs.get(id-key)
  else:
    raise("Identifier not found in environment! Tried to find: " + id-key)
  end
end

fun synthesis(e :: A.Expr, info :: TCInfo) -> Pair<A.Expr, Type>:
  cases(A.Expr) e:
    | s-module(l, answer, provides, types, checks) =>
      raise("s-module not yet handled")
    | s-type-let-expr(l, binds, body) =>
      handle-type-let-binds(binds, info)
      new-body = synthesis(body, info)
      new-body.on-left(A.s-type-let-expr(l, binds, _))
    | s-let-expr(l, binds, body) =>
      new-binds = for map(bind from binds):
                    synthesis-let-bind(bind, info).left
                  end
      new-body = synthesis(body, info)
      new-body.on-left(A.s-let-expr(l, new-binds, _))
    | s-letrec(l, binds, body) =>
      raise("s-letrec not yet handled")
    | s-hint-exp(l, hints, exp) =>
      raise("s-hint-exp not yet handled")
    | s-instantiate(l, expr, params) =>
      raise("s-instantiate not yet handled")
    | s-block(l, stmts) =>
      var typ = t-top
      new-stmts = for map(stmt from stmts):
        result = synthesis(stmt, info)
        typ := result.right
        result.left
      end
      pair(A.s-block(l, new-stmts), typ)
    | s-user-block(l, body) =>
      raise("s-user-block not yet handled")
    | s-fun(l, name,
        params, # Type parameters
        args, # Value parameters
        ann, # return type
        doc, body, _check) =>
      synthesis-fun(l, body, args, ann, A.s-fun(l, name, params, _, _, doc, _, _check), info)
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
      synthesis-fun(l, body, args, ann, A.s-lam(l, params, _, _, doc, _, _check), info)
    | s-method(l,
        args, # Value parameters
        ann, # return type
        doc, body, _check) =>
      synthesis-fun(l, body, args, ann, A.s-method(l, _, _, doc, _, _check), info)
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
      new-fun = synthesis(_fun, info)
      fun-typ = new-fun.right
      cases(Type) fun-typ:
        | t-arrow(_, arg-typs, ret-typ) =>
          new-args = for lists.map2(arg from args, 
                                    arg-typ from arg-typs):
                       checking(arg, arg-typ, info)
                     end
          pair(A.s-app(l, new-fun.left, new-args), ret-typ)
        | else =>
          raise("Cannot apply a non-function! Found type: " + torepr(fun-typ))
      end
    | s-prim-app(l, _fun, args) =>
      raise("s-prim-app not yet handled")
    | s-prim-val(l, name) =>
      raise("s-prim-val not yet handled")
    | s-id(l, id) =>
      pair(e, lookup-id(id, info))
    | s-id-var(l, id) =>
      pair(e, lookup-id(id, info))
    | s-id-letrec(l, id, safe) =>
      pair(e, lookup-id(id, info))
    | s-undefined(l) =>
      raise("s-undefined not yet handled")
    | s-srcloc(l, loc) =>
      raise("s-srcloc not yet handled")
    | s-num(l, n) =>
      pair(e, t-number)
    | s-frac(l, num, den) =>
      pair(e, t-number)
    | s-bool(l, b) =>
      pair(e, t-boolean)
    | s-str(l, s) =>
      pair(e, t-string)
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

fun synthesis-binding(binding :: A.Bind, value :: A.Expr, recreate :: (A.Bind, A.Expr -> A.Expr), info :: TCInfo) -> Pair<A.Expr, Type>:
  new-value = synthesis(value, info)
                .on-left(recreate(binding, _))
                .on-right(ensure-satisfies(_, binding.ann, info))
  info.typs.set(binding.id.key(), new-value.right)
  new-value
end

fun synthesis-let-bind(binding :: A.LetBind, info :: TCInfo) -> Pair<A.Expr, Type>:
  cases(A.LetBind) binding:
    | s-let-bind(l, b, value) =>
      synthesis-binding(b, value, A.s-let-bind(l, _, _), info)
    | s-var-bind(l, b, value) =>
      raise("s-var-bind not yet handled")
  end
end

fun check-fun(body :: A.Expr, args :: List<A.Bind>, ret-ann :: A.Ann, expect-typ :: Type, recreate :: (List<A.Bind>, A.Ann, A.Expr -> A.Expr), info :: TCInfo) -> A.Expr:
  arg-typs = cases(Type) expect-typ:
               | t-arrow(_, expect-args, _) =>
                 for map2(arg from args, expect-arg from expect-args):
                   arg-typ = to-type-or(arg.ann, expect-arg, info)
                   info.typs.set(arg.id.key(), arg-typ)
                   arg-typ
                 end
               | else =>
                 for map(arg from args):
                   arg-typ = to-type(arg.ann, info)
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
  new-body  = checking(body, ret-typ, info) 
  new-fun   = recreate(args, ret-ann, new-body)
  check-and-return(arrow-typ, expect-typ, new-fun, info)
end

fun <V> check-and-return(typ :: Type, expect-typ :: Type, value :: V, info :: TCInfo) -> V:
  when not(typ.satisfies-type(expect-typ)):
    info.errors.insert(C.incorrect-type(typ.tostring(), typ.toloc(), expect-typ.tostring(), expect-typ.toloc()))
  end
  value
end

fun checking(e :: A.Expr, expect-typ :: Type, info :: TCInfo) -> A.Expr:
  cases(A.Expr) e:
    | s-module(l, answer, provides, types, checks) =>
      new-answer = checking(answer, expect-typ, info)
      A.s-module(l, new-answer, provides, types, checks)
    | s-type-let-expr(l, binds, body) =>
      handle-type-let-binds(binds, info)
      new-body = checking(body, expect-typ, info)
      A.s-type-let-expr(l, binds, new-body)
    | s-let-expr(l, binds, body) =>
      new-binds = for map(bind from binds):
                    synthesis-let-bind(bind, info).left
                  end
      new-body  = checking(body, expect-typ, info)
      A.s-let-expr(l, new-binds, new-body)
    | s-letrec(l, binds, body) =>
      for each(bind from binds):
        info.typs.set(bind.b.id.key(), to-type(bind.b.ann, info))
      end
      new-binds = for map(bind from binds):
                    cases(A.LetrecBind) bind:
                      | s-letrec-bind(l2, b, value) =>
                        recreate = A.s-letrec-bind(l2, _, _)
                        synthesis-binding(b, value, recreate, info).left
                    end
                  end
      new-body  = checking(body, expect-typ, info)
      A.s-letrec(l, new-binds, new-body)
    | s-hint-exp(l, hints, exp) =>
      raise("s-hint-exp not yet handled")
    | s-instantiate(l, expr, params) =>
      raise("s-instantiate not yet handled")
    | s-block(l, stmts) =>
      fun gen(curr-stmt, fun-typ):
        pair(lam():
               link(checking(curr-stmt, fun-typ.right, info),
                    fun-typ.left())
             end, t-top)
      end
      fun just-empty():
        empty
      end
      thunk = stmts.foldr(gen, pair(just-empty, expect-typ)).left
      A.s-block(l, thunk())
    | s-user-block(l, body) =>
      raise("s-user-block not yet handled")
    | s-fun(l, name,
        params, # Type parameters
        args, # Value parameters
        ann, # return type
        doc, body, _check) =>
      check-fun(body, args, ann, expect-typ, A.s-fun(l, name, _, _, doc, _, _check), info)
    | s-type(l, name, ann) =>
      type-alias   = name.key()
      type-aliased = to-type(ann, info)
      info.aliases.set(type-alias, type-aliased)
      e
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
      check-fun(body, args, ann, expect-typ, A.s-lam(l, params, _, _, doc, _, _check), info)
    | s-method(l,
        args, # Value parameters
        ann, # return type
        doc, body, _check) =>
      check-fun(body, args, ann, expect-typ, A.s-method(l, _, _, doc, _, _check), info)
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
      new-fun = synthesis(_fun, info)
      fun-typ = new-fun.right
      cases(Type) fun-typ:
        | t-arrow(_, arg-typs, ret-typ) =>
          new-args = for lists.map2(arg from args, arg-typ from arg-typs):
            checking(arg, arg-typ, info)
          end
          ast = A.s-app(l, new-fun.left, new-args)
          check-and-return(ret-typ, expect-typ, ast, info)
        | else =>
          raise("Cannot apply a non-function! Found type: " + torepr(fun-typ))
      end
    | s-prim-app(l, _fun, args) =>
      raise("s-prim-app not yet handled")
    | s-prim-val(l, name) =>
      raise("s-prim-val not yet handled")
    | s-id(l, id) =>
      check-and-return(lookup-id(id, info), expect-typ, e, info)
    | s-id-var(l, id) =>
      check-and-return(lookup-id(id, info), expect-typ, e, info)
    | s-id-letrec(l, id, safe) =>
      check-and-return(lookup-id(id, info), expect-typ, e, info)
    | s-undefined(l) =>
      raise("s-undefined not yet handled")
    | s-srcloc(l, loc) =>
      raise("s-srcloc not yet handled")
    | s-num(l, n) =>
      check-and-return(t-number, expect-typ, e, info)
    | s-frac(l, num, den) =>
      check-and-return(t-number, expect-typ, e, info)
    | s-bool(l, b) =>
      check-and-return(t-boolean, expect-typ, e, info)
    | s-str(l, s) =>
      check-and-return(t-string, expect-typ, e, info)
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
  errors = lam():
    var err-list = empty
    {
      insert: lam(err :: C.CompileError):
        err-list := link(err, err-list)
      end,
      get: lam():
        err-list
      end
    }
  end()

  cases(A.Program) program:
    | s-program(l, _provide, provided-types, imports, body) =>
      info = tc-info(default-typs, SD.string-dict(), SD.string-dict(), errors)
      new-body = checking(body, t-top, info)
      err-list = info.errors.get()
      cases(List<C.CompileError>) err-list:
        | empty =>
          C.ok(A.s-program(l, _provide, provided-types, imports, new-body))
        | link(_, _) =>
          C.err(err-list)
      end
    | else => raise("Attempt to type-check non-program: " + torepr(program))
  end
end
