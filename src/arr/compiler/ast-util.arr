#lang pyret

provide *
provide-types *
import srcloc as SL
import ast as A
import parse-pyret as PP
import "compiler/compile-structs.arr" as CS
import "compiler/ast-anf.arr" as N
import "compiler/type-structs.arr" as T
import "compiler/type-check-structs.arr" as TCS
import string-dict as SD
import either as E

type URI = String
type Loc = SL.Srcloc

fun ok-last(stmt):
  not(
    A.is-s-let(stmt) or
    A.is-s-var(stmt) or
    A.is-s-rec(stmt) or
    A.is-s-fun(stmt) or
    A.is-s-data(stmt) or
    A.is-s-contract(stmt) or
    A.is-s-check(stmt) or
    A.is-s-type(stmt) or
    A.is-s-newtype(stmt)
  )
end

fun checkers(l): A.s-app(l, A.s-dot(l, A.s-id(l, A.s-name(l, "builtins")), "current-checker"), [list: ]) end

fun append-nothing-if-necessary(prog :: A.Program) -> Option<A.Program>:
  cases(A.Program) prog:
    | s-program(l1, _provide, _provide-types, imports, body) =>
      cases(A.Expr) body:
        | s-block(l2, stmts) =>
          cases(List) stmts:
            | empty =>
              some(A.s-program(l1, _provide, _provide-types, imports, A.s-block(l2, [list: A.s-id(l2, A.s-name(l2, "nothing"))])))
            | link(_, _) =>
              last-stmt = stmts.last()
              if ok-last(last-stmt): none
              else:
                some(A.s-program(l1, _provide, _provide-types, imports,
                    A.s-block(l2, stmts + [list: A.s-id(A.dummy-loc, A.s-name(l2, "nothing"))])))
              end
          end
        | else => none
      end
  end
end

flatten-single-blocks = A.default-map-visitor.{
    s-block(self, l, stmts):
      if stmts.length() == 1: stmts.first.visit(self)
      else: A.s-block(l, stmts.map(_.visit(self)))
      end
    end
  }

check:
  d = A.dummy-loc
  PP.surface-parse("x", "test").block.visit(flatten-single-blocks).visit(A.dummy-loc-visitor)
    is A.s-id(d, A.s-name(d, "x"))
end

merge-nested-blocks = A.default-map-visitor.{
    s-block(self, l, stmts):
      merged-stmts = for fold(new-stmts from [list: ], s from stmts):
        cases(A.Expr) s.visit(self):
          | s-block(l2, stmts2) => stmts2.reverse() + new-stmts
          | else => [list: s] + new-stmts
        end
      end
      A.s-block(l, merged-stmts.reverse())
    end
  }


fun count-apps(expr):
  var count = 0
  visitor = A.default-iter-visitor.{
      s-app(self, l, f, args):
        count := count + 1
        f.visit(self) and args.map(_.visit(self))
      end
    }
  expr.visit(visitor)
  count
end

data BindingInfo:
  | b-prim(name :: String) # Some "primitive" value supplied by the initial environment
  | b-dict(dict :: SD.StringDict) # Some module supplied by the initial environment
  | b-exp(exp :: A.Expr) # This name is bound to some expression that we can't interpret yet
  | b-dot(base :: BindingInfo, name :: String) # A field lookup off some binding that isn't a b-dict
  | b-typ # A type
  | b-import(imp :: A.ImportType) # imported from a module
  | b-unknown # Any unknown value
end

data Binding:
  | e-bind(loc :: Loc, mut :: Boolean, info :: BindingInfo)
end

fun bind-exp(e :: A.Expr, env) -> Option<Binding>:
  cases(A.Expr) e:
    | s-dot(l, o, name) =>
      cases(Option<Binding>) bind-exp(o, env):
        | some(eb) =>
          b = eb.info
          cases(BindingInfo) b:
            | b-dict(dict) =>
              if dict.has-key(name.key()): some(e-bind(A.dummy-loc, false, dict.get-value(name.key())))
              else: some(e-bind(A.dummy-loc, false, b-dot(b, name)))
              end
            | else => some(e-bind(A.dummy-loc, false, b-dot(b, name)))
          end
        | none => none
      end
    | s-id(_, name) =>
      if env.has-key(name.key()): some(env.get-value(name.key()))
      else: none
      end
    | s-id-var(_, name) =>
      if env.has-key(name.key()): some(env.get-value(name.key()))
      else: none
      end
    | s-id-letrec(_, name, _) =>
      if env.has-key(name.key()): some(env.get-value(name.key()))
      else: none
      end
    | else => some(e-bind(A.dummy-loc, false, b-exp(e)))
  end
end

fun bind-or-unknown(e :: A.Expr, env) -> BindingInfo:
  cases(Option<Binding>) bind-exp(e, env):
    | none => b-unknown
    | some(b) =>
      when not(is-e-bind(b)):
        print-error("b isn't a binding for expr " + string-substring(torepr(e), 0, 100))
        print-error(b)
      end
      b.info
  end
end

fun binding-type-env-from-env(env):
  for lists.fold(acc from SD.make-string-dict(), name from env.globals.types.keys-list()):
    acc.set(A.s-type-global(name).key(), e-bind(A.dummy-loc, false, b-typ))
  end
end
fun binding-env-from-env(env):
  for lists.fold(acc from SD.make-string-dict(), name from env.globals.values.keys-list()):
    acc.set(A.s-global(name).key(), e-bind(A.dummy-loc, false, b-prim(name)))
  end
end

fun default-env-map-visitor<a, c>(
    initial-env :: a,
    initial-type-env :: c,
    bind-handlers :: {
        s-letrec-bind :: (A.LetrecBind, a -> a),
        s-let-bind :: (A.LetBind, a -> a),
        s-bind :: (A.Bind, a -> a),
        s-header :: (A.Header, a, c -> { val-env :: a, type-env :: c }),
        s-type-let-bind :: (A.TypeLetBind, a, c -> { val-env :: a, type-env :: c }),
        s-param-bind :: (Loc, A.Name, c -> c)
      }
    ):
  A.default-map-visitor.{
    env: initial-env,
    type-env: initial-type-env,

    s-program(self, l, _provide, _provide-types, imports, body):
      visit-provide = _provide.visit(self)
      visit-provide-types = _provide-types.visit(self)
      visit-imports = for map(i from imports):
        i.visit(self)
      end
      new-envs = { val-env: self.env, type-env: self.type-env }
      imported-envs = for fold(acc from new-envs, i from visit-imports):
        bind-handlers.s-header(i, acc.val-env, acc.type-env)
      end
      visit-body = body.visit(self.{env: imported-envs.val-env, type-env: imported-envs.type-env })
      A.s-program(l, visit-provide, visit-provide-types, visit-imports, visit-body)
    end,
    s-type-let-expr(self, l, binds, body):
      new-envs = { val-env: self.env, type-env: self.type-env }
      bound-env = for lists.fold(acc from new-envs.{ bs: [list: ] }, b from binds):
        updated = bind-handlers.s-type-let-bind(b, acc.val-env, acc.type-env)
        visit-envs = self.{ env: updated.val-env, type-env: updated.type-env }
        new-bind = b.visit(visit-envs)
        updated.{ bs: link(new-bind, acc.bs) }
      end
      A.s-type-let-expr(l, bound-env.bs, body.visit(self.{ env: bound-env.val-env, type-env: bound-env.type-env }))
    end,
    s-let-expr(self, l, binds, body):
      bound-env = for fold(acc from { e: self.env, bs : [list: ] }, b from binds):
        new-bind = b.visit(self.{env : acc.e})
        this-env = bind-handlers.s-let-bind(new-bind, acc.e)
        {
          e: this-env,
          bs: link(new-bind, acc.bs)
        }
      end
      visit-binds = bound-env.bs.reverse()
      visit-body = body.visit(self.{env: bound-env.e})
      A.s-let-expr(l, visit-binds, visit-body)
    end,
    s-letrec(self, l, binds, body):
      bind-env = for fold(acc from self.env, b from binds):
        bind-handlers.s-letrec-bind(b, acc)
      end
      new-visitor = self.{env: bind-env}
      visit-binds = binds.map(_.visit(new-visitor))
      visit-body = body.visit(new-visitor)
      A.s-letrec(l, visit-binds, visit-body)
    end,
    s-lam(self, l, params, args, ann, doc, body, _check):
      new-type-env = for lists.fold(acc from self.type-env, param from params):
        bind-handlers.s-param-bind(l, param, acc)
      end
      with-params = self.{type-env: new-type-env}
      new-args = args.map(_.visit(with-params))
      args-env = for lists.fold(acc from with-params.env, new-arg from args):
        bind-handlers.s-bind(new-arg, acc)
      end
      with-args = with-params.{env: args-env}
      new-body = body.visit(with-args)
      new-check = with-args.option(_check)
      A.s-lam(l, params, new-args, ann.visit(with-args), doc, new-body, new-check)
    end,
    s-cases-else(self, l, typ, val, branches, _else):
      A.s-cases-else(l, typ.visit(self), val.visit(self), branches.map(_.visit(self)), _else.visit(self))
    end,
    s-cases-branch(self, l, pat-loc, name, args, body):
      new-args = args.map(_.visit(self))
      args-env = for lists.fold(acc from self.env, arg from args.map(_.bind)):
        bind-handlers.s-bind(arg, acc)
      end
      A.s-cases-branch(l, pat-loc, name, new-args, body.visit(self.{env: args-env}))
    end,
    s-singleton-cases-branch(self, l, pat-loc, name, body):
      A.s-singleton-cases-branch(l, pat-loc, name, body.visit(self))
    end,
    s-data-expr(self, l, name, namet, params, mixins, variants, shared-members, _check):
      new-type-env = for lists.fold(acc from self.type-env, param from params):
        bind-handlers.s-param-bind(l, param, acc)
      end
      with-params = self.{type-env: new-type-env}
      A.s-data-expr(l, name, namet.visit(with-params), params,
        mixins.map(_.visit(with-params)), variants.map(_.visit(with-params)),
        shared-members.map(_.visit(with-params)), with-params.option(_check))
    end,
    s-method(self, l, params, args, ann, doc, body, _check):
      new-type-env = for lists.fold(acc from self.type-env, param from params):
        bind-handlers.s-param-bind(l, param, acc)
      end
      with-params = self.{type-env: new-type-env}
      new-args = args.map(_.visit(with-params))
      args-env = for lists.fold(acc from with-params.env, arg from new-args):
        bind-handlers.s-bind(arg, acc)
      end
      new-body = body.visit(with-params.{env: args-env})
      new-check = with-params.{env: args-env}.option(_check)
      A.s-method(l, params, new-args, ann.visit(with-params.{env: args-env}), doc, new-body, new-check)
    end
  }
end


fun default-env-iter-visitor<a, c>(
    initial-env :: a,
    initial-type-env :: c,
    bind-handlers :: {
        s-letrec-bind :: (A.LetrecBind, a -> a),
        s-let-bind :: (A.LetBind, a -> a),
        s-bind :: (A.Bind, a -> a),
        s-header :: (A.Header, a, c -> { val-env :: a, type-env :: c }),
        s-type-let-bind :: (A.TypeLetBind, a, c -> { val-env :: a, type-env :: c }),
        s-param-bind :: (Loc, A.Name, c -> c)
      }
    ):
  A.default-iter-visitor.{
    env: initial-env,
    type-env: initial-type-env,

    s-program(self, l, _provide, _provide-types, imports, body):
      if _provide.visit(self) and _provide-types.visit(self):
        new-envs = { val-env: self.env, type-env: self.type-env }
        imported-envs = for fold(acc from new-envs, i from imports):
          bind-handlers.s-header(i, acc.val-env, acc.type-env)
        end
        new-visitor = self.{ env: imported-envs.val-env, type-env: imported-envs.type-env }
        lists.all(_.visit(new-visitor), imports) and body.visit(new-visitor)
      else:
        false
      end
    end,
    s-type-let-expr(self, l, binds, body):
      new-envs = { val-env: self.env, type-env: self.type-env }
      bound-env = for lists.fold-while(acc from new-envs.{ bs: true }, b from binds):
        updated = bind-handlers.s-type-let-bind(b, acc.val-env, acc.type-env)
        visit-envs = self.{ env: updated.val-env, type-env: updated.type-env }
        new-bind = b.visit(visit-envs)
        if new-bind:
          E.left(updated.{ bs: true })
        else:
          E.right(updated.{ bs: false})
        end
      end
      bound-env.bs and body.visit(self.{ env: bound-env.val-env, type-env: bound-env.type-env })
    end,
    s-let-expr(self, l, binds, body):
      bound-env = for lists.fold-while(acc from { e: self.env, bs: true }, b from binds):
        this-env = bind-handlers.s-let-bind(b, acc.e)
        new-bind = b.visit(self.{env : acc.e})
        if new-bind:
          E.left({ e: this-env, bs: true })
        else:
          E.right({ e: this-env, bs: false })
        end
      end
      bound-env.bs and body.visit(self.{env: bound-env.e})
    end,
    s-letrec(self, l, binds, body):
      bind-env = for lists.fold(acc from self.env, b from binds):
        bind-handlers.s-letrec-bind(b, acc)
      end
      new-visitor = self.{env: bind-env}
      continue-binds = for lists.fold-while(acc from true, b from binds):
        if b.visit(new-visitor): E.left(true) else: E.right(false) end
      end
      continue-binds and body.visit(new-visitor)
    end,
    s-lam(self, l, params, args, ann, doc, body, _check):
      new-type-env = for lists.fold(acc from self.type-env, param from params):
        bind-handlers.s-param-bind(l, param, acc)
      end
      with-params = self.{type-env: new-type-env}
      visit-args = lists.all(_.visit(with-params), args)
      args-env = for lists.fold(acc from with-params.env, arg from args):
        bind-handlers.s-bind(arg, acc)
      end
      with-args = with-params.{env: args-env}
      visit-args and
        ann.visit(with-args) and
        body.visit(with-args) and
        with-args.option(_check)
    end,
    s-cases-else(self, l, typ, val, branches, _else):
      typ.visit(self)
      and val.visit(self)
      and lists.all(_.visit(self), branches)
      and _else.visit(self)
    end,
    s-cases-branch(self, l, pat-loc, name, args, body):
      visit-args = lists.all(_.visit(self), args)
      args-env = for lists.fold(acc from self.env, arg from args.map(_.bind)):
        bind-handlers.s-bind(arg, acc)
      end
      visit-args
      and body.visit(self.{env: args-env})
    end,
    # s-singleton-cases-branch introduces no new bindings, so default visitor is fine
    s-data-expr(self, l, name, namet, params, mixins, variants, shared-members, _check):
      new-type-env = for lists.fold(acc from self.type-env, param from params):
        bind-handlers.s-param-bind(l, param, acc)
      end
      with-params = self.{type-env: new-type-env}
      namet.visit(with-params)
      and lists.all(_.visit(with-params), mixins)
      and lists.all(_.visit(with-params), variants)
      and lists.all(_.visit(with-params), shared-members)
      and with-params.option(_check)
    end,
    s-method(self, l, params, args, ann, doc, body, _check):
      new-type-env = for lists.fold(acc from self.type-env, param from params):
        bind-handlers.s-param-bind(l, param, acc)
      end
      with-params = self.{type-env: new-type-env}
      args-env = for lists.fold(acc from self.env, arg from args):
        bind-handlers.s-bind(arg, acc)
      end
      lists.all(_.visit(with-params), args) and
        ann.visit(with-params.{env: args-env}) and
        body.visit(with-params.{env: args-env}) and
        with-params.{env: args-env}.option(_check)
    end
  }
end

binding-handlers = {
  s-header(_, imp, env, type-env):
    with-vname = env.set(imp.vals-name.key(), e-bind(imp.l, false, b-unknown))
    with-tname = type-env.set(imp.types-name.key(), e-bind(imp.l, false, b-typ))
    with-vnames = for fold(venv from with-vname, v from imp.values):
      venv.set(v.key(), e-bind(imp.l, false, b-import(imp.import-type)))
    end
    with-tnames = for fold(tenv from with-tname, t from imp.types):
      tenv.set(t.key(), e-bind(imp.l, false, b-import(imp.import-type)))
    end
    {
      val-env: with-vnames,
      type-env: with-tnames
    }
  end,
  s-param-bind(_, l, param, type-env):
    type-env.set(param.key(), e-bind(l, false, b-typ))
  end,
  s-type-let-bind(_, tlb, env, type-env):
    cases(A.TypeLetBind) tlb:
      | s-type-bind(l, name, ann) =>
        {
          val-env: env,
          type-env: type-env.set(name.key(), e-bind(l, false, b-typ))
        }
      | s-newtype-bind(l, tname, bname) =>
        {
          val-env: env.set(bname.key(), e-bind(l, false, b-unknown)),
          type-env: type-env.set(tname.key(), e-bind(l, false, b-typ))
        }
    end
  end,
  s-let-bind(_, lb, env):
    cases(A.LetBind) lb:
      | s-let-bind(l2, bind, val) =>
        env.set(bind.id.key(), e-bind(l2, false, bind-or-unknown(val, env)))
      | s-var-bind(l2, bind, val) =>
        env.set(bind.id.key(), e-bind(l2, true, b-unknown))
    end
  end,
  s-letrec-bind(_, lrb, env):
    env.set(lrb.b.id.key(),
      e-bind(lrb.l, false, bind-or-unknown(lrb.value, env)))
  end,
  s-bind(_, b, env):
    env.set(b.id.key(), e-bind(b.l, false, b-unknown))
  end
}
fun binding-env-map-visitor(initial-env):
  default-env-map-visitor(binding-env-from-env(initial-env), binding-type-env-from-env(initial-env), binding-handlers)
end
fun binding-env-iter-visitor(initial-env):
  default-env-iter-visitor(binding-env-from-env(initial-env), binding-type-env-from-env(initial-env), binding-handlers)
end

fun link-list-visitor(initial-env):
  binding-env-map-visitor(initial-env).{
    s-app(self, l, f, args):
      if A.is-s-dot(f) and (f.field == "_plus"):
        target = f.obj
        cases(A.Expr) target:
          | s-app(l2, lnk, _args) =>
            cases(BindingInfo) bind-or-unknown(lnk, self.env):
              | b-prim(n) =>
                if n == "list:link":
                  A.s-app(l2, lnk, [list: _args.first,
                      A.s-app(l, A.s-dot(f.l, _args.rest.first, f.field), args).visit(self)]) 
                else if n == "list:empty":
                  args.first.visit(self)
                else:
                  A.s-app(l, f.visit(self), args.map(_.visit(self)))
                end
              | else =>
                A.s-app(l, f.visit(self), args.map(_.visit(self)))
            end
          | s-id(_, _) =>
            cases(BindingInfo) bind-or-unknown(target, self.env):
              | b-prim(name) =>
                if (name == "list:empty"):
                  args.first.visit(self)
                else:
                  A.s-app(l, f.visit(self), args.map(_.visit(self)))
                end
              | else =>
                A.s-app(l, f.visit(self), args.map(_.visit(self)))
            end
          | s-dot(_, _, _) =>
            cases(BindingInfo) bind-or-unknown(target, self.env):
              | b-prim(name) =>
                if (name == "list:empty"):
                  args.first.visit(self)
                else:
                  A.s-app(l, f.visit(self), args.map(_.visit(self)))
                end
              | else =>
                A.s-app(l, f.visit(self), args.map(_.visit(self)))
            end
          | else =>
            A.s-app(l, f.visit(self), args.map(_.visit(self)))
        end
      else:
        A.s-app(l, f.visit(self), args.map(_.visit(self)))
      end
    end
  }
end

fun bad-assignments(initial-env, ast):
  var errors = [list: ] # THE MUTABLE LIST OF ERRORS
  fun add-error(err): errors := err ^ link(_, errors) end
  ast.visit(binding-env-iter-visitor(initial-env).{
    s-assign(self, loc, id, value):
      cases(Option<Binding>) bind-exp(A.s-id(loc, id), self.env):
        | none => nothing
        | some(b) =>
          when not(b.mut):
            add-error(CS.bad-assignment(id.toname(), loc, b.loc))
          end
      end
      value.visit(self)
    end,
  })
  errors
end

inline-lams = A.default-map-visitor.{
  s-app(self, loc, f, exps):
    cases(A.Expr) f:
      | s-lam(l, _, args, ann, _, body, _) =>
        if (args.length() == exps.length()):
          a = A.global-names.make-atom("inline_body")
          let-binds = for lists.map2(arg from args, exp from exps):
            A.s-let-bind(arg.l, arg, exp.visit(self))
          end
          cases(A.Ann) ann:
            | a-blank => A.s-let-expr(l, let-binds, body.visit(self))
            | a-any => A.s-let-expr(l, let-binds, body.visit(self))
            | else =>
              A.s-let-expr(l,
                let-binds
                  + [list: A.s-let-bind(body.l, A.s-bind(l, false, a, ann), body.visit(self))],
                A.s-id(l, a))
          end
        else:
          A.s-app(loc, f.visit(self), exps.map(_.visit(self)))
        end
      | else => A.s-app(loc, f.visit(self), exps.map(_.visit(self)))
    end
  end
}

fun check-unbound(initial-env, ast):
  var errors = [list: ] # THE MUTABLE LIST OF UNBOUND IDS
  fun add-error(err): errors := err ^ link(_, errors) end
  fun handle-id(this-id, env):
    if A.is-s-underscore(this-id.id):
      add-error(CS.underscore-as-expr(this-id.id.l))
    else if is-none(bind-exp(this-id, env)):
      add-error(CS.unbound-id(this-id))
    else:
      nothing
    end
  end
  fun handle-type-id(ann, env):
    if A.is-s-underscore(ann.id):
      add-error(CS.underscore-as-ann(ann.id.l))
    else if not(env.has-key(ann.id.key())):
      add-error(CS.unbound-type-id(ann))
    else:
      nothing
    end
  end
  ast.visit(binding-env-iter-visitor(initial-env).{
      s-id(self, loc, id):
        handle-id(A.s-id(loc, id), self.env)
        true
      end,
      s-id-var(self, loc, id):
        handle-id(A.s-id-var(loc, id), self.env)
        true
      end,
      s-id-letrec(self, loc, id, safe):
        handle-id(A.s-id-letrec(loc, id, safe), self.env)
        true
      end,
      s-assign(self, loc, id, value):
        when is-none(bind-exp(A.s-id(loc, id), self.env)):
          add-error(CS.unbound-var(id.toname(), loc))
        end
        value.visit(self)
      end,
      a-name(self, loc, id):
        handle-type-id(A.a-name(loc, id), self.type-env)
        true
      end,
      a-dot(self, loc, name, field):
        handle-type-id(A.a-name(loc, name), self.type-env)
        true
      end
    })
  errors
where:
  p = PP.surface-parse(_, "test")
  unbound1 = check-unbound(CS.no-builtins, p("x"))
  unbound1.length() is 1

end

fun value-delays-exec-of(name, expr):
  A.is-s-lam(expr) or A.is-s-method(expr)
end

letrec-visitor = A.default-map-visitor.{
  env: SD.make-string-dict(),
  s-letrec(self, l, binds, body):
    bind-envs = for map2(b1 from binds, i from range(0, binds.length())):
      rhs-is-delayed = value-delays-exec-of(b1.b.id, b1.value)
      for fold2(acc from self.env, b2 from binds, j from range(0, binds.length())):
        key = b2.b.id.key()
        if i < j:
          acc.set(key, false)
        else if i == j:
          acc.set(key, rhs-is-delayed)
        else:
          acc.set(key, true)
        end
      end
    end
    new-binds = for map2(b from binds, bind-env from bind-envs):
      b.visit(self.{ env: bind-env })
    end
    body-env = bind-envs.last().set(binds.last().b.id.key(), true)
    new-body = body.visit(self.{ env: body-env })
    A.s-letrec(l, new-binds, new-body)
  end,
  s-id-letrec(self, l, id, _):
    A.s-id-letrec(l, id, self.env.get-value(id.key()))
  end
}

fun make-renamer(replacements :: SD.StringDict):
  A.default-map-visitor.{
    s-atom(self, base, serial):
      a = A.s-atom(base, serial)
      k = a.key()
      if replacements.has-key(k):
        replacements.get-value(k)
      else:
        a
      end
    end
  }
end

fun wrap-extra-imports(p :: A.Program, env :: CS.ExtraImports) -> A.Program:
  expr = p.block
  cases(CS.ExtraImports) env:
    | extra-imports(imports) =>
      full-imports = p.imports + for map(i from imports):
          cases(CS.Dependency) i.dependency:
            | builtin(name) =>
              A.s-import-complete(
                p.l,
                i.values.map(A.s-name(p.l, _)),
                i.types.map(A.s-name(p.l, _)),
                A.s-const-import(p.l, name),
                A.s-name(p.l, i.as-name),
                A.s-name(p.l, i.as-name))
            | dependency(protocol, args) =>
              A.s-import-complete(
                p.l,
                i.values.map(A.s-name(p.l, _)),
                i.types.map(A.s-name(p.l, _)),
                A.special-import(p.l, protocol, args),
                A.s-name(p.l, i.as-name),
                A.s-name(p.l, i.as-name))
          end
        end
      A.s-program(p.l, p._provide, p.provided-types, full-imports, p.block)
  end
end

fun import-to-dep(imp):
  cases(A.ImportType) imp:
    # crossover compatibility
    | s-file-import(_, path) => CS.dependency("legacy-path", [list: path])
    | s-const-import(_, modname) => CS.builtin(modname)
    | s-special-import(_, protocol, args) => CS.dependency(protocol, args)
  end
end

fun import-to-dep-anf(imp):
  cases(N.AImportType) imp:
    | a-import-builtin(_, name) => CS.builtin(name)
    | a-import-file(_, name) => CS.dependency("legacy-path", [list: name])
    | a-import-special(_, kind, args) => CS.dependency(kind, args)
  end
end

fun some-pred<a>(pred :: (a -> Boolean), o :: Option<a>) -> a:
  cases(Option) o:
    | none => raise("Expected some but got none")
    | some(exp) =>
      when not(pred(exp)):
        raise("Predicate failed for " + torepr(exp))
      end
      exp
  end
end

is-s-data-expr = A.is-s-data-expr
mt-dict = [SD.string-dict:]

fun get-named-provides(resolved :: CS.NameResolution, uri :: URI, compile-env :: CS.CompileEnvironment) -> CS.Provides:
  fun field-to-typ(f :: A.AField) -> T.TypeMember:
    T.t-member(f.name, ann-to-typ(f.ann))
  end

  fun collect-shared-fields(vs :: List<A.Variant>):
    init-members = for fold(sd from mt-dict, m from vs.first.with-members):
      sd.set(m.name, member-to-t-member(m))
    end

    shared-across-variants = for fold(sd from init-members, v from vs.rest):
      for fold(shadow sd from sd, m from v.with-members):
        name = m.name
        if not(sd.has-key(name)): sd.remove(name)
        else:
          existing-mem = sd.get-value(name)
          this-mem = member-to-t-member(m)
          if existing-mem == this-mem: sd
          else: sd.remove(name)
          end
        end
      end
    end

    for map(k from shared-across-variants.keys-list()):
      shared-across-variants.get-value(k)
    end
  end

  fun v-member-to-t-member(m):
    cases(A.VariantMember) m:
      | s-variant-member(l, kind, bind) =>
        typ = if A.is-s-mutable(kind):
            T.t-ref(ann-to-typ(bind.ann))
          else:
            ann-to-typ(bind.ann)
          end
        T.t-member(bind.id.toname(), typ)
    end
  end

  fun member-to-t-member(m):
    cases(A.Member) m:
      | s-data-field(l, name, val) =>
        T.t-member(name, T.t-top)
      | s-mutable-field(l, name, ann, val) =>
        T.t-member(name, T.t-ref(ann-to-typ(ann)))
      | s-method-field(l, name, params, args, ann, _, _, _) =>
        arrow-part = 
          T.t-arrow(map(ann-to-typ, map(args, _.ann)), ann-to-typ(ann))
        typ =
          if is-empty(params): arrow-part
          else:
            tvars = for map(p from params): T.t-variable(l, p, T.t-top, T.invariant) end
            T.t-forall(tvars, arrow-part)
          end
        T.t-member(name, typ)
    end
  end


  fun ann-to-typ(a :: A.Ann) -> T.Type:
    cases(A.Ann) a:
      | a-blank => T.t-top
      | a-any => T.t-top
      | a-name(l, id) => T.t-name(some(uri), id)
      | a-type-var(l, id) =>
        T.t-var(id)
      | a-arrow(l, args, ret, use-parens) =>
        T.t-arrow(map(ann-to-typ, args), ann-to-typ(ret))
      | a-method(l, args, ret) =>
        raise("Cannot provide a raw method")
      | a-record(l, fields) =>
        T.t-record(map(field-to-typ, fields))
      | a-app(l, ann, args) =>
        T.t-app(ann-to-typ(ann), map(ann-to-typ, args))
      | a-pred(l, ann, exp) =>
        # TODO(joe): give more info than this to type checker?  only needed dynamically, right?
        ann-to-typ(ann)
      | a-dot(l, obj, field) =>
        b = resolved.type-bindings.get-value-now(obj.key())
        exp = for some-pred(v from b.ann):
          A.is-s-import-complete(v)
        end
        dep = import-to-dep(exp.import-type)
        provided-from-other = compile-env.mods.get-value(dep.key())
        T.t-name(some(provided-from-other.from-uri), A.s-name(l, field))
      | a-checked(checked, residual) =>
        raise("a-checked should only be generated by the type-checker")
    end
  end
  fun data-expr-to-datatype(exp :: A.Expr % (is-s-data-expr)) -> T.DataType:
    cases(A.Expr) exp:
      | s-data-expr(l, name, _, params, _, variants, shared-members, _) =>

        tvars = for map(tvar from params):
          T.t-variable(l, tvar, T.t-top, T.invariant)
        end

        tvariants = for map(tv from variants):
          cases(A.Variant) tv:
            | s-variant(l2, constr-loc, vname, members, with-members) =>
              T.t-variant(
                l2,
                vname,
                map(v-member-to-t-member, members),
                map(member-to-t-member, with-members))
            | s-singleton-variant(l2, vname, with-members) =>
              T.t-singleton-variant(
                l2,
                vname,
                map(member-to-t-member, with-members))
          end
        end

        shared-across-variants = collect-shared-fields(variants)

        all-shared-fields = map(member-to-t-member, shared-members) + shared-across-variants

        T.t-datatype(
          name,
          tvars,
          tvariants,
          all-shared-fields
        )
    end
  end
  cases(A.Program) resolved.ast:
    | s-program(_, provide-complete, _, _, _) =>
      cases(A.Provide) provide-complete:
        | s-provide-complete(_, values, aliases, datas) =>
          val-typs = for fold(sd from [SD.string-dict:], v from values):
            sd.set(v.v.toname(), ann-to-typ(v.ann))
          end
          alias-typs = for fold(sd from [SD.string-dict:], a from aliases):
            # TODO(joe): recursive lookup here until reaching a non-alias?
            target-binding = resolved.type-bindings.get-value-now(a.in-name.key())
            typ = cases(Option) target-binding.ann:
              | none => T.t-top
              | some(target-ann) =>
                if A.is-Ann(a):
                  ann-to-typ(target-ann)
                else:
                  T.t-top
                end
            end
            sd.set(a.out-name.toname(), typ)
          end
          data-typs = for fold(sd from [SD.string-dict:], d from datas):
            exp = resolved.datatypes.get-value-now(d.d.key())
            sd.set(d.d.key(), data-expr-to-datatype(exp))
          end
          print("Defined in provides as: ")
          print(data-typs)
          CS.provides(
              uri,
              val-typs,
              alias-typs,
              data-typs
            )
      end
  end
end

fun canonicalize-members(ms :: T.TypeMembers, uri :: URI) -> T.TypeMembers:
  for map(f from ms):
    T.t-member(f.field-name, canonicalize-names(f.typ, uri))
  end
end

fun canonicalize-variant(v :: T.TypeVariant, uri :: URI) -> T.TypeVariant:
  c = canonicalize-members(_, uri)
  cases(T.TypeVariant) v:
    | t-variant(l, name, fields, with-fields) =>
      T.t-variant(l, name, c(fields), c(with-fields))
    | t-singleton-variant(l, name, with-fields) =>
      T.t-singleton-variant(l, name, c(with-fields))
  end
end

fun canonicalize-datatype(dtyp :: T.DataType, uri :: URI) -> T.DataType:
  cases(T.DataType) dtyp:
    | t-datatype(name, params, variants, fields) =>
      T.t-datatype(
          name,
          params,
          map(canonicalize-variant(_, uri), variants),
          canonicalize-members(fields, uri))
  end
end

fun canonicalize-names(typ :: T.Type, uri :: URI) -> T.Type:
  c = canonicalize-names(_, uri)
  cases(T.Type) typ:
    | t-name(module-name, id) =>
      cases(Option<String>) module-name:
        | none => T.t-name(some(uri), id)
        | some(other-uri) => typ
      end
    | t-var(id) => typ
    | t-arrow(args, ret) => T.t-arrow(map(c, args), c(ret))
    | t-app(onto, args) => T.t-app(c(onto), map(c, args))
    | t-top => T.t-top
    | t-bot => T.t-bot
    | t-record(fields) =>
      T.t-record(canonicalize-members(fields, uri))
    | t-forall(introduces, onto) => T.t-forall(introduces, c(onto))
    | t-ref(t) => T.t-ref(c(t))
  end
end

fun get-typed-provides(typed :: TCS.Typed, uri :: URI, compile-env :: CS.CompileEnvironment):
  c = canonicalize-names(_, uri)
  cases(A.Program) typed.ast:
    | s-program(_, provide-complete, _, _, _) =>
      cases(A.Provide) provide-complete:
        | s-provide-complete(_, values, aliases, datas) =>
          val-typs = for fold(sd from [SD.string-dict:], v from values):
            sd.set(v.v.toname(), c(typed.info.typs.get-value-now(v.v.key())))
          end
          alias-typs = for fold(sd from [SD.string-dict:], a from aliases):
            # TODO(joe): recursive lookup here until reaching a non-alias?
            print(typed.info)
            cases(Option) typed.info.data-exprs.get-now(a.in-name.key()):
              | some(typ) => sd.set(a.out-name.toname(), c(typ))
              | none => 
                cases(Option) typed.info.branders.get-now(a.in-name.key()):
                  | some(typ) => sd.set(a.out-name.toname(), c(typ))
                  | else =>
                    typ = typed.info.aliases.get-value-now(a.in-name.key())
                    sd.set(a.out-name.toname(), c(typ))
                end
            end
          end
          data-typs = for fold(sd from [SD.string-dict:], d from datas):
            print(typed.info.data-exprs)
            sd.set(d.d.toname(), canonicalize-datatype(typed.info.data-exprs.get-value-now(d.d.key()), uri))
          end
          CS.provides(
              uri,
              val-typs,
              alias-typs,
              data-typs
            )
      end
  end
end
