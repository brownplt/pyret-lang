#lang pyret

provide *
provide-types *
import srcloc as SL
import ast as A
import parse-pyret as PP
import file("compile-structs.arr") as CS
import file("ast-anf.arr") as N
import file("type-structs.arr") as T
import file("type-check-structs.arr") as TCS
import string-dict as SD
import either as E
import lists as L

type URI = String
type Loc = SL.Srcloc

type NameOrigin = T.NameOrigin
local = T.local
module-uri = T.module-uri
dependency = T.dependency

fun ok-last(stmt):
  not(
    A.is-s-let(stmt) or
    A.is-s-tuple-let(stmt) or
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

fun append-nothing-if-necessary(prog :: A.Program) -> A.Program:
  cases(A.Program) prog:
    | s-program(l1, _provide, _provide-types, imports, body) =>
      cases(A.Expr) body:
        | s-block(l2, stmts) =>
          cases(List) stmts:
            | empty =>
              A.s-program(l1, _provide, _provide-types, imports,
                A.s-block(l2, [list: A.s-id(l2, A.s-name(l2, "nothing"))]))
            | link(_, _) =>
              last-stmt = stmts.last()
              if ok-last(last-stmt): prog
              else:
                A.s-program(l1, _provide, _provide-types, imports,
                  A.s-block(l2, stmts + [list: A.s-id(A.dummy-loc, A.s-name(l2, "nothing"))]))
              end
          end
        | else => prog
      end
  end
end


flatten-and-merge-blocks = A.default-map-visitor.{
    method s-block(self, l, stmts):
    if stmts.length() == 1: stmts.first.visit(self)
    else:
      merged-stmts = for fold(new-stmts from [list: ], s from stmts):
        cases(A.Expr) s.visit(self):
          | s-block(l2, stmts2) => L.reverse-help(stmts2, new-stmts)
          | else => s ^ link(_, new-stmts)
        end
      end
      A.s-block(l, merged-stmts.reverse())
    end
  end
  }


fun count-apps(expr) block:
  var count = 0
  visitor = A.default-iter-visitor.{
      method s-app(self, l, f, args) block:
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
  cases(Option<Binding>) bind-exp(e, env) block:
    | none => b-unknown
    | some(b) =>
      when not(is-e-bind(b)) block:
        print-error("b isn't a binding for expr " + string-substring(torepr(e), 0, 100))
        print-error(b)
      end
      b.info
  end
end

fun binding-type-env-from-env(env) block:
  acc = SD.make-mutable-string-dict()
  for each(name from env.globals.types.keys-list()):
    acc.set-now(A.s-type-global(name).key(), e-bind(A.dummy-loc, false, b-typ))
  end
  acc.freeze()
end
fun binding-env-from-env(env) block:
  acc = SD.make-mutable-string-dict()
  for each(name from env.globals.values.keys-list()):
    acc.set-now(A.s-global(name).key(), e-bind(A.dummy-loc, false, b-prim(name)))
  end
  acc.freeze()
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

    method s-program(self, l, _provide, _provide-types, imports, body):
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
    method s-type-let-expr(self, l, binds, body, blocky):
      new-envs = { val-env: self.env, type-env: self.type-env }
      bound-env = for lists.fold(acc from new-envs.{ bs: [list: ] }, b from binds):
        updated = bind-handlers.s-type-let-bind(b, acc.val-env, acc.type-env)
        visit-envs = self.{ env: updated.val-env, type-env: updated.type-env }
        new-bind = b.visit(visit-envs)
        updated.{ bs: link(new-bind, acc.bs) }
      end
      A.s-type-let-expr(l, bound-env.bs.reverse(), body.visit(self.{ env: bound-env.val-env, type-env: bound-env.type-env }), blocky)
    end,
    method s-let-expr(self, l, binds, body, blocky):
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
      A.s-let-expr(l, visit-binds, visit-body, blocky)
    end,
    method s-letrec(self, l, binds, body, blocky):
      bind-env = for fold(acc from self.env, b from binds):
        bind-handlers.s-letrec-bind(b, acc)
      end
      new-visitor = self.{env: bind-env}
      visit-binds = binds.map(_.visit(new-visitor))
      visit-body = body.visit(new-visitor)
      A.s-letrec(l, visit-binds, visit-body, blocky)
    end,
    method s-lam(self, l, params, args, ann, doc, body, _check, blocky):
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
      A.s-lam(l, params, new-args, ann.visit(with-args), doc, new-body, new-check, blocky)
    end,
    method s-cases-else(self, l, typ, val, branches, _else, blocky):
      A.s-cases-else(l, typ.visit(self), val.visit(self), branches.map(_.visit(self)), _else.visit(self), blocky)
    end,
    method s-cases-branch(self, l, pat-loc, name, args, body):
      new-args = args.map(_.visit(self))
      args-env = for lists.fold(acc from self.env, arg from args.map(_.bind)):
        bind-handlers.s-bind(arg, acc)
      end
      A.s-cases-branch(l, pat-loc, name, new-args, body.visit(self.{env: args-env}))
    end,
    method s-singleton-cases-branch(self, l, pat-loc, name, body):
      A.s-singleton-cases-branch(l, pat-loc, name, body.visit(self))
    end,
    method s-data-expr(self, l, name, namet, params, mixins, variants, shared-members, _check):
      new-type-env = for lists.fold(acc from self.type-env, param from params):
        bind-handlers.s-param-bind(l, param, acc)
      end
      with-params = self.{type-env: new-type-env}
      A.s-data-expr(l, name, namet.visit(with-params), params,
        mixins.map(_.visit(with-params)), variants.map(_.visit(with-params)),
        shared-members.map(_.visit(with-params)), with-params.option(_check))
    end,
    method s-method(self, l, params, args, ann, doc, body, _check, blocky):
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
      A.s-method(l, params, new-args, ann.visit(with-params.{env: args-env}), doc, new-body, new-check, blocky)
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

    method s-program(self, l, _provide, _provide-types, imports, body):
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
    method s-type-let-expr(self, l, binds, body, blocky):
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
    method s-let-expr(self, l, binds, body, blocky):
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
    method s-letrec(self, l, binds, body, blocky):
      bind-env = for lists.fold(acc from self.env, b from binds):
        bind-handlers.s-letrec-bind(b, acc)
      end
      new-visitor = self.{env: bind-env}
      continue-binds = for lists.fold-while(acc from true, b from binds):
        if b.visit(new-visitor): E.left(true) else: E.right(false) end
      end
      continue-binds and body.visit(new-visitor)
    end,
    method s-lam(self, l, params, args, ann, doc, body, _check, blocky):
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
    method s-cases-else(self, l, typ, val, branches, _else, blocky):
      typ.visit(self)
      and val.visit(self)
      and lists.all(_.visit(self), branches)
      and _else.visit(self)
    end,
    method s-cases-branch(self, l, pat-loc, name, args, body):
      visit-args = lists.all(_.visit(self), args)
      args-env = for lists.fold(acc from self.env, arg from args.map(_.bind)):
        bind-handlers.s-bind(arg, acc)
      end
      visit-args
      and body.visit(self.{env: args-env})
    end,
    # s-singleton-cases-branch introduces no new bindings, so default visitor is fine
    method s-data-expr(self, l, name, namet, params, mixins, variants, shared-members, _check):
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
    method s-method(self, l, params, args, ann, doc, body, _check, blocky):
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
  method s-header(_, imp, env, type-env) block:

    shadow env = env.unfreeze()
    shadow type-env = type-env.unfreeze()

    env.set-now(imp.vals-name.key(), e-bind(imp.l, false, b-unknown))
    type-env.set-now(imp.types-name.key(), e-bind(imp.l, false, b-typ))
    for each(v from imp.values):
      env.set-now(v.key(), e-bind(imp.l, false, b-import(imp.import-type)))
    end
    for each(t from imp.types):
      type-env.set-now(t.key(), e-bind(imp.l, false, b-import(imp.import-type)))
    end
    {
      val-env: env.freeze(),
      type-env: type-env.freeze()
    }
  end,
  method s-param-bind(_, l, param, type-env):
    type-env.set(param.key(), e-bind(l, false, b-typ))
  end,
  method s-type-let-bind(self, tlb, env, type-env):
    cases(A.TypeLetBind) tlb block:
      | s-type-bind(l, name, params, ann) =>
        new-type-env = for lists.fold(acc from type-env, param from params):
          self.s-param-bind(l, param, acc)
        end
        {
          val-env: env,
          type-env: new-type-env.set(name.key(), e-bind(l, false, b-typ))
        }
      | s-newtype-bind(l, tname, bname) =>
        {
          val-env: env.set(bname.key(), e-bind(l, false, b-unknown)),
          type-env: type-env.set(tname.key(), e-bind(l, false, b-typ))
        }
    end
  end,
  method s-let-bind(_, lb, env):
    cases(A.LetBind) lb:
      | s-let-bind(l2, bind, val) =>
        env.set(bind.id.key(), e-bind(l2, false, bind-or-unknown(val, env)))
      | s-var-bind(l2, bind, val) =>
        env.set(bind.id.key(), e-bind(l2, true, b-unknown))
    end
  end,
  method s-letrec-bind(_, lrb, env):
    env.set(lrb.b.id.key(),
      e-bind(lrb.l, false, bind-or-unknown(lrb.value, env)))
  end,
  method s-bind(_, b, env):
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
    method s-app(self, l, f, args):
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

fun bad-assignments(initial-env, ast) block:
  var errors = [list: ] # THE MUTABLE LIST OF ERRORS
  fun add-error(err): errors := err ^ link(_, errors) end
  ast.visit(binding-env-iter-visitor(initial-env).{
    method s-assign(self, loc, id, value) block:
      cases(Option<Binding>) bind-exp(A.s-id(loc, id), self.env):
        | none => nothing
        | some(b) =>
          when not(b.mut):
            add-error(CS.bad-assignment(A.s-assign(loc, id, value), b.loc))
          end
      end
      value.visit(self)
    end,
  })
  errors
end

inline-lams = A.default-map-visitor.{
  method s-app(self, loc, f, exps):
    cases(A.Expr) f:
      | s-lam(l, _, args, ann, _, body, _, blocky) =>
        if (args.length() == exps.length()):
          a = A.global-names.make-atom("inline_body")
          let-binds = for lists.map2(arg from args, exp from exps):
            A.s-let-bind(arg.l, arg, exp.visit(self))
          end
          cases(A.Ann) ann:
            | a-blank => A.s-let-expr(l, let-binds, body.visit(self), blocky)
            | a-any(_) => A.s-let-expr(l, let-binds, body.visit(self), blocky)
            | else =>
              A.s-let-expr(l,
                let-binds
                  + [list: A.s-let-bind(body.l, A.s-bind(l, false, a, ann), body.visit(self))],
                A.s-id(l, a), false)
          end
        else:
          A.s-app(loc, f.visit(self), exps.map(_.visit(self)))
        end
      | else => A.s-app(loc, f.visit(self), exps.map(_.visit(self)))
    end
  end
}

fun check-unbound(initial-env, ast) block:
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
      method s-id(self, loc, id) block:
        handle-id(A.s-id(loc, id), self.env)
        true
      end,
      method s-id-var(self, loc, id) block:
        handle-id(A.s-id-var(loc, id), self.env)
        true
      end,
      method s-id-letrec(self, loc, id, safe) block:
        handle-id(A.s-id-letrec(loc, id, safe), self.env)
        true
      end,
      method s-assign(self, loc, id, value) block:
        when is-none(bind-exp(A.s-id(loc, id), self.env)):
          add-error(CS.unbound-var(id.toname(), loc))
        end
        value.visit(self)
      end,
      method a-name(self, loc, id) block:
        handle-type-id(A.a-name(loc, id), self.type-env)
        true
      end,
      method a-dot(self, loc, name, field) block:
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
  method s-letrec(self, l, binds, body, blocky):
    bind-envs = for map2(b1 from binds, i from range(0, binds.length())) block:
      rhs-is-delayed = value-delays-exec-of(b1.b.id, b1.value)
      acc = self.env.unfreeze()
      for each2(b2 from binds, j from range(0, binds.length())):
        key = b2.b.id.key()
        if i < j:
          acc.set-now(key, false)
        else if i == j:
          acc.set-now(key, rhs-is-delayed)
        else:
          acc.set-now(key, true)
        end
      end
      acc.freeze()
    end
    new-binds = for map2(b from binds, bind-env from bind-envs):
      b.visit(self.{ env: bind-env })
    end
    body-env = bind-envs.last().set(binds.last().b.id.key(), true)
    new-body = body.visit(self.{ env: body-env })
    A.s-letrec(l, new-binds, new-body, blocky)
  end,
  method s-id-letrec(self, l, id, _):
    A.s-id-letrec(l, id, self.env.get-value(id.key()))
  end
}

fun make-renamer(replacements :: SD.StringDict):
  A.default-map-visitor.{
    method s-atom(self, base, serial):
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
          name-to-use = if i.as-name == "_": A.s-underscore(p.l) else: A.s-name(p.l, i.as-name) end
          cases(CS.Dependency) i.dependency:
            | builtin(name) =>
              loc = SL.builtin(i.as-name)
              A.s-import-complete(
                p.l,
                i.values.map(A.s-name(loc, _)),
                i.types.map(A.s-name(loc, _)),
                A.s-const-import(p.l, name),
                name-to-use,
                name-to-use)
            | dependency(protocol, args) =>
              A.s-import-complete(
                p.l,
                i.values.map(A.s-name(p.l, _)),
                i.types.map(A.s-name(p.l, _)),
                A.s-special-import(p.l, protocol, args),
                name-to-use,
                name-to-use)
          end
        end
      A.s-program(p.l, p._provide, p.provided-types, full-imports, p.block)
  end
end

fun import-to-dep(imp):
  cases(A.ImportType) imp:
    # crossover compatibility
    | s-const-import(_, modname) => CS.builtin(modname)
    | s-special-import(_, protocol, args) => CS.dependency(protocol, args)
  end
end

fun import-to-dep-anf(imp):
  cases(N.AImportType) imp:
    | a-import-builtin(_, name) => CS.builtin(name)
    | a-import-special(_, kind, args) => CS.dependency(kind, args)
  end
end

fun some-pred<a>(pred :: (a -> Boolean), o :: Option<a>) -> a:
  cases(Option) o block:
    | none => raise("Expected some but got none")
    | some(exp) =>
      when not(pred(exp)):
        raise("Predicate failed for " + torepr(exp))
      end
      exp
  end
end

is-s-data-expr = A.is-s-data-expr

is-t-name = T.is-t-name
type NameChanger = (T.Type%(is-t-name) -> T.Type)


fun get-named-provides(resolved :: CS.NameResolution, uri :: URI, compile-env :: CS.CompileEnvironment) -> CS.Provides:
  fun field-to-typ(f :: A.AField) -> T.TypeMember:
    T.t-member(f.name, ann-to-typ(f.ann))
  end

  fun collect-shared-fields(vs :: List<A.Variant>) block:
    init-members = SD.make-mutable-string-dict()
    for each(m from vs.first.with-members):
      init-members.set-now(m.name, member-to-t-member(m))
    end

    shared-across-variants = init-members

    for each(v from vs.rest):
      for each(m from v.with-members):
        when shared-across-variants.has-key-now(m.name):
          existing-mem = shared-across-variants.get-value-now(m.name)
          this-mem = member-to-t-member(m)
          when not(existing-mem == this-mem):
            shared-across-variants.remove-now(m.name)
          end
        end
      end
    end

    for map(k from shared-across-variants.keys-list-now()):
      shared-across-variants.get-value-now(k)
    end
  end

  fun v-member-to-t-member(m):
    cases(A.VariantMember) m:
      | s-variant-member(l, kind, bind) =>
        typ = if A.is-s-mutable(kind):
            T.t-ref(ann-to-typ(bind.ann), l)
          else:
            ann-to-typ(bind.ann)
          end
        T.t-member(bind.id.toname(), typ)
    end
  end

  fun member-to-t-member(m):
    cases(A.Member) m:
      | s-data-field(l, name, val) =>
        T.t-member(name, T.t-top(l))
      | s-mutable-field(l, name, ann, val) =>
        T.t-member(name, T.t-ref(ann-to-typ(ann)))
      | s-method-field(l, name, params, args, ann, _, _, _, _) =>
        arrow-part =
          T.t-arrow(map(ann-to-typ, map(_.ann, args)), ann-to-typ(ann), l)
        typ =
          if is-empty(params): arrow-part
          else:
            tvars = for map(p from params): T.t-var(p, l) end
            T.t-forall(tvars, arrow-part, l)
          end
        T.t-member(name, typ)
    end
  end

  # TODO(MATT): a-blank should have location
  fun ann-to-typ(a :: A.Ann) -> T.Type:
    cases(A.Ann) a:
      | a-blank => T.t-top(A.dummy-loc)
      | a-any(l) => T.t-top(l)
      | a-name(l, id) =>
        cases(A.Name) id:
          | s-type-global(name) =>
            cases(Option<String>) compile-env.globals.types.get(name):
              | none => raise("Name not found in globals.types: " + name)
              | some(key) =>
                cases(Option<CS.Provides>) compile-env.mods.get(key):
                  | none => raise("Module not found in compile-env.mods: " + key
                        + " (looked up for " + name + " for module " + uri + ")")
                  | some(mod) => T.t-name(T.module-uri(mod.from-uri), id, l)
                end
            end
          | s-atom(_, _) => T.t-name(T.module-uri(uri), id, l)
          | else => raise("Bad name found in ann-to-typ: " + id.key())
        end
      | a-type-var(l, id) =>
        T.t-var(id, l)
      | a-arrow(l, args, ret, use-parens) =>
        T.t-arrow(map(ann-to-typ, args), ann-to-typ(ret), l)
      | a-method(l, args, ret) =>
        raise("Cannot provide a raw method")
      | a-record(l, fields) =>
        T.t-record(map(field-to-typ, fields), l)
      | a-tuple(l, fields) =>
        T.t-top(l)
      | a-app(l, ann, args) =>
        T.t-app(ann-to-typ(ann), map(ann-to-typ, args), l)
      | a-pred(l, ann, exp) =>
        # TODO(joe): give more info than this to type checker?  only needed dynamically, right?
        ann-to-typ(ann)
      | a-dot(l, obj, field) =>
        maybe-b = resolved.type-bindings.get-now(obj.key())
        cases(Option) maybe-b:
          | none =>
            T.t-top(l)
          | some(b) =>
            exp = for some-pred(v from b.ann):
              A.is-s-import-complete(v)
            end
            dep = import-to-dep(exp.import-type)
            provided-from-other = compile-env.mods.get-value(dep.key())
            T.t-name(module-uri(provided-from-other.from-uri), A.s-name(l, field), l)
        end
      | a-checked(checked, residual) =>
        raise("a-checked should only be generated by the type-checker")
    end
  end
  fun data-expr-to-datatype(exp :: A.Expr % (is-s-data-expr)) -> T.Type:
    cases(A.Expr) exp:
      | s-data-expr(l, name, _, params, _, variants, shared-members, _) =>

        tvars = for map(tvar from params):
          T.t-var(tvar, l)
        end

        tvariants = for map(tv from variants):
          cases(A.Variant) tv:
            | s-variant(l2, constr-loc, vname, members, with-members) =>
              T.t-variant(
                vname,
                map(v-member-to-t-member, members),
                map(member-to-t-member, with-members))
            | s-singleton-variant(l2, vname, with-members) =>
              T.t-singleton-variant(
                vname,
                map(member-to-t-member, with-members))
          end
        end

        shared-across-variants = collect-shared-fields(variants)

        all-shared-fields = map(member-to-t-member, shared-members) + shared-across-variants

        T.t-forall(
          tvars,
          T.t-data(
            name,
            tvariants,
            all-shared-fields,
            l),
          l)
    end
  end
  cases(A.Program) resolved.ast:
    | s-program(l, provide-complete, _, _, _) =>
      cases(A.Provide) provide-complete block:
        | s-provide-complete(_, values, aliases, datas) =>
          val-typs = SD.make-mutable-string-dict()
          for each(v from values):
            val-typs.set-now(v.v.toname(), ann-to-typ(v.ann))
          end
          alias-typs = SD.make-mutable-string-dict()
          for each(a from aliases):
            # TODO(joe): recursive lookup here until reaching a non-alias?
            target-binding = resolved.type-bindings.get-value-now(a.in-name.key())
            typ = cases(Option) target-binding.ann:
              | none => T.t-top(l)
              | some(target-ann) =>
                if A.is-Ann(a):
                  ann-to-typ(target-ann)
                else:
                  T.t-top(l)
                end
            end
            alias-typs.set-now(a.out-name.toname(), typ)
          end
          data-typs = SD.make-mutable-string-dict()
          for each(d from datas):
            exp = resolved.datatypes.get-value-now(d.d.key())
            data-typs.set-now(d.d.key(), data-expr-to-datatype(exp))
          end
          CS.provides(
              uri,
              val-typs.freeze(),
              alias-typs.freeze(),
              data-typs.freeze()
            )
      end
  end
end


fun canonicalize-members(ms :: T.TypeMembers, uri :: URI, tn :: NameChanger) -> T.TypeMembers:
  for map(f from ms):
    T.t-member(f.field-name, canonicalize-names(f.typ, uri, tn))
  end
end

fun canonicalize-variant(v :: T.TypeVariant, uri :: URI, tn :: NameChanger) -> T.TypeVariant:
  c = canonicalize-members(_, uri, tn)
  cases(T.TypeVariant) v:
    | t-variant(name, fields, with-fields) =>
      T.t-variant(name, c(fields), c(with-fields))
    | t-singleton-variant(name, with-fields) =>
      T.t-singleton-variant(name, c(with-fields))
  end
end

#fun canonicalize-datatype(dtyp :: T.DataType, uri :: URI) -> T.DataType:
#  cases(T.DataType) dtyp:
#    | t-datatype(name, params, variants, fields) =>
#      T.t-datatype(
#          name,
#          params,
#          map(canonicalize-variant(_, uri), variants),
#          canonicalize-members(fields, uri))
#  end
#end

fun canonicalize-names(typ :: T.Type, uri :: URI, transform-name :: NameChanger) -> T.Type:
  c = canonicalize-names(_, uri, transform-name)
  cases(T.Type) typ:
    | t-name(module-name, id, l) => transform-name(typ)
    | t-var(id, l) => typ
    | t-arrow(args, ret, l) => T.t-arrow(map(c, args), c(ret), l)
    | t-tuple(elts, l) => T.t-tuple(map(c, elts), l)
    | t-app(onto, args, l) => T.t-app(c(onto), map(c, args), l)
    | t-top(l) => T.t-top(l)
    | t-bot(l) => T.t-bot(l)
    | t-record(fields, l) =>
      T.t-record(canonicalize-members(fields, uri, transform-name), l)
    | t-forall(introduces, onto, l) => T.t-forall(map(c, introduces), c(onto), l)
    | t-ref(t, l) => T.t-ref(c(t), l)
    | t-existential(id, l) => typ
    | t-data(name, variants, fields, l) =>
      T.t-data(name, map(canonicalize-variant(_, uri, transform-name), variants), canonicalize-members(fields, uri, transform-name), l)
  end
end

fun find-mod(compile-env, uri) -> Option<String>:
  for find(depkey from compile-env.mods.keys-list()):
    other-uri = compile-env.mods.get-value(depkey).from-uri
    other-uri == uri
  end
end

fun transform-dict(d, uri, transformer):
  for fold(s from [SD.string-dict:], v from d.keys-list()):
    s.set(v, canonicalize-names(d.get-value(v), uri, transformer))
  end
end

fun transform-provides(provides, compile-env, transformer):
  cases(CS.Provides) provides:
  | provides(from-uri, values, aliases, data-definitions) =>
    new-vals = transform-dict(values, from-uri, transformer)
    new-aliases = transform-dict(aliases, from-uri, transformer)
    new-data-definitions = transform-dict(data-definitions, from-uri, transformer)
    CS.provides(from-uri, new-vals, new-aliases, new-data-definitions)
  end
end

fun canonicalize-provides(provides :: CS.Provides, compile-env :: CS.CompileEnvironment):
  doc: ```
  Produces a new provides data structure that has no `dependency` NameOrigins
  in the types, by looking up dependencies in the compile environment.
  Also produces an error if there is a module URI or dependency that is not
  mentioned in the compile-env.
  ```
  transformer = lam(t :: T.Type%(is-t-name)):
    cases(T.Type) t:
    | t-name(origin, name, loc) =>
      cases(T.NameOrigin) origin:
      | local => T.t-name(T.module-uri(provides.from-uri), name, loc)
      | module-uri(uri) =>
        cases(Option<String>) find-mod(compile-env, uri):
        | some(_) => T.t-name(T.module-uri(uri), name, loc)
        | none =>
          if string-index-of(uri, "builtin://") == 0:
            T.t-name(T.module-uri(uri), name, loc)
          else if uri == provides.from-uri:
            T.t-name(T.module-uri(uri), name, loc)
          else:
          # TODO(joe): This should become an error once things are localized again

            #T.t-name(T.module-uri(uri), name, loc)
            raise("Unknown module URI for type: " + torepr(t) + " in provides for " + provides.from-uri)
          end
        end
      | dependency(d) =>
        provides-for-d = compile-env.mods.get(d)
        cases(Option<CS.Provides>) provides-for-d:
        | some(p) => T.t-name(module-uri(p.from-uri), name, loc)
        | none => raise("Unknown module dependency for type: " + torepr(t) + " in provides for " + provides.from-uri)
        end
      end
    end
  end
  transform-provides(provides, compile-env, transformer)
end

fun localize-provides(provides :: CS.Provides, compile-env :: CS.CompileEnvironment):
  doc: ```
  Produces a new provides data structure that has no `module-uri` NameOrigins
  in the types, by looking up uris in the compile environment, or using `local`.

  Also produces an error if there is a module URI or dependency that is not
  mentioned in the compile-env.
  ```
  transformer = lam(t :: T.Type%(is-t-name)):
    cases(T.Type) t:
    | t-name(origin, name, loc) =>
      cases(T.NameOrigin) origin:
      | local => t
      | module-uri(uri) =>
        if uri == provides.from-uri:
          T.t-name(T.local, name, loc)
        else:
          cases(Option<String>) find-mod(compile-env, uri):
          | some(d) => T.t-name(T.dependency(d), name, loc)
          | none =>
            if string-index-of(uri, "builtin://") == 0:
              T.t-name(T.module-uri(uri), name, loc)
            else:
              # TODO(joe): This should become an error once things are localized again
              #T.t-name(T.module-uri(uri), name, loc)

              raise("Unknown module URI for type: " + torepr(t) + " in provides for " + provides.from-uri)
            end
          end
        end
      | dependency(d) =>
        provides-for-d = compile-env.mods.get(d)
        cases(Option<CS.Provides>) provides-for-d:
        | some(p) => t
        | none => raise("Unknown module dependency for type: " + torepr(t) + " in provides for " + provides.from-uri)
        end
      end
    end
  end
  transform-provides(provides, compile-env, transformer)
end

# TODO(MATT): this does not actually get the provided module values
fun get-typed-provides(typed :: TCS.Typed, uri :: URI, compile-env :: CS.CompileEnvironment):
  transformer = lam(t):
    cases(T.Type) t:
      | t-name(origin, name, l) =>
        T.t-name(origin, A.s-type-global(name.toname()), l)
      | else => t
    end
  end
  c = canonicalize-names(_, uri, transformer)
  cases(A.Program) typed.ast block:
    | s-program(_, provide-complete, _, _, _) =>
      cases(A.Provide) provide-complete block:
        | s-provide-complete(_, values, aliases, datas) =>
          val-typs = SD.make-mutable-string-dict()
          for each(v from values):
            val-typs.set-now(v.v.toname(), c(typed.info.types.get-value(v.v.key())))
          end
          alias-typs = SD.make-mutable-string-dict()
          for each(a from aliases):
            # TODO(joe): recursive lookup here until reaching a non-alias?
           cases(Option) typed.info.data-types.get(a.in-name.key()):
              | some(typ) => alias-typs.set-now(a.out-name.toname(), c(typ))
              | none =>
                # NOTE(joe): This was commented _in_ on new-horizons.
                #cases(Option) typed.info.branders.get-now(a.in-name.key()):
                #  | some(typ) => alias-typs.set-now(a.out-name.toname(), c(typ))
                #  | else =>
                #    typ = typed.info.aliases.get-value-now(a.in-name.key())
                #    alias-typs.set-now(a.out-name.toname(), c(typ))
                #end

                # NOTE(joe): We look up `typ` by key, and then
                # canonicalize-names converts all the names to be s-global-type
                # which removes all gensym information from what's provided
                # as types.
                typ = typed.info.aliases.get-value(a.in-name.key())
                alias-typs.set-now(a.out-name.toname(), c(typ))
            end
          end
          data-typs = SD.make-mutable-string-dict()
          for each(d from datas):
            data-typs.set-now(d.d.toname(), c(typed.info.data-types.get-value(d.d.key())))
          end
          CS.provides(
              uri,
              val-typs.freeze(),
              alias-typs.freeze(),
              data-typs.freeze()
            )
      end
  end
end
