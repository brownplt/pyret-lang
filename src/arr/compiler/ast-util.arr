#lang pyret

provide *
provide-types *
import srcloc as SL
import ast as A
import parse-pyret as PP
import "compiler/compile-structs.arr" as CS
import "compiler/ast-anf.arr" as N
import string-dict as SD
import either as E

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
      A.s-type-let-expr(l, bound-env.bs.reverse(), body.visit(self.{ env: bound-env.val-env, type-env: bound-env.type-env }))
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

data Scope:
  | no-s

  | fun-s(id :: A.Name)
  | method-s(self-id :: A.Name, name :: String)

  | partial-fun-s(id :: A.Name)
  | partial-method-s(name :: String)
end

# set-recursive-visitor is to replace s-app with s-app-enhanced with correct is-recursive
# but with incorrect is-tail (all false)
# postcondition: no s-app
set-recursive-visitor = A.default-map-visitor.{
  scope: no-s,

  clear-scope(self):
    self.{scope: no-s}
  end,

  is-recursive(self, f :: A.Expr) -> Boolean:
    doc: "Return a Boolean indicating whether a call with `f` is recursive or not"
    cases (Scope) self.scope:
      | fun-s(id) => A.is-s-id-letrec(f) and (f.id == id)
      | method-s(self-id, name) => false # TODO(Oak, 15 Jan 2016): Don't care about method for now

      | no-s => false
      | partial-method-s(_) => false # Do not actually find a method: `{ a : lam(): id(1) end }`
      | partial-fun-s(_) => raise("Error while querying: after partial-fun-s should immediately be fun-s")
    end
  end,

  activate-fun(self):
    doc: "Activate fun-s"
    cases (Scope) self.scope:
      | partial-fun-s(id) => self.{scope: fun-s(id)}

      | no-s => self # no letrec, meaning it's just normal lambda: `lam(x): x + 1 end(5)`, for example
      | fun-s(_) => self.clear-scope() # lam in function: `fun foo() lam(): 1 end end`
      | method-s(_, _) => self.clear-scope() # lam in method: `{ foo(self): lam(): 1 end end }`
      | partial-method-s(_) => self.clear-scope() # lam in object: `{ a : lam(): id(1) end }`
    end
  end,

  collect-fun-name(self, binding :: A.LetrecBind):
    doc: "Return an environment with a binding containing function's name"
    if A.is-s-lam(binding.value):
      self.{scope: partial-fun-s(binding.b.id)}
    else:
      self
    end
  end,

  collect-method-self(self, self-bind :: A.Bind):
    doc: "Return an environment with method's self id"
    cases (Scope) self.scope:
      | partial-method-s(name) => self.{scope: method-s(self-bind.id, name)}

      | no-s => self.clear-scope() # `method(self): 1 end`
      | fun-s(_) => self.clear-scope() # fun foo(): method(self): 1 end end
      | method-s(_, _) => self.clear-scope() # { a(self1): method(self2): 1 end end }
      | partial-fun-s(_) => raise("Error while collecting self: after partial-fun-s should immediately be fun-s")
    end
  end,

  collect-method-name(self, method-name :: String):
    doc: "Return an environment with a method's name"
    self.{scope: partial-method-s(method-name)}
  end,


  s-app(self, l, f, exps):
    A.s-app-enriched(
      l,
      f.visit(self),
      exps.map(_.visit(self)),
      A.app-info-c(self.is-recursive(f), false))
  end,
  s-lam(self, l, params, args, ann, doc, body, _check):
    A.s-lam(
      l,
      params.map(_.visit(self.clear-scope())),
      args.map(_.visit(self.clear-scope())),
      ann.visit(self.clear-scope()),
      doc,
      body.visit(self.activate-fun()),
      self.clear-scope().option(_check))
  end,
  s-method(self, l, params, args, ann, doc, body, _check):
    A.s-method(
      l,
      params.map(_.visit(self)),
      args.map(_.visit(self)),
      ann.visit(self),
      doc,
      body.visit(self.collect-method-self(args.first)),
      self.option(_check))
  end,
  s-letrec(self, l, binds, body):
    A.s-letrec(
      l,
      binds.map(lam(bind :: A.LetrecBind): bind.visit(self.collect-fun-name(bind)) end),
      body.visit(self))
  end,
  s-data-field(self, l, name, value):
    A.s-data-field(l, name, value.visit(self.collect-method-name(name)))
  end,
}

fun is-stateful-ann(ann :: A.Ann) -> Boolean:
  doc: ```
       Return whether `ann` is a stateful annotation or not. For now, consider
       all refinements as potentially could be stateful.
       ```
  # TODO(Oak, 26 Jan 2016): make sure below are correct when static type checker lands
  cases (A.Ann) ann:
    | a-blank => false
    | a-any => false
    | a-name(_, _) => false
    | a-type-var(_, _) => false 
    | a-arrow(_, args, ret, _) => false
    | a-method(_, args, ret, _) => false
    | a-record(_, fields) => fields.map(_.ann).all(is-stateful-ann)
    | a-app(_, inner, args) => is-stateful-ann(inner)
    | a-pred(_, _, _) => true # TODO(Oak, 21 Jan 2016): true for now. Could refine later
    | a-dot(_, _, _) => true # TODO(Oak, 7 Feb 2016): true for now. Could refine later
    | a-checked(_, _) => raise("NYI")
  end
end

# set-tail-visitor is to correct is-tail in s-app-enriched
# precondition: no s-app
set-tail-visitor = A.default-map-visitor.{
  is-tail: false,

  s-module(self, l, answer, dv, dt, provides, types, checks):
    no-tail = self.{is-tail: false}
    A.s-module(
      l,
      answer.visit(no-tail),
      dv.map(_.visit(no-tail)),
      dt.map(_.visit(no-tail)),
      provides.visit(no-tail),
      types.map(_.visit(no-tail)),
      checks.visit(no-tail))
  end,

  # skip s-num, s-frac, s-str, s-undefined, s-bool, s-id, s-id-var, s-id-letrec, s-srcloc
  # because it has no s-app-enriched

  # skip s-type-let-expr because all positions which could have s-app-enriched could be in the tail position

  s-let-expr(self, l, binds, body):
    A.s-let-expr(
      l,
      binds.map(_.visit(self.{is-tail: false})),
      body.visit(self))
  end,

  s-letrec(self, l, binds, body):
    A.s-letrec(
      l,
      binds.map(_.visit(self.{is-tail: false})),
      body.visit(self))
  end,

  # skip s-data-expr because it couldn't be at the tail position

  # skip s-if-else because all positions which could have s-app-enriched could be in the tail position

  s-if-branch(self, l, test, body):
    A.s-if-branch(l, test.visit(self.{is-tail: false}), body.visit(self))
  end,

  s-cases-else(self, l, typ, val, branches, _else):
    A.s-cases-else(l, typ.visit(self), val.visit(self.{is-tail: false}), branches.map(_.visit(self)), _else.visit(self))
  end,

  s-block(self, l, stmts):
    len = stmts.length() # can be sure that len >= 1
    splitted = stmts.split-at(len - 1)
    A.s-block(
      l,
      splitted.prefix.map(_.visit(self.{is-tail: false})) +
      splitted.suffix.map(_.visit(self)))
  end,

  s-check-expr(self, l, expr, ann):
    A.s-check-expr(
      l,
      expr.visit(self.{is-tail: false}),
      ann.visit(self.{is-tail: false}))
  end,

  s-lam(self, l, params, args, ann, doc, body, _check):
    A.s-lam(
      l,
      params.map(_.visit(self.{is-tail: false})),
      args.map(_.visit(self.{is-tail: false})),
      ann.visit(self.{is-tail: false}),
      doc,
      body.visit(self.{is-tail: not(is-stateful-ann(ann))}),
      self.{is-tail: false}.option(_check))
  end,

  s-method(self, l, params, args, ann, doc, body, _check):
    A.s-method(
      l,
      params.map(_.visit(self.{is-tail: false})),
      args.map(_.visit(self.{is-tail: false})),
      ann.visit(self.{is-tail: false}),
      doc,
      body.visit(self.{is-tail: not(is-stateful-ann(ann))}),
      self.{is-tail: false}.option(_check))
  end,

  s-array(self, l, values):
    A.s-array(l, values.map(_.visit(self.{is-tail: false})))
  end,

  s-app-enriched(self, l, f, exps, app-info):
    A.s-app-enriched(
      l,
      f.visit(self.{is-tail: false}),
      exps.map(_.visit(self.{is-tail: false})),
      A.app-info-c(app-info.is-recursive, self.is-tail))
  end,

  s-prim-app(self, l, _fun, args):
    A.s-prim-app(l, _fun, args.map(_.visit(self.{is-tail: false})))
  end,

  # skip s-instantiate because all positions which could have s-app-enriched could be in the tail position

  s-dot(self, l, obj, field):
    A.s-dot(l, obj.visit(self.{is-tail: false}), field)
  end,

  s-bracket(self, l, obj, field): # NOTE(Oak, 15 Jan 2016): s-bracket actually doesn't exist
    A.s-bracket(l, obj.visit(self.{is-tail: false}), field.visit(self.{is-tail: false}))
  end,

  # skip s-ref because it has no s-app-enriched -- what is s-ref anyway

  s-get-bang(self, l, obj, field):
    A.s-get-bang(l, obj.visit(self.{is-tail: false}), field)
  end,

  s-assign(self, l, id, value):
    A.s-assign(l, id.visit(self.{is-tail: false}), value.visit(self.{is-tail: false}))
  end,

  s-obj(self, l, fields):
    A.s-obj(l, fields.map(_.visit(self.{is-tail: false})))
  end,

  s-update(self, l, supe, fields):
    A.s-update(l, supe.visit(self.{is-tail: false}), fields.map(_.visit(self.{is-tail: false})))
  end,

  s-extend(self, l, supe, fields):
    A.s-extend(l, supe.visit(self.{is-tail: false}), fields.map(_.visit(self.{is-tail: false})))
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

