#lang pyret

provide *
import ast as A
import parse-pyret as PP
import "compiler/compile-structs.arr" as CS
import string-dict as SD
import either as E

fun ok-last(stmt):
  not (
    A.is-s-let(stmt) or
    A.is-s-var(stmt) or
    A.is-s-fun(stmt) or
    A.is-s-data(stmt) or
    A.is-s-graph(stmt) or
    A.is-s-contract(stmt) or
    A.is-s-check(stmt)
  )
end

fun checkers(l): A.s-app(l, A.s-dot(l, A.s-id(l, A.s-name(l, "builtins")), "current-checker"), []) end

fun append-nothing-if-necessary(prog :: A.Program) -> Option<A.Program>:
  cases(A.Program) prog:
    | s-program(l1, _provide, headers, body) =>
      cases(A.Expr) body:
        | s-block(l2, stmts) =>
          cases(List) stmts:
            | empty =>
              some(A.s-program(l1, _provide, headers, A.s-block(l2, [A.s-id(l2, A.s-name(l2, "nothing"))])))
            | link(_, _) =>
              last-stmt = stmts.last()
              if ok-last(last-stmt): none
              else:
                some(A.s-program(l1, _provide, headers,
                    A.s-block(l2, stmts + [A.s-id(l2, A.s-name(l2, "nothing"))])))
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
  PP.surface-parse("x", "test").block.visit(flatten-single-blocks) satisfies
    A.equiv-ast(_, A.s-id(d, A.s-name(d, "x")))
end

merge-nested-blocks = A.default-map-visitor.{
    s-block(self, l, stmts):
      merged-stmts = for fold(new-stmts from [], s from stmts):
        cases(A.Expr) s.visit(self):
          | s-block(l2, stmts2) => stmts2.reverse() + new-stmts
          | else => [s] + new-stmts
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
  | b-unknown # Any unknown value
end

data Binding:
  | e-bind(loc :: Loc, mut :: Bool, info :: BindingInfo)
end

fun bind-exp(e :: A.Expr, env) -> Option<Binding>:
  cases(A.Expr) e:
    | s-dot(l, o, name) =>
      cases(Option<BindingInfo>) bind-exp(o, env):
        | some(b) =>
          cases(BindingInfo) b:
            | b-dict(dict) =>
              if dict.has-key(name.key()): some(e-bind(A.dummy-loc, false, dict.get(name.key())))
              else: some(e-bind(A.dummy-loc, false, b-dot(b, name)))
              end
            | else => some(e-bind(A.dummy-loc, false, b-dot(b, name)))
          end
        | none => none
      end
    | s-id(_, name) =>
      if env.has-key(name.key()): some(env.get(name.key()))
      else: none
      end
    | s-id-var(_, name) =>
      if env.has-key(name.key()): some(env.get(name.key()))
      else: none
      end
    | s-id-letrec(_, name) =>
      if env.has-key(name.key()): some(env.get(name.key()))
      else: none
      end
    | else => some(e-bind(A.dummy-loc, false, b-exp(e)))
  end
end

fun bind-or-unknown(e :: A.Expr, env) -> BindingInfo:
  cases(Option<Binding>) bind-exp(e, env):
    | none => b-unknown
    | some(b) =>
      when not Binding(b):
        print-error("b isn't a binding for expr " + torepr(e))
        print-error(b)
      end
      b.info
  end
end

fun binding-env-from-env(initial-env):
  for list.fold(acc from SD.immutable-string-dict(), binding from initial-env.bindings):
    cases(C.CompileBinding) binding:
      | module-bindings(name, ids) =>
        mod = for list.fold(m from SD.immutable-string-dict(), b from ids):
          m.set(A.s-name(A.dummy-loc, b).key(), e-bind(A.dummy-loc, false, b-prim(name + ":" + b)))
        end
        acc.set(A.s-name(A.dummy-loc, name).key(), e-bind(A.dummy-loc, false, b-dict(mod)))
      | builtin-id(name) => acc.set(A.s-global(name).key(), e-bind(A.dummy-loc, false, b-prim(name)))
    end
  end
end

fun <a> default-env-map-visitor(
    initial-env :: a,
    bind-handlers :: {
        s-letrec-bind :: (A.LetrecBind, a -> a),
        s-let-bind :: (A.LetBind, a -> a),
        s-bind :: (A.Bind, a -> a),
        s-header :: (A.Header, a -> a)
      }
    ):
  A.default-map-visitor.{
    env: initial-env,

    s-program(self, l, _provide, imports, body):
      visit-provide = _provide.visit(self)
      visit-imports = for map(i from imports):
        i.visit(self)
      end
      imported-env = for fold(acc from self.env, i from visit-imports):
        bind-handlers.s-header(i, acc)
      end
      visit-body = body.visit(self.{env: imported-env})
      A.s-program(l, visit-provide, visit-imports, visit-body)
    end,
    s-let-expr(self, l, binds, body):
      bound-env = for fold(acc from { e: self.env, bs : [] }, b from binds):
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
      new-args = args.map(_.visit(self))
      args-env = for list.fold(acc from self.env, new-arg from args):
        bind-handlers.s-bind(new-arg, acc)
      end
      new-body = body.visit(self.{env: args-env})
      new-check = self.{env: args-env}.option(_check)
      A.s-lam(l, params, new-args, ann, doc, new-body, new-check)
    end,
    s-method(self, l, args, ann, doc, body, _check):
      new-args = args.map(_.visit(self))
      args-env = for list.fold(acc from self.env, a from new-args):
        bind-handlers.s-bind(a, acc)
      end
      new-body = body.visit(self.{env: args-env})
      new-check = self.{env: args-env}.option(_check)
      A.s-method(l, new-args, ann, doc, new-body, new-check)
    end
  }
end


fun <a> default-env-iter-visitor(
    initial-env :: a,
    bind-handlers :: {
        s-letrec-bind :: (A.LetrecBind, a -> a),
        s-let-bind :: (A.LetBind, a -> a),
        s-bind :: (A.Bind, a -> a),
        s-header :: (A.Header, a -> a)
      }
    ):
  A.default-iter-visitor.{
    env: initial-env,

    s-program(self, l, _provide, imports, body):
      if _provide.visit(self):
        imported-env = for fold(acc from self.env, i from imports):
          bind-handlers.s-header(i, acc)
        end
        new-visitor = self.{ env: imported-env }
        list.all(_.visit(new-visitor), imports) and body.visit(new-visitor)
      else:
        false
      end
    end,
    s-let-expr(self, l, binds, body):
      bound-env = for list.fold-while(acc from { e: self.env, bs: true }, b from binds):
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
      bind-env = for list.fold(acc from self.env, b from binds):
        bind-handlers.s-letrec-bind(b, acc)
      end
      new-visitor = self.{env: bind-env}
      continue-binds = for list.fold-while(acc from true, b from binds):
        if b.visit(new-visitor): E.left(true) else: E.right(false) end
      end
      continue-binds and body.visit(new-visitor)
    end,
    s-lam(self, l, params, args, ann, doc, body, _check):
      args-env = for list.fold(acc from self.env, a from args):
        bind-handlers.s-bind(a, acc)
      end
      list.all(_.visit(self), args) and
        body.visit(self.{env: args-env}) and
        self.{env: args-env}.option(_check)
    end,
    s-method(self, l, args, ann, doc, body, _check):
      args-env = for list.fold(acc from self.env, a from args):
        bind-handlers.s-bind(a, acc)
      end
      list.all(_.visit(self), args) and
        body.visit(self.{env: args-env}) and
        self.{env: args-env}.option(_check)
    end
  }
end

binding-handlers = {
  s-header(_, imp, env):
    cases(A.ImportType) imp.file:
      | s-const-import(_, modname) =>
        if env.has-key(modname): env.set(imp.name, env.get(modname.key()))
        else: env.set(imp.name.key(), e-bind(imp.l, false, b-unknown))
        end
      | else => env.set(imp.name.key(), e-bind(imp.l, false, b-unknown))
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
  default-env-map-visitor(binding-env-from-env(initial-env), binding-handlers)
end
fun binding-env-iter-visitor(initial-env):
  default-env-iter-visitor(binding-env-from-env(initial-env), binding-handlers)
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
                  A.s-app(l2, lnk, [_args.first,
                      (A.s-app(l, A.s-dot(f.l, _args.rest.first, f.field), args)).visit(self)]) 
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
  var errors = [] # THE MUTABLE LIST OF ERRORS
  fun add-error(err): errors := err ^ link(errors) end
  ast.visit(binding-env-iter-visitor(initial-env).{
    s-assign(self, loc, id, value):
      cases(Option<Binding>) bind-exp(A.s-id(loc, id), self.env):
        | none => nothing
        | some(b) =>
          when not b.mut:
            add-error(CS.bad-assignment(tostring(id), loc, b.loc))
          end
      end
      value.visit(self)
    end,
  })
  errors
end

fun check-unbound(initial-env, ast):
  var errors = [] # THE MUTABLE LIST OF UNBOUND IDS
  fun add-error(err): errors := err ^ link(errors) end
  fun handle-id(this-id, env):
    when is-none(bind-exp(this-id, env)):
      add-error(CS.unbound-id(this-id))
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
      s-id-letrec(self, loc, id):
        handle-id(A.s-id-letrec(loc, id), self.env)
        true
      end,
      s-assign(self, loc, id, value):
        when is-none(bind-exp(A.s-id(loc, id), self.env)):
          add-error(CS.unbound-var(A.s-assign(loc, id, value)), loc)
        end
        value.visit(self)
      end
    })
  errors
where:
  p = PP.surface-parse(_, "test")
  unbound1 = check-unbound(CS.no-builtins, p("x"))
  unbound1.length() is 1

end

