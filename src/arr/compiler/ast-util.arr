#lang pyret

provide *
import ast as A
import parse-pyret as PP
import "./compile-structs.arr" as CS
import string-dict as SD

fun ok-last(stmt):
  not (
    A.is-s_let(stmt) or
    A.is-s_var(stmt) or
    A.is-s_fun(stmt) or
    A.is-s_data(stmt) or
    A.is-s_graph(stmt) or
    A.is-s_check(stmt)
  )
end

fun checkers(l): A.s_app(l, A.s_dot(l, A.s_id(l, "builtins"), "current-checker"), []) end

fun append-nothing-if-necessary(prog :: A.Program):
  cases(A.Program) prog:
    | s_program(l, headers, body) =>
      new-body = cases(A.Expr) body:
        | s_block(l, stmts) =>
          last-stmt = stmts.last()
          if ok-last(last-stmt): A.s_block(l, stmts)
          else: A.s_block(l, stmts + [A.s_id(l, "nothing")])
          end
        | else => body
      end
      A.s_program(l, headers, new-body)
  end
end

flatten-single-blocks = A.default-map-visitor.{
    s_block(self, l, stmts):
      if stmts.length() == 1: stmts.first
      else: A.s_block(l, stmts.map(_.visit(self)))
      end
    end
  }

check:
  d = A.dummy-loc
  PP.surface-parse("x", "test").block.visit(flatten-single-blocks) satisfies
    A.equiv-ast(_, A.s_id(d, "x"))
end

merge-nested-blocks = A.default-map-visitor.{
    s_block(self, l, stmts):
      merged-stmts = for fold(new-stmts from [], s from stmts):
        cases(A.Expr) s.visit(self):
          | s_block(l2, stmts2) => stmts2.reverse() + new-stmts
          | else => [s] + new-stmts
        end
      end
      A.s_block(l, merged-stmts.reverse())
    end
  }


fun count-apps(expr):
  var count = 0
  visitor = A.default-map-visitor.{
      s_app(self, l, f, args):
        count := count + 1
        A.s_app(l, f.visit(self), args.map(_.visit(self)))
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
    | s_dot(l, o, name) =>
      cases(Option<BindingInfo>) bind-exp(o, env):
        | some(b) =>
          cases(BindingInfo) b:
            | b-dict(dict) =>
              if dict.has-key(name): some(e-bind(A.dummy-loc, false, dict.get(name)))
              else: some(e-bind(A.dummy-loc, false, b-dot(b, name)))
              end
            | else => some(e-bind(A.dummy-loc, false, b-dot(b, name)))
          end
        | none => none
      end
    | s_id(_, name) =>
      if env.has-key(name): some(env.get(name))
      else: none
      end
    | s_id_var(_, name) =>
      if env.has-key(name): some(env.get(name))
      else: none
      end
    | s_id_letrec(_, name) =>
      if env.has-key(name): some(env.get(name))
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
          m.set(b, e-bind(A.dummy-loc, false, b-prim(name + ":" + b)))
        end
        acc.set(name, e-bind(A.dummy-loc, false, b-dict(mod)))
      | builtin-id(name) => acc.set(name, e-bind(A.dummy-loc, false, b-prim(name)))
    end
  end
end


fun default-env-map-visitor(initial-env):
  A.default-map-visitor.{
    env: binding-env-from-env(initial-env),

    s_program(self, l, headers, body):
      visit-headers = for list.fold(acc from {env: self.env, rev-headers: []}, h from headers):
        new-header = h.visit(self.{env: acc.env})
        new-env = cases(A.Header) new-header:
          | s_import(l2, import-type, name) =>
            cases(A.ImportType) import-type:
              | s_const_import(modname) =>
                if acc.env.has-key(modname): acc.env.set(name, acc.env.get(modname))
                else: acc.env.set(name, e-bind(l2, false, b-unknown))
                end
              | else => acc.env.set(name, e-bind(l2, false, b-unknown))
            end
          | else => acc.env
        end
        {env: new-env, rev-headers: new-header ^ link(acc.rev-headers)}
      end
      visit-body = body.visit(self.{env: visit-headers.env})
      A.s_program(l, visit-headers.rev-headers.reverse(), visit-body)
    end,
    s_let_expr(self, l, binds, body):
      visit-binds = for list.fold(acc from {env: self.env, rev-binds: []}, b from binds):
        new-letbind = b.visit(self.{env: acc.env})
        cases(A.LetBind) new-letbind:
          | s_let_bind(l2, bind, val) =>
            new-env = acc.env.set(bind.id, e-bind(l2, false, bind-or-unknown(val, acc.env)))
            {env: new-env, rev-binds: new-letbind ^ link(acc.rev-binds)}
          | s_var_bind(l2, bind, val) =>
            new-env = acc.env.set(bind.id, e-bind(l2, true, b-unknown))
            {env: new-env, rev-binds: new-letbind ^ link(acc.rev-binds)}
        end
      end
      visit-body = body.visit(self.{env: visit-binds.env})
      A.s_let_expr(l, visit-binds.rev-binds.reverse(), visit-body)
    end,
    s_letrec(self, l, binds, body):
      bind-env = for fold(acc from self.env, b from binds):
        acc.set(b.b.id, e-bind(b.l, false, b-unknown))
      end
      visit-binds = for list.fold(acc from {env: bind-env, rev-binds: []}, b from binds):
        new-letbind = b.visit(self.{env: acc.env})
        new-env =
          acc.env.set(new-letbind.b.id,
            e-bind(b.l, false, bind-or-unknown(new-letbind.value, acc.env)))
        {env: new-env, rev-binds: new-letbind ^ link(acc.rev-binds)}
      end
      visit-body = body.visit(self.{env: visit-binds.env})
      A.s_letrec(l, visit-binds.rev-binds.reverse(), visit-body)
    end,
    s_fun(self, l, name, params, args, ann, doc, body, _check):
      new-args = args.map(_.visit(self))
      name-env = self.env.set(name, e-bind(l, false, b-unknown))
      new-env = for list.fold(acc from name-env, a from new-args):
        acc.set(a.id, e-bind(a.l, false, b-unknown))
      end
      new-body = body.visit(self.{env: new-env})
      new-check = self.{env: name-env}.option(_check)
      A.s_fun(l, name, params, new-args, ann, doc, new-body, new-check)
    end,
    s_lam(self, l, params, args, ann, doc, body, _check):
      new-args = args.map(_.visit(self))
      new-env = for list.fold(acc from self.env, a from new-args):
        acc.set(a.id, e-bind(a.l, false, b-unknown))
      end
      new-body = body.visit(self.{env: new-env})
      new-check = self.{env: new-env}.option(_check)
      A.s_lam(l, params, new-args, ann, doc, new-body, new-check)
    end,
    s_method(self, l, args, ann, doc, body, _check):
      new-args = args.map(_.visit(self))
      new-env = for list.fold(acc from self.env, a from new-args):
        acc.set(a.id, e-bind(a.l, false, b-unknown))
      end
      new-body = body.visit(self.{env: new-env})
      new-check = self.option(_check)
      A.s_method(l, new-args, ann, doc, new-body, new-check)
    end
  }
end


fun default-env-iter-visitor(initial-env):
  A.default-iter-visitor.{
    env: binding-env-from-env(initial-env),

    s_program(self, l, headers, body):
      visit-headers = for list.fold(acc from {env: self.env, ans: true}, h from headers):
        if not acc.ans: acc
        else:
          if h.visit(self.{env: acc.env}):
            cases(A.Header) h:
              | s_import(l2, import-type, name) =>
                cases(A.ImportType) import-type:
                  | s_const_import(modname) =>
                    if acc.env.has-key(modname): {env: acc.env.set(name, acc.env.get(modname)), ans: true}
                    else: {env: acc.env.set(name, e-bind(l2, false, b-unknown)), ans: true}
                    end
                  | else => {env: acc.env.set(name, e-bind(l2, false, b-unknown)), ans: true}
                end
              | else => acc
            end
          else:
            acc.{ans: false}
          end
        end
      end
      visit-headers.ans and body.visit(self.{env: visit-headers.env})
    end,
    s_let_expr(self, l, binds, body):
      visit-binds = for list.fold(acc from {env: self.env, ans: true}, b from binds):
        if not acc.ans: acc
        else:
          if b.visit(self.{env: acc.env}):
            cases(A.LetBind) b:
              | s_let_bind(l2, bind, val) =>
                {env: acc.env.set(bind.id, e-bind(l2, false, bind-or-unknown(val, acc.env))),
                  ans: true}
              | s_var_bind(l2, bind, val) =>
                {env: acc.env.set(bind.id, e-bind(l2, true, b-unknown)), ans: true}
            end
          else:
            acc.{ans: false}
          end
        end
      end
      visit-binds.ans and body.visit(self.{env: visit-binds.env})
    end,
    ## TODO: GET THE RIGHT SHADOWING HERE
    s_letrec_bind(self, l, bind, expr):
      bind.visit(self) and expr.visit(self.{env: self.env.set(bind.id, e-bind(bind.l, false, b-unknown))})
    end,
    s_letrec(self, l, binds, body):
      bind-env = for fold(acc from self.env, b from binds):
        acc.set(b.b.id, e-bind(b.l, false, b-unknown))
      end
      visit-binds = for list.fold(acc from {env: bind-env, ok: true}, b from binds):
        if not acc.ok: acc
        else:
          if b.visit(self.{env: acc.env}):
            new-env = acc.env.set(b.b.id, e-bind(b.l, false, bind-or-unknown(b.value, acc.env)))
            acc.{env: new-env}
          else: acc.{ok: false}
          end
        end
      end
      visit-binds.ok and body.visit(self.{env: visit-binds.env})
    end,
    s_fun(self, l, name, params, args, ann, doc, body, _check):
      if list.all(_.visit(self), args):
        name-env = self.env.set(name, e-bind(l, false, b-unknown))
        new-env = for list.fold(acc from name-env, a from args):
          acc.set(a.id, e-bind(a.l, false, b-unknown))
        end
        body.visit(self.{env: new-env}) and self.{env: name-env}.option(_check)
      else: false
      end
    end,
    s_lam(self, l, params, args, ann, doc, body, _check):
      if list.all(_.visit(self), args):
        new-env = for list.fold(acc from self.env, a from args):
          acc.set(a.id, e-bind(a.l, false, b-unknown))
        end
        body.visit(self.{env: new-env}) and self.{env: new-env}.option(_check)
      else: false
      end
    end,
    s_method(self, l, args, ann, doc, body, _check):
      if list.all(_.visit(self), args):
        new-env = for list.fold(acc from self.env, a from args):
          acc.set(a.id, e-bind(a.l, false, b-unknown))
        end
        body.visit(self.{env: new-env}) and self.option(_check)
      else: false
      end
    end
  }
end


fun link-list-visitor(initial-env):
  default-env-map-visitor(initial-env).{
    s_app(self, l, f, args):
      if A.is-s_dot(f) and (f.field == "_plus"):
        target = f.obj
        cases(A.Expr) target:
          | s_app(l2, _link, _args) =>
            cases(BindingInfo) bind-or-unknown(_link, self.env):
              | b-prim(n) =>
                if n == "list:link":
                  A.s_app(l2, _link, [_args.first,
                      (A.s_app(l, A.s_dot(f.l, _args.rest.first, f.field), args)).visit(self)]) 
                else if n == "list:empty":
                  args.first.visit(self)
                else:
                  A.s_app(l, f.visit(self), args.map(_.visit(self)))
                end
              | else =>
                A.s_app(l, f.visit(self), args.map(_.visit(self)))
            end
          | s_id(_, _) =>
            cases(BindingInfo) bind-or-unknown(target, self.env):
              | b-prim(name) =>
                if (name == "list:empty"):
                  args.first.visit(self)
                else:
                  A.s_app(l, f.visit(self), args.map(_.visit(self)))
                end
              | else =>
                A.s_app(l, f.visit(self), args.map(_.visit(self)))
            end
          | s_dot(_, _, _) =>
            cases(BindingInfo) bind-or-unknown(target, self.env):
              | b-prim(name) =>
                if (name == "list:empty"):
                  args.first.visit(self)
                else:
                  A.s_app(l, f.visit(self), args.map(_.visit(self)))
                end
              | else =>
                A.s_app(l, f.visit(self), args.map(_.visit(self)))
            end
          | else =>
            A.s_app(l, f.visit(self), args.map(_.visit(self)))
        end
      else:
        A.s_app(l, f.visit(self), args.map(_.visit(self)))
      end
    end
  }
end

fun check-unbound(initial-env, ast):
  var errors = [] # THE MUTABLE LIST OF UNBOUND IDS
  fun add-error(err): errors := err ^ link(errors) end
  fun handle-id(this-id, env):
    when (this-id.id <> "_") and is-none(bind-exp(this-id, env)): # FIX when we have real underscores
      add-error(CS.unbound-id(this-id))
    end
  end
  ast.visit(default-env-iter-visitor(initial-env).{
      s_id(self, loc, id):
        handle-id(A.s_id(loc, id), self.env)
        true
      end,
      s_id_var(self, loc, id):
        handle-id(A.s_id_var(loc, id), self.env)
        true
      end,
      s_id_letrec(self, loc, id):
        handle-id(A.s_id_letrec(loc, id), self.env)
        true
      end,
      s_assign(self, loc, id, value):
        cases(Option<Binding>) bind-exp(A.s_id(loc, id), self.env):
          | none => add-error(CS.unbound-var(A.s_assign(loc, id, value)))
          | some(b) =>
            when not b.mut:
              add-error(CS.bad-assignment(id, loc, b.loc))
            end
        end
        value.visit(self)
      end,
      s_let_bind(self, l, bind, expr):
        cases(Option<Binding>) bind-exp(A.s_id(bind.l, bind.id), self.env):
          | none => nothing
          | some(b) =>
            if bind.shadows: nothing
            else:
              if b.mut: add-error(CS.mixed-id-var(bind.id, b.loc, bind.l))
              else if bind.id == "self": nothing
              else if bind.id == "_": nothing # FIX when we have real underscores
              else:
                add-error(CS.shadow-id(bind.id, bind.l, b.loc))
              end
            end
        end
        bind.visit(self) and expr.visit(self)
      end,
      s_var_bind(self, l, bind, expr):
        cases(Option<Binding>) bind-exp(A.s_id(bind.l, bind.id), self.env):
          | none => nothing
          | some(b) =>
            if bind.shadows: nothing
            else:
              if not b.mut: add-error(CS.mixed-id-var(bind.id, bind.l, b.loc))
              else if bind.id == "self": nothing
              else if bind.id == "_":
                add-error(CS.pointless-var(bind.l))
              else:
                add-error(CS.shadow-id(bind.id, bind.l, b.loc))
              end
            end
        end
        bind.visit(self) and expr.visit(self)
      end,
      s_letrec_bind(self, l, bind, expr):
        bind.visit(self) and expr.visit(self.{env: self.env.set(bind.id, e-bind(bind.l, false, b-unknown))})
      end,
      ## AND HERE
      s_letrec(self, l, binds, body):
        for list.each(letrec-bind from binds):
          bind = letrec-bind.b
          cases(Option<Binding>) bind-exp(A.s_id(bind.l, bind.id), self.env):
            | none => nothing
            | some(b) =>
              if b.mut: add-error(CS.mixed-id-var(bind.id, b.loc, bind.l))
              else:
                if bind.id == "self": nothing
                else if bind.id == "_": nothing # FIX when we have real underscores
                else if bind.shadows: nothing
                else:
                  add-error(CS.shadow-id(bind.id, bind.l, b.loc))
                end
              end
          end
        end
        bind-env = for fold(acc from self.env, b from binds):
          acc.set(b.b.id, e-bind(b.l, false, b-unknown))
        end
        good-binds = for fold(acc from true, letrec-bind from binds):
          acc and letrec-bind.b.visit(self) and letrec-bind.value.visit(self.{env: bind-env})
        end
        good-binds and body.visit(self.{env: bind-env})
      end
    })
  errors
end
