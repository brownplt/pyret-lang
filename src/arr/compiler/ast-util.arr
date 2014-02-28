#lang pyret

provide *
import ast as A
import parse-pyret as PP
import "./compile-structs.arr" as CS
import string-dict as SD
import either as E

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

fun <a> default-env-map-visitor(
    initial-env :: a,
    bind-handlers :: {
        s_letrec_bind :: (A.LetrecBind, a -> a),
        s_let_bind :: (A.LetBind, a -> a),
        s_bind :: (A.Bind, a -> a),
        s_header :: (A.Header, a -> a)
      }
    ):
  A.default-map-visitor.{
    env: initial-env,

    s_program(self, l, headers, body):
      imports = headers.filter(A.is-s_import)
      imported-env = for fold(acc from self.env, i from imports):
        bind-handlers.s_header(i, acc)
      end
      visit-headers = for map(h from headers):
        h.visit(self.{ env: imported-env })
      end
      visit-body = body.visit(self.{env: imported-env})
      A.s_program(l, visit-headers, visit-body)
    end,
    s_let_expr(self, l, binds, body):
      bound-env = for fold(acc from { e: self.env, bs : [] }, b from binds):
        this-env = bind-handlers.s_let_bind(b, acc.e)
        new-bind = b.visit(self.{env : acc.e})
        {
          e: this-env,
          bs: link(new-bind, acc.bs)
        }
      end
      visit-binds = bound-env.bs.reverse()
      visit-body = body.visit(self.{env: bound-env.e})
      A.s_let_expr(l, visit-binds, visit-body)
    end,
    s_letrec(self, l, binds, body):
      bind-env = for fold(acc from self.env, b from binds):
        bind-handlers.s_letrec_bind(b, acc)
      end
      new-visitor = self.{env: bind-env}
      visit-binds = binds.map(_.visit(new-visitor))
      visit-body = body.visit(new-visitor)
      A.s_letrec(l, visit-binds, visit-body)
    end,
    s_lam(self, l, params, args, ann, doc, body, _check):
      args-env = for list.fold(acc from self.env, a from args):
        bind-handlers.s_bind(a, acc)
      end
      new-args = args.map(_.visit(self))
      new-body = body.visit(self.{env: args-env})
      new-check = self.{env: args-env}.option(_check)
      A.s_lam(l, params, new-args, ann, doc, new-body, new-check)
    end,
    s_method(self, l, args, ann, doc, body, _check):
      args-env = for list.fold(acc from self.env, a from args):
        bind-handlers.s_bind(a, acc)
      end
      new-args = args.map(_.visit(self))
      new-body = body.visit(self.{env: args-env})
      new-check = self.{env: args-env}.option(_check)
      A.s_method(l, new-args, ann, doc, new-body, new-check)
    end
  }
end


fun <a> default-env-iter-visitor(
    initial-env :: a,
    bind-handlers :: {
        s_letrec_bind :: (A.LetrecBind, a -> a),
        s_let_bind :: (A.LetBind, a -> a),
        s_bind :: (A.Bind, a -> a),
        s_header :: (A.Header, a -> a)
      }
    ):
  A.default-iter-visitor.{
    env: initial-env,

    s_program(self, l, headers, body):
      imports = headers.filter(A.is-s_import)
      imported-env = for fold(acc from self.env, i from imports):
        bind-handlers.s_header(i, acc)
      end
      new-visitor = self.{ env: imported-env }
      list.all(_.visit(new-visitor), headers) and body.visit(new-visitor)
    end,
    s_let_expr(self, l, binds, body):
      bound-env = for list.fold-while(acc from { e: self.env, bs: true }, b from binds):
        this-env = bind-handlers.s_let_bind(b, acc.e)
        new-bind = b.visit(self.{env : acc.e})
        if new-bind:
          E.left({ e: this-env, bs: true })
        else:
          E.right({ e: this-env, bs: false })
        end
      end
      bound-env.bs and body.visit(self.{env: bound-env.e})
    end,
    s_letrec(self, l, binds, body):
      bind-env = for list.fold(acc from self.env, b from binds):
        bind-handlers.s_letrec_bind(b, acc)
      end
      new-visitor = self.{env: bind-env}
      continue-binds = for list.fold-while(acc from true, b from binds):
        if b.visit(new-visitor): E.left(true) else: E.right(false) end
      end
      continue-binds and body.visit(new-visitor)
    end,
    s_lam(self, l, params, args, ann, doc, body, _check):
      args-env = for list.fold(acc from self.env, a from args):
        bind-handlers.s_bind(a, acc)
      end
      list.all(_.visit(self), args) and
        body.visit(self.{env: args-env}) and
        self.{env: args-env}.option(_check)
    end,
    s_method(self, l, args, ann, doc, body, _check):
      args-env = for list.fold(acc from self.env, a from args):
        bind-handlers.s_bind(a, acc)
      end
      list.all(_.visit(self), args) and
        body.visit(self.{env: args-env}) and
        self.{env: args-env}.option(_check)
    end
  }
end

binding-handlers = {
  s_header(_, imp, env):
    cases(A.ImportType) imp.file:
      | s_const_import(modname) =>
        if env.has-key(modname): env.set(imp.name, env.get(modname))
        else: env.set(imp.name, e-bind(imp.l, false, b-unknown))
        end
      | else => env.set(imp.name, e-bind(imp.l, false, b-unknown))
    end
  end,
  s_let_bind(_, lb, env):
    cases(A.LetBind) lb:
      | s_let_bind(l2, bind, val) =>
        env.set(bind.id, e-bind(l2, false, bind-or-unknown(val, env)))
      | s_var_bind(l2, bind, val) =>
        env.set(bind.id, e-bind(l2, true, b-unknown))
    end
  end,
  s_letrec_bind(_, lrb, env):
    env.set(lrb.b.id,
      e-bind(lrb.l, false, bind-or-unknown(lrb.value, env)))
  end,
  s_bind(_, b, env):
    env.set(b.id, e-bind(b.l, false, b-unknown))
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

fun check-unbound(initial-env, ast, options):
  var errors = [] # THE MUTABLE LIST OF UNBOUND IDS
  fun add-error(err): errors := err ^ link(errors) end
  fun handle-id(this-id, env):
    when (this-id.id <> "_") and is-none(bind-exp(this-id, env)): # FIX when we have real underscores
      add-error(CS.unbound-id(this-id))
    end
  end
  ast.visit(binding-env-iter-visitor(initial-env).{
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
            if bind.shadows:
              when (bind.id == "_") and (not options.allow-shadowed):
                add-error(CS.pointless-shadow(bind.l))
              end
            else:
              if b.mut: add-error(CS.mixed-id-var(bind.id, b.loc, bind.l))
              else if bind.id == "self": nothing
              else if bind.id == "_": nothing # FIX when we have real underscores
              else:
                when not options.allow-shadowed:
                  add-error(CS.shadow-id(bind.id, bind.l, b.loc))
                end
              end
            end
        end
        bind.visit(self) and expr.visit(self)
      end,
      s_var_bind(self, l, bind, expr):
        cases(Option<Binding>) bind-exp(A.s_id(bind.l, bind.id), self.env):
          | none => nothing
          | some(b) =>
            if bind.shadows: 
              when (bind.id == "_") and (not options.allow-shadowed):
                add-error(CS.pointless-shadow(bind.l))
              end
            else:
              if not b.mut: add-error(CS.mixed-id-var(bind.id, bind.l, b.loc))
              else if bind.id == "self": nothing
              else if bind.id == "_":
                add-error(CS.pointless-var(bind.l))
              else:
                when not options.allow-shadowed:
                  add-error(CS.shadow-id(bind.id, bind.l, b.loc))
                end
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
                  when not options.allow-shadowed:
                    add-error(CS.shadow-id(bind.id, bind.l, b.loc))
                  end
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
