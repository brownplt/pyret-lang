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

data CurBinding:
  | b-prim(name :: String)
  | b-dict(dict :: SD.StringDict)
  | b-exp(exp :: A.Expr)
  | b-dot(bind :: CurBinding, name :: String)
  | b-unknown
end

fun bind-exp(e, env):
  cases(A.Expr) e:
    | s_dot(_, o, name) =>
      b = bind-exp(o, env)
      cases(CurBinding) b:
        | b-dict(dict) =>
          if dict.has-key(name): dict.get(name)
          else: b-dot(b, name)
          end
        | else => b-dot(b, name)
      end
    | s_id(_, name) =>
      if env.has-key(name): env.get(name)
      else: b-unknown
      end
    | s_id_var(_, name) =>
      if env.has-key(name): env.get(name)
      else: b-unknown
      end
    | s_id_letrec(_, name) =>
      if env.has-key(name): env.get(name)
      else: b-unknown
      end
    | else => b-exp(e)
  end
end

fun default-env-visitor(initial-env):
  initial-dict = for list.fold(acc from SD.immutable-string-dict(), binding from initial-env.bindings):
    cases(C.CompileBinding) binding:
      | module-bindings(name, ids) =>
        mod = for list.fold(m from SD.immutable-string-dict(), b from ids):
          m.set(b, b-prim(name + ":" + b))
        end
        acc.set(name, b-dict(mod))
      | builtin-id(name) => acc.set(name, b-prim(name))
    end
  end
  A.default-map-visitor.{
    env: initial-dict,

    s_program(self, l, headers, body):
      visit-headers = for list.fold(acc from {env: self.env, rev-headers: []}, h from headers):
        new-header = h.visit(self.{env: acc.env})
        new-env = cases(A.Header) new-header:
          | s_import(_, import-type, name) =>
            cases(A.ImportType) import-type:
              | s_const_import(modname) =>
                if acc.env.has-key(modname): acc.env.set(name, acc.env.get(modname))
                else: acc.env.set(name, b-unknown)
                end
              | else => acc.env.set(name, b-unknown)
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
        new-env = cases(A.Expr) new-letbind.value:
          | s_dot(_, _, _) => acc.env.set(new-letbind.b.id, bind-exp(new-letbind.value, acc.env))
          | s_id(_, _) => acc.env.set(new-letbind.b.id, bind-exp(new-letbind.value, acc.env))
          | else => acc.env
        end
        {env: new-env, rev-binds: new-letbind ^ link(acc.rev-binds)}
      end
      visit-body = body.visit(self.{env: visit-binds.env})
      A.s_let_expr(l, visit-binds.rev-binds.reverse(), visit-body)
    end,
    s_letrec_expr(self, l, binds, body):
      visit-binds = for list.fold(acc from {env: self.env, rev-binds: []}, b from binds):
        new-letbind = b.visit(self.{env: acc.env})
        new-env = cases(A.Expr) new-letbind.value:
          | s_dot(_, _, _) => acc.env.set(new-letbind.b.id, bind-exp(new-letbind.value, acc.env))
          | s_id(_, _) => acc.env.set(new-letbind.b.id, bind-exp(new-letbind.value, acc.env))
          | else => acc.env
        end
        {env: new-env, rev-binds: new-letbind ^ link(acc.rev-binds)}
      end
      visit-body = body.visit(self.{env: visit-binds.env})
      A.s_letrec_expr(l, visit-binds.rev-binds.reverse(), visit-body)
    end,
    s_fun(self, l, name, params, args, ann, doc, body, _check):
      new-args = args.map(_.visit(self))
      name-env = self.env.set(name, b-unknown)
      new-env = for list.fold(acc from name-env, a from new-args):
        acc.set(a.id, b-unknown)
      end
      new-body = body.visit(self.{env: new-env})
      new-check = self.{env: name-env}.option(_check)
      A.s_fun(l, name, params, new-args, ann, doc, new-body, new-check)
    end,
    s_method(self, l, args, ann, doc, body, _check):
      new-args = args.map(_.visit(self))
      new-env = for list.fold(acc from self.env, a from new-args):
        acc.set(a.id, b-unknown)
      end
      new-body = body.visit(self.{env: new-env})
      new-check = self.option(_check)
      A.s_method(l, new-args, ann, doc, new-body, new-check)
    end
  }
end

fun link-list-visitor(initial-env):
  default-env-visitor(initial-env).{
    s_app(self, l, f, args):
      if A.is-s_dot(f) and (f.field == "_plus"):
        target = f.obj
        cases(A.Expr) target:
          | s_app(l2, _link, _args) =>
            b = bind-exp(_link, self.env)
            cases(CurBinding) b:
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
            b = bind-exp(target, self.env)
            if is-b-prim(b) and (b.name == "list:empty"):
              args.first.visit(self)
            else:
              A.s_app(l, f.visit(self), args.map(_.visit(self)))
            end
          | s_dot(_, _, _) =>
            b = bind-exp(target, self.env)
            if is-b-prim(b) and (b.name == "list:empty"):
              args.first.visit(self)
            else:
              A.s_app(l, f.visit(self), args.map(_.visit(self)))
            end
          | else =>
            A.s_app(l, f.visit(self), args.map(_.visit(self)))
        end
      else:
        A.s_app(l, f.visit(self), args.map(_.visit(self)))
      end
      # cases(A.Expr) f:
      #   | s_dot(_, o, name) =>
    end
  }
end

fun check-unbound(initial-env, ast):
  initial-dict = for list.fold(acc from SD.immutable-string-dict(), binding from initial-env.bindings):
    cases(C.CompileBinding) binding:
      | module-bindings(name, ids) => acc.set(name, true)
      | builtin-id(name) => acc.set(name, true)
    end
  end
  var unbound-ids = []
  fun handle-id(this-id, env):
    when not (env.has-key(this-id.id)):
      unbound-ids := [this-id] + unbound-ids
    end
    this-id
  end
  ast.visit(A.default-map-visitor.{
      env: initial-dict,
      s_program(self, loc, headers, body):
        header-env = for fold(acc from self.env, h from headers):
          cases(A.Header) h:
            | s_import(l, _, name) => acc.set(name, true)
            | else => acc
          end
        end
        A.s_program(loc, headers, body.visit(self.{ env: header-env }))
      end,
      s_let_expr(self, loc, binds, body):
        bind-pair = for fold(acc from { env: self.env, new-binds: [] }, b from binds):
          cases(A.LetrecBind) b:
            | s_let_bind(l, b, val) =>
              {
                env: acc.env.set(b.id, true),
                new-binds: link(
                    A.s_let_bind(l, b, val.visit(self.{ env: acc.env })),
                    acc.new-binds
                  )
              }
            | s_var_bind(l, b, val) =>
              {
                env: acc.env.set(b.id, true),
                new-binds: link(
                    A.s_var_bind(l, b, val.visit(self.{ env: acc.env })),
                    acc.new-binds
                  )
              }
          end
        end
        A.s_let_expr(
            loc,
            bind-pair.new-binds.reverse(),
            body.visit(self.{ env: bind-pair.env })
          )
      end,
      s_letrec(self, loc, binds, body):
        new-env = for fold(acc from self.env, b from binds):
          acc.set(b.b.id, true)
        end
        binds.map(_.visit(self.{env: new-env}))
        A.s_letrec(loc, binds, body.visit(self.{env: new-env}))
      end,





      s_lam(self, loc, ps, args, ann, doc, body, chk):
        new-env = for fold(acc from self.env, a from args): acc.set(a.id, true);
        new-body = body.visit(self.{env: new-env})
        A.s_lam(loc, ps, args, ann, doc, new-body, chk)
      end,
      s_method(self, loc, args, ann, doc, body, chk):
        new-env = for fold(acc from self.env, a from args): acc.set(a.id, true);
        new-body = body.visit(self.{env: new-env})
        A.s_method(loc, args, ann, doc, new-body, chk)
      end,
      s_id(self, loc, id):
        handle-id(A.s_id(loc, id), self.env)
      end,
      s_id_var(self, loc, id):
        handle-id(A.s_id_var(loc, id), self.env)
      end,
      s_id_letrec(self, loc, id):
        handle-id(A.s_id_letrec(loc, id), self.env)
      end
    })
  unbound-ids
end

