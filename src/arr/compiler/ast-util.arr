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
  | b-prim(name :: String) # Some "primitive" value supplied by the initial environment
  | b-dict(dict :: SD.StringDict) # Some module supplied by the initial environment
  | b-exp(exp :: A.Expr) # This name is bound to some expression that we can't interpret yet
  | b-dot(bind :: CurBinding, name :: String) # A field lookup off some binding that isn't a b-dict
  | b-unknown # Any unknown value
end

fun bind-exp(e, env) -> Option<CurBinding>:
  cases(A.Expr) e:
    | s_dot(_, o, name) =>
      cases(Option<CurBinding>) bind-exp(o, env):
        | some(b) =>
          cases(CurBinding) b:
            | b-dict(dict) =>
              if dict.has-key(name): some(dict.get(name))
              else: some(b-dot(b, name))
              end
            | else => some(b-dot(b, name))
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
    | else => some(b-exp(e))
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
        cases(A.LetBind) new-letbind:
          | s_let_bind(l2, bind, val) =>
            new-env = acc.env.set(bind.id, bind-exp(val, acc.env).orelse(b-unknown))
            {env: new-env, rev-binds: new-letbind ^ link(acc.rev-binds)}
          | s_var_bind(l2, bind, val) =>
            new-env = acc.env.set(bind.id, b-unknown)
            {env: new-env, rev-binds: new-letbind ^ link(acc.rev-binds)}
        end
      end
      visit-body = body.visit(self.{env: visit-binds.env})
      A.s_let_expr(l, visit-binds.rev-binds.reverse(), visit-body)
    end,
    s_letrec(self, l, binds, body):
      bind-env = for fold(acc from self.env, b from binds):
        acc.set(b.b.id, b-unknown)
      end
      visit-binds = for list.fold(acc from {env: bind-env, rev-binds: []}, b from binds):
        new-letbind = b.visit(self.{env: acc.env})
        new-env = acc.env.set(new-letbind.b.id, bind-exp(new-letbind.value, acc.env).orelse(b-unknown))
        {env: new-env, rev-binds: new-letbind ^ link(acc.rev-binds)}
      end
      visit-body = body.visit(self.{env: visit-binds.env})
      A.s_letrec(l, visit-binds.rev-binds.reverse(), visit-body)
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
    s_lam(self, l, params, args, ann, doc, body, _check):
      new-args = args.map(_.visit(self))
      new-env = for list.fold(acc from self.env, a from new-args):
        acc.set(a.id, b-unknown)
      end
      new-body = body.visit(self.{env: new-env})
      new-check = self.{env: new-env}.option(_check)
      A.s_lam(l, params, new-args, ann, doc, new-body, new-check)
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
            cases(Option<CurBinding>) bind-exp(_link, self.env):
              | some(b) =>
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
              | none =>
                A.s_app(l, f.visit(self), args.map(_.visit(self)))
            end
          | s_id(_, _) =>
            cases(Option<CurBinding>) bind-exp(target, self.env):
              | some(b) => 
                if is-b-prim(b) and (b.name == "list:empty"):
                  args.first.visit(self)
                else:
                  A.s_app(l, f.visit(self), args.map(_.visit(self)))
                end
              | none =>
                A.s_app(l, f.visit(self), args.map(_.visit(self)))
            end
          | s_dot(_, _, _) =>
            cases(Option<CurBinding>) bind-exp(target, self.env):
              | some(b) =>
                if is-b-prim(b) and (b.name == "list:empty"):
                  args.first.visit(self)
                else:
                  A.s_app(l, f.visit(self), args.map(_.visit(self)))
                end
              | none =>
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
  var unbound-ids = [] # THE MUTABLE LIST OF UNBOUND IDS
  fun handle-id(this-id, env):
    when is-none(bind-exp(this-id, env)):
      unbound-ids := [this-id] + unbound-ids
    end
  end
  ast.visit(default-env-visitor(initial-env).{
      s_id(self, loc, id):
        ret = A.s_id(loc, id)
        handle-id(ret, self.env)
        ret
      end,
      s_id_var(self, loc, id):
        ret = A.s_id_var(loc, id)
        handle-id(ret, self.env)
        ret
      end,
      s_id_letrec(self, loc, id):
        ret = A.s_id_letrec(loc, id)
        handle-id(ret, self.env)
        ret
      end
    })
  unbound-ids
end
