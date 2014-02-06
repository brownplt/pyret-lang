#lang pyret

provide *
import ast as A
import "desugar.arr" as D
import parse-pyret as PP
import "compile-structs.arr" as CS
import string-dict as SD

flatten-single-blocks = A.default-map-visitor.{
    s_block(self, s, stmts):
      if stmts.length() == 1: stmts.first
      else: A.s_block(s, stmts.map(_.visit(self)))
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

check:
  d = A.dummy-loc
  D.desugar-expr(D.mt-d-env, PP.surface-parse("x block: y z end w", "test").block)
    .visit(merge-nested-blocks) satisfies
      A.equiv-ast(_, A.s_block(d, [
          A.s_id(d, "x"),
          A.s_id(d, "y"),
          A.s_id(d, "z"),
          A.s_id(d, "w")
        ]))
end

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
    | else => b-exp(e)
  end
end

fun default-env-visitor(initial-env):
  initial-dict = for list.fold(acc from SD.immutable-string-dict(), module from initial-env.modules):
    mod = for list.fold(m from SD.immutable-string-dict(), b from module.bindings):
      m.set(b, b-prim(module.name + ":" + b))
    end
    acc.set(module.name, b-dict(mod))
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
      new-check = _check.visit(self.{env: name-env})
      A.s_fun(l, name, params, new-args, ann, doc, new-body, new-check)
    end,
    s_method(self, l, args, ann, doc, body, _check):
      new-args = args.map(_.visit(self))
      new-env = for list.fold(acc from self.env, a from new-args):
        acc.set(a.id, b-unknown)
      end
      new-body = body.visit(self.{env: new-env})
      new-check = _check.visit(self)
      A.s_method(l, new-args, ann, doc, new-body, new-check)
    end
  }
end

fun link-list-visitor(initial-env):
  default-env-visitor(initial-env).{
    s_app(self, l, f, args):
      if A.is-s_dot(f) and (f.field == "_plus"):
        print("Found an expr that is a plus: " + tostring(f.obj))
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


#check:
#  pf = fun(f): PP.surface-parse(F.file-to-string(f), f);
#  ds = D.desugar(_, CS.standard-builtins)
#  ds(pf("builtin-libs/ast.arr"))^count-apps() is 2
#  ds(pf("builtin-libs/list.arr"))^count-apps() is 2
#  ds(pf("js-ast.arr"))^count-apps() is 2
#end
