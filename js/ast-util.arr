#lang pyret

provide *
import ast as A
import "desugar.arr" as D
import parse-pyret as PP
import "compile-structs.arr" as CS

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

#check:
#  pf = fun(f): PP.surface-parse(F.file-to-string(f), f);
#  ds = D.desugar(_, CS.standard-builtins)
#  ds(pf("builtin-libs/ast.arr"))^count-apps() is 2
#  ds(pf("builtin-libs/list.arr"))^count-apps() is 2
#  ds(pf("js-ast.arr"))^count-apps() is 2
#end
