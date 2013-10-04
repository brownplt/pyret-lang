#lang pyret

import ast as A
import format as format

provide *

fun program-to-js(ast):
  cases(A.Program) ast:
    # import/provide ignored
    | s_program(_, _, block) =>
      format("(function(RUNTIME) {
         return ~a;
       })", [expr-to-js(block)])
  end
end

fun expr-to-js(ast):
  cases(A.Expr) ast:
    | s_block(_, stmts) =>
      if stmts.length() == 0:
        "RUNTIME.nothing"
      else:
        fun sequence-return-last(ss):
          cases(list.List) ss:
            | link(f, r) =>
              cases(list.List) r:
                | empty => format("return ~a;", [expr-to-js(f)])
                | link(_, _) =>
                  format("~a;\n", [expr-to-js(f)]) + sequence-return-last(r)
              end
          end
        end
        format("(function(){ ~a })", [sequence-return-last(stmts)])
      end
    | s_num(_, n) =>
      format("RUNTIME.makeNumber(~a)", [n])
    | s_app(_, f, args) =>
      format("~a.app(~a)", [expr-to-js(f), args.map(expr-to-js).join-str(",")])
    | s_bracket(_, obj, f) =>
      cases (A.Expr) f:
        | s_str(_, s) => format("RUNTIME.getField(~a, '~a')", [expr-to-js(obj), s])
        | else => raise("Non-string lookups not supported")
      end
    | else => raise("Not yet implemented") 
  end
end

