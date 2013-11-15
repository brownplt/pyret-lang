#lang pyret

import ast as A
import json as J
import format as format

provide *

fun make-checker-name(name): "is-" + name;

fun flatten(list-of-lists :: List):
  for fold(biglist from [], piece from list-of-lists):
    biglist + piece
  end
end

fun binding-ids(stmt):
  fun variant-ids(variant):
    cases(A.Variant) variant:
      | s_variant(_, name, _, _) => [name, make-checker-name(name)]
      | s_singleton-variant(_, name, _, _) => [name, make-checker-name(name)]
    end
  end
  cases(A.Expr) stmt:
    | s_let(_, b, _) => [b.id]
    | s_var(_, b, _) => [b.id]
    | s_graph(_, bindings) => flatten(bindings.map(binding-ids))
    | s_data(_, name, _, _, variants, _, _) =>
      [name] + flatten(variants.map(variant-ids))
    | else => []
  end
end

fun toplevel-ids(program):
  cases(A.Program) program:
    | s_program(_, _, b) =>
      cases(A.Expr) b:
        | s_block(_, stmts) => flatten(stmts.map(binding-ids))
        | else => raise("Non-block given to toplevel-ids")
      end
    | else => raise("Non-program given to toplevel-ids")
  end
end

js-id-of = block:
  var js-ids = {}
  fun(id :: String):
    if builtins.has-field(js-ids, id):
      js-ids.[id]
    else:
      no-hyphens = id.replace("-", "_DASH_")
      safe-id = gensym(no-hyphens)
      js-ids := js-ids.{ [id]: safe-id }
      safe-id
    end
  end
end

fun program-to-js(ast, runtime-ids):
  cases(A.Program) ast:
    # import/provide ignored
    | s_program(_, _, block) =>
      cases(A.Expr) block :
        | s_block(_, stmts) =>
          bindings = for list.fold(bs from "", id from runtime-ids):
            bs + format("var ~a = NAMESPACE.get('~a');\n", [js-id-of(id), id])
          end
          program-body = if stmts.length() == 0:
            "RESULT = NAMESPACE.get('nothing');"
          else:
            fun sequence-assign-last(ss):
              cases(list.List) ss:
                | link(f, r) =>
                  cases(list.List) r:
                    | empty =>
                      fun ends-in-bind(e):
                        format("~a;\nRESULT = NAMESPACE.get('nothing');", [expr-to-js(f)])
                      end
                      cases(A.Expr) f:
                        | s_let(_, _, _) => ends-in-bind(f)
                        | s_var(_, _, _) => ends-in-bind(f)
                        | else => format("RESULT = ~a;", [expr-to-js(f)])
                      end
                    | link(_, _) =>
                      format("~a;\n", [expr-to-js(f)]) + sequence-assign-last(r)
                  end
              end
            end
            sequence-assign-last(stmts)
          end
          ids-to-export = toplevel-ids(ast)
          export-fields = for list.fold(export from "", id from ids-to-export):
            export + format("EXPORT_NAMESPACE = EXPORT_NAMESPACE.set(\"~a\", ~a)\n",
              [id, js-id-of(id)])
          end
          # function(R, N, success, fail)
          # function(R, N, conts)
          # $K is { success : NormalResult -> Undef, failure : FailResult -> Undef }
          format("(function(RUNTIME, NAMESPACE, $K) {
            try {
              ~a
              var RESULT;
              var EXPORT_NAMESPACE = Namespace({});
              (function() {
                var k = RUNTIME.makeFunction(function(RESULT) {
                  $K.success(RUNTIME.makeNormalResult(RESULT, EXPORT_NAMESPACE));
                });
                (function() { return ~a})().app(k);
              })();
            } catch(e) {
              $K.failure(RUNTIME.makeFailResult(e));
            }
          })", [bindings, expr-to-js(cps(block))])
      end
  end
end

fun do-block(str):
  format("(function() { ~a })()", [str])
end

fun cps(ast):
  cases(A.Expr) ast:
    | s_block(l, stmts) => cps(stmts.first)
    | s_num(l, n) =>
      A.s_lam(l, [], [A.s_bind(l, "k", A.a_blank)], A.a_blank, "cpsed", A.s_app(l, A.s_id(l, "k"), [A.s_num(l, n)]), A.s_block(l, []))
    | else => ast
  end
end

fun expr-to-js(ast):
  cases(A.Expr) ast:
    | s_block(_, stmts) =>
      if stmts.length() == 0:
        "NAMESPACE.get('nothing')"
      else:
        fun sequence-return-last(ss):
          cases(list.List) ss:
            | link(f, r) =>
              cases(list.List) r:
                | empty => format("return ~a;", [expr-to-js(f)])
                  fun ends-in-bind(e):
                    format("~a;\nreturn NAMESPACE.get('nothing');", [expr-to-js(f)])
                  end
                  cases(A.Expr) f:
                    | s_let(_, _, _) => ends-in-bind(f)
                    | s_var(_, _, _) => ends-in-bind(f)
                    | else => format("return ~a;", [expr-to-js(f)])
                  end
                | link(_, _) =>
                  format("~a;\n", [expr-to-js(f)]) + sequence-return-last(r)
              end
          end
        end
        format("(function(){\n ~a \n})()", [sequence-return-last(stmts)])
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
    | s_id(_, id) => js-id-of(id)
    | s_let(_, bind, value) => format("var ~a = ~a;", [js-id-of(bind.id), expr-to-js(value)])
    | s_lam(_, _, binds, _, _, body, _) =>
      names = binds.map(fun(b): b.id end).map(js-id-of)
      format("RUNTIME.makeFunction(function(~a) { return ~a })", [names.join-str(","), expr-to-js(body)])
    | else => do-block(format("throw new Error('Not yet implemented ~a')", [ast.label()]))
  end
end

