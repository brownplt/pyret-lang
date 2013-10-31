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
    else: no-hyphens = id.replace("-", "_DASH_")
      safe-id = gensym(no-hyphens)
      js-ids := js-ids.{ [id]: safe-id }
      safe-id
    end
  end
end


fun id-access(id :: String):
#    format("RUNTIME.ids[\"~a\"]" ,[js-id-of(id)])
js-id-of(id)
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
          format("(function(RUNTIME, NAMESPACE) {
            try {
              ~a
              var RESULT;
              var EXPORT_NAMESPACE = Namespace({});
              (function() {
                ~a
                ~a
              })();
              return RUNTIME.makeNormalResult(RESULT, EXPORT_NAMESPACE);
            } catch(e) {
              return RUNTIME.makeFailResult(e);
            }
          })", [bindings, program-body, export-fields])
      end
  end
end

fun do-block(str):
  format("(function() { ~a })()", [str])
end

fun make-field-js(field):
    format("'~a' : ~a", [field.name.s, expr-to-js(field.value)])
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
                | link(_, _) =>
                  format("~a;\n", [expr-to-js(f)]) + sequence-return-last(r)
              end
          end
        end
        format("(function(){\n ~a \n})()", [sequence-return-last(stmts)])
      end
    | s_user_block(s, expr) => expr-to-js(expr)
    | s_num(_, n) =>
      format("RUNTIME.makeNumber(~a)", [n])
    | s_str(_, s) =>
      format("RUNTIME.makeString(~s)", [s])
    | s_bool(_, b) =>
      format("RUNTIME.makeBoolean(~a)", [b])
    | s_lam(_, _, args, _, doc, body, _) =>
      #format("RUNTIME.makeFunction(function(~a) {~a \nreturn ~a; })", [args.map(fun(b): js-id-of(b.id);).join-str(","),args.map(fun(b): format("~a = ~a;\n", [id-access(b.id), js-id-of(b.id)]);).join-str(""),expr-to-js(body)])
      format("RUNTIME.makeFunction(function(~a) {return ~a; }, ~s)", [args.map(fun(b): js-id-of(b.id);).join-str(","),expr-to-js(body), doc])
    #Should check that body is a block
    | s_method(_, args, _, doc, body, _) =>
      #format("RUNTIME.makeMethod(function(~a) {~a \nreturn ~a; })", [args.map(fun(b): js-id-of(b.id);).join-str(","),args.map(fun(b): format("~a = ~a;\n", [id-access(b.id), js-id-of(b.id)]);).join-str(""),expr-to-js(body)])
      format("RUNTIME.makeMethod(function(~a) {return ~a; }, ~s)", [args.map(fun(b): js-id-of(b.id);).join-str(","),expr-to-js(body), doc])
    | s_app(_, f, args) =>
      format("RUNTIME.applyFunction(~a, [~a])", [expr-to-js(f), args.map(expr-to-js).join-str(",")])
    | s_bracket(_, obj, f) =>
      cases (A.Expr) f:
        | s_str(_, s) => format("RUNTIME.getField(~a, '~a')", [expr-to-js(obj), s])
        | else => raise("Non-string lookups not supported")
      end
    | s_colon_bracket(_, obj, f) =>
      cases (A.Expr) f:
        | s_str(_, s) => format("RUNTIME.getColonField(~a, '~a')", [expr-to-js(obj), s])
        | else => raise("Non-string lookups not supported")
      end
    | s_id(_, id) => id-access(id)
    | s_var(_, bind, value) =>
      js_id = id-access(bind.id)
      format("var ~a = ~a", [js_id, expr-to-js(value)])
    | s_assign(_, id, value) =>
      format("~a = ~a", [id-access(id), expr-to-js(value)])
    | s_let(_, bind, value) => 
      format("~a = ~a",[id-access(bind.id), expr-to-js(value)])
      #js_id = id-access(bind.id)
      #format("(function(){
            #if(typeof ~a === \'undefined\') {
                #return ~a = ~a;
            #} else {
                #throw \"NO SHADOWING\";
                #}
            #})()",[js_id, js_id, expr-to-js(value)])
    | s_obj(_, fields) =>
        format("RUNTIME.makeObj({~a})",[fields.map(make-field-js).join-str(",\n")])
    | s_extend(_, obj, fields) =>
        format("~a.extendWith({~a})", [expr-to-js(obj), fields.map(make-field-js).join-str(",\n")])
    | s_if_else(_, branches,_else) =>
       all_but_else = cases(list.List) branches:
        | link(f, r) => format("if(RUNTIME.checkBool(~a)) {return ~a;}\n~a",[expr-to-js(f.test), expr-to-js(f.body), r.map(fun(x): format("else if(RUNTIME.checkBool(~a)) {return ~a;}",[expr-to-js(x.test), expr-to-js(x.body)]);).join-str("\n")])
        end
        ifblock = format("~a else{return ~a;}", [all_but_else, expr-to-js(_else)])
        format("(function() {~a})()",[ifblock])
    | s_try(_, body, bind, _except) =>
        format("(function() {\n try{\n  return ~a; \n} catch(~a) {\n return ~a; \n}})()", [expr-to-js(body), js-id-of(bind.id), expr-to-js(_except)])
    | s_get_bang(l, obj, field) => 
        format("RUNTIME.applyFunction(RUNTIME.getField(RUNTIME.getMutField(~a, '~a'), 'get'),[])", [expr-to-js(obj), field])
    |s_update(l, super, fields) => 
        format("~a.updateWith({~a})", [expr-to-js(super), fields.map(make-field-js).join-str(",\n")])
    | else => do-block(format("throw new Error('Not yet implemented ~a')", [torepr(ast)]))
  end
end

