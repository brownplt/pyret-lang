#lang pyret

import ast as A
import json as J
import format as format

provide *

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
    format("RUNTIME.ids[\"~a\"]" ,[js-id-of(id)])
end

fun program-to-js(ast, runtime-ids):
  cases(A.Program) ast:
    # import/provide ignored
    | s_program(_, _, block) =>
      bindings = for list.fold(bs from "", id from runtime-ids):
        bs + format("~a = RUNTIME['~a'];\n", [id-access(id), id]) #Was var ~a
      end
      format("(function(RUNTIME) {
        try {
          ~a
          return RUNTIME.makeNormalResult(~a);
        } catch(e) {
          return RUNTIME.makeFailResult(e);
        }
       })", [bindings, expr-to-js(block)])
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
        format("(function(){\n ~a \n})()", [sequence-return-last(stmts)])
      end
    | s_num(_, n) =>
      format("RUNTIME.makeNumber(~a)", [n])
    | s_str(_, s) =>
      format("RUNTIME.makeString(\"~a\")", [s])
    | s_bool(_, b) =>
      format("RUNTIME.makeBoolean(~a)", [b])
    | s_lam(_, _, args, _, _, body, _) =>
      format("RUNTIME.makeFunction(function(~a) {~a \nreturn ~a; })", [args.map(fun(b): js-id-of(b.id);).join-str(","),args.map(fun(b): format("~a = ~a;\n", [id-access(b.id), js-id-of(b.id)]);).join-str(""),expr-to-js(body)])
    #Should check that body is a block
    | s_method(_, args, _, _, body, _) =>
      format("RUNTIME.makeMethod(function(~a) {~a \nreturn ~a; })", [args.map(fun(b): js-id-of(b.id);).join-str(","),args.map(fun(b): format("~a = ~a;\n", [id-access(b.id), js-id-of(b.id)]);).join-str(""),expr-to-js(body)])
    | s_app(_, f, args) =>
      format("RUNTIME.checkFun(~a).app(~a)", [expr-to-js(f), args.map(expr-to-js).join-str(",")])
    | s_bracket(_, obj, f) =>
      cases (A.Expr) f:
        | s_str(_, s) => format("RUNTIME.getField(~a, '~a')", [expr-to-js(obj), s])
        | else => raise("Non-string lookups not supported")
      end
    | s_id(_, id) => id-access(id)
    | s_var(_, bind, value) =>
      js_id = id-access(bind.id)
      format("~a = ~a", [js_id, expr-to-js(value)])
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
    | else => do-block(format("throw new Error('Not yet implemented ~a')", [torepr(ast)]))
  end
end

