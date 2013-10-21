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
#    format("RUNTIME.ids[\"~a\"]" ,[js-id-of(id)])
js-id-of(id)
end

fun program-to-js(ast, runtime-ids):
  cases(A.Program) ast:
    # import/provide ignored
    | s_program(_, _, block) =>
      bindings = for list.fold(bs from "", id from runtime-ids):
        bs + format("var ~a = RUNTIME['~a'];\n", [id-access(id), id]) #Was var ~a
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
    | s_user_block(s, expr) => expr-to-js(expr)
    | s_num(_, n) =>
      format("RUNTIME.makeNumber(~a)", [n])
    | s_str(_, s) =>
      format("RUNTIME.makeString(\"~a\")", [s])
    | s_bool(_, b) =>
      format("RUNTIME.makeBoolean(~a)", [b])
    | s_lam(_, _, args, _, doc, body, _) =>
      #format("RUNTIME.makeFunction(function(~a) {~a \nreturn ~a; })", [args.map(fun(b): js-id-of(b.id);).join-str(","),args.map(fun(b): format("~a = ~a;\n", [id-access(b.id), js-id-of(b.id)]);).join-str(""),expr-to-js(body)])
      format("RUNTIME.makeFunction(function(~a) {return ~a; }, '~a')", [args.map(fun(b): js-id-of(b.id);).join-str(","),expr-to-js(body), doc])
    #Should check that body is a block
    | s_method(_, args, _, doc, body, _) =>
      #format("RUNTIME.makeMethod(function(~a) {~a \nreturn ~a; })", [args.map(fun(b): js-id-of(b.id);).join-str(","),args.map(fun(b): format("~a = ~a;\n", [id-access(b.id), js-id-of(b.id)]);).join-str(""),expr-to-js(body)])
      format("RUNTIME.makeMethod(function(~a) {return ~a; }, '~a')", [args.map(fun(b): js-id-of(b.id);).join-str(","),expr-to-js(body), doc])
    | s_app(_, f, args) =>
      format("RUNTIME.checkFun(~a).app(~a)", [expr-to-js(f), args.map(expr-to-js).join-str(",")])
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
    | s_if_else(_, branches,_else) =>
       all_but_else = cases(list.List) branches:
        | link(f, r) => format("if(RUNTIME.checkBool(~a)) {return ~a;}\n~a",[expr-to-js(f.test), expr-to-js(f.body), r.map(fun(x): format("else if(RUNTIME.checkBool(~a)) {return ~a;}",[expr-to-js(x.test), expr-to-js(x.body)]);).join-str("\n")])
        end
        ifblock = format("~a else{return ~a;}", [all_but_else, expr-to-js(_else)])
        format("(function() {~a})()",[ifblock])
    | s_try(_, body, bind, _except) =>
        format("(function() {\n try{\n  return ~a; \n} catch(~a) {\n return ~a; \n}})()", [expr-to-js(body), js-id-of(bind.id), expr-to-js(_except)])
    | s_get_bang(l, obj, field) => 
        format("RUNTIME.getField(RUNTIME.getMutField(~a, '~a'), 'get').app()", [expr-to-js(obj), field])
    |s_update(l, super, fields) => 
        format("~a.updateWith({~a})", [expr-to-js(super), fields.map(make-field-js).join-str(",\n")])
    | else => do-block(format("throw new Error('Not yet implemented ~a')", [torepr(ast)]))
  end
end

