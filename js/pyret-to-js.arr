#lang pyret 
import ast as A
import json as J
import format as format
import "count-nodes.arr" as C

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

fun block-ids(b :: A.is-s_block):
  cases(A.Expr) b:
    | s_block(_, stmts) => flatten(stmts.map(binding-ids))
    | else => raise("Non-block given to block-ids")
  end
end

fun toplevel-ids(program :: A.Program):
  cases(A.Program) program:
    | s_program(_, _, b) => block-ids(b)
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

fun program-to-cps-js(ast, runtime-ids):
  cases(A.Program) ast:
    # import/provide ignored
    | s_program(_, _, block) =>
      cases(A.Expr) block :
        | s_block(l, stmts) =>

          bindings = for list.fold(bs from "", id from ["nothing"] + runtime-ids):
            bs + format("var ~a = NAMESPACE.get('~a');\n", [js-id-of(id), id])
          end

          ids = block-ids(block)
          namespace-fields = ids.map(fun(id): A.s_data_field(l, A.s_str(l, id), A.s_id(l, id));)
          fun make-final-object(val):
            A.s_obj(l, [
                A.s_data_field(l, A.s_str(l, "value"), val),
                A.s_data_field(l, A.s_str(l, "namespace"), A.s_obj(l, namespace-fields))
              ])
          end
          stmts-for-cps = if stmts.length() == 0:
              [make-final-object(A.s_id(l, "nothing"))]
            else:
              fun sequence-wrap-last(ss):
                cases(list.List) ss:
                  | link(f, r) =>
                    cases(list.List) r:
                      | empty =>
                        if A.is-s_let(f) or A.is-s_var(f):
                          [f, make-final-object(A.s_id(l, "nothing"))]
                        else:
                          [make-final-object(f)]
                        end
                      | link(_, _) => [f] + sequence-wrap-last(r)
                    end
                end
              end
              sequence-wrap-last(stmts)
            end
          block-for-cps = A.s_block(l, stmts-for-cps)
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
              (function() {
                var k = RUNTIME.makeFunction(function(RESULT) {
                    var namespace = RUNTIME.getField(RESULT, 'namespace');
                    var value = RUNTIME.getField(RESULT, 'value');
                    // TODO(joe): Relying on the representation here to get off
                    // the ground, via the pyretToJSDict endpoint.  Need to codify
                    // namespaces and their interaction with runtime precisely
                    var EXPORT_NAMESPACE = Namespace(RUNTIME.pyretToJSDict(namespace));
                    $K.success(RUNTIME.makeNormalResult(value, EXPORT_NAMESPACE));
                  });
                var f = RUNTIME.makeFunction(function(ERR) {
                    $K.failure(RUNTIME.makeFailResult(ERR));
                  });
                (function() { return ~a})().app(k, f);
              })();
            } catch(e) {
              if(e instanceof RUNTIME.TrampolineException) {
                throw e;
              }
              $K.failure(RUNTIME.makeFailResult(e));
            }
          })", [bindings, expr-to-js(cps(block-for-cps))])

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

fun make-field-list-js(field):
    format("{name : '~a', value: ~a}", [field.name.s, expr-to-js(field.value)])
end

fun arg(l, name): A.s_bind(l, name, A.a_blank);
fun lam(l, args, body :: A.Expr): A.s_lam(l, [], args, A.a_blank, "anon lam", body, A.s_block(l, []));
app = A.s_app

K = "$k"
F = "$f"
fun mk-K(): gensym("$k");

fun cps(ast):
    #print("cpsing....")

  # punt can be used when you don't know how to CPS yet
  fun punt():
    print("Punting on: " + torepr(ast))
    l = ast.l
    lam(l, [arg(l, K), arg(l, F)], A.s_app(l, A.s_id(l, K), [ast]));
  id = A.s_id
  cases(A.Expr) ast:
    | s_user_block(l, block) => 
        cps(block)
    | s_block(l, pre-stmts) =>
      stmts = if pre-stmts.length() == 0: [A.s_id(l, "nothing")] else: pre-stmts;
      ids = block-ids(ast)
      vars = ids.map(fun(v): A.s_var(l, A.s_bind(l, v, A.a_blank), A.s_id(l, "nothing"));)

      cont = for fold(
            k from fun(e): A.s_app(l, cps(e), [A.s_id(l, K), A.s_id(l, F)]) end,
            s from stmts.take(stmts.length() - 1).reverse()):
        fun(e): A.s_app(l, cps(s), [lam(l, [arg(l, "ignored")], k(e)), A.s_id(l, F)]);;

      body = lam(l, [arg(l, K), arg(l, F)], cont(stmts.last()))

      A.s_block(l, vars + [body])

    #Primitives
    | s_num(l, _) =>
        lam(l, [arg(l, K), arg(l, F)], app(l, id(l, K), [ast]))
    | s_str(l, _) =>
        #app(l, id(l, K), [ast])
        lam(l, [arg(l, K), arg(l, F)], app(l, id(l, K), [ast]))
    | s_bool(l, _) =>
        #app(l, id(l, K), [ast])
        lam(l, [arg(l, K), arg(l, F)], app(l, id(l, K), [ast]))
   
    #Lambdas and Methods
    | s_lam(l, _, args, _, _, body, _) =>
        lam(l, [arg(l, K), arg(l, F)], app(l, id(l, K), 
            [lam(l, link(arg(l,"$dynK"), link(arg(l, "$dynF"), args)), app(l, cps(body), [id(l, "$dynK"), id(l, "$dynF")]))]))
    | s_method(l, args, a, b, body, d) =>
        #app(l, id(l,K), [lam(l, args, cps(body))])
        lam(l, [arg(l, K), arg(l, F)], app(l, id(l, K), 
            [A.s_method(l, link(arg(l,"$dynK"), link(arg(l, "$dynF"), args)), a, b, app(l, cps(body), [id(l, "$dynK"), id(l,"$dynF")]),d)]))
    | s_if_else(l, branches, _else) =>
        
        fun cps_branches(branchesToCps):
        cases(List) branchesToCps:
            | empty => 
                lam(l, [arg(l,K) , arg(l, F)], app(l, cps(_else), [id(l, K), id(l, F)]))
            | link(branch, rest) => 
                lam(l, [arg(l,K), arg(l, F)], app(l, cps(branch.test), 
                    [lam(l,  [arg(l, "$condVal")], 
                        A.s_if_else(l, [A.s_if_branch(l, id(l, "$condVal"), app(l, cps(branch.body), [id(l, K), id(l,F)]))],
                        app(l, cps_branches(rest), [id(l, K) , id(l, F)]))
                        ), id(l, F)])
                )
            end
        end
        cps_branches(branches)

    | s_obj(l, fields) =>
        fun makeFields(fds :: List<Members>, acc :: List<Members>):
            cases(List) fds:
                | empty => 
                    #lam(l, [arg(l, K), arg(l, F)] ,
                    app(l, id(l, K), [A.s_obj(l, acc.reverse())])
                | link(mem, rest) =>
                    #Treating mem like they have string names, not going to cps      
                    fname = gensym(mem.name.s) #Assuming string name
                    new_fields = link(A.s_data_field(l, mem.name, id(l, fname)), acc)

                    #print("mem: " + torepr(cps(mem.value)))
                   # lam(l, [arg(l, K)], 
                   # lam(l, [arg(l, K), arg(l,F)],
                   app(l, cps(mem.value), [lam(l, [arg(l, fname)], makeFields(rest, new_fields)), id(l, F)])
            end
        end
        lam(l, [arg(l, K), arg(l,F)],
        makeFields(fields, []))

    | s_extend(l, obj, fields) =>
        fun makeFields(fds :: List<Members>, acc :: List<Members>):
            cases(List) fds:
                | empty => 
                    #print(acc.reverse())
                    #lam(l, [arg(l, K), arg(l, F)] ,
                    app(l, id(l, K), [A.s_extend(l, id(l, '$ov'),acc.reverse())])
                | link(mem, rest) =>
                    #Treating mem like they have string names, not going to cps      
                    fname = gensym(mem.name.s) #Assuming string name
                    new_fields = link(A.s_data_field(l, mem.name, id(l, fname)), acc)

                    #print("mem: " + torepr(cps(mem.value)))
                   # lam(l, [arg(l, K)], 
                   # lam(l, [arg(l, K), arg(l,F)],
                    app(l, cps(mem.value), [lam(l, [arg(l, fname)], makeFields(rest, new_fields)), id(l, F)])
            end
        end

        lam(l, [arg(l, K), arg(l,F)],
            app(l, cps(obj), [lam(l, [arg(l, '$ov')], makeFields(fields, [])), id(l, F)]))

   | s_update(l, obj, fields) =>
         fun makeFields(fds :: List<Members>, acc :: List<Members>):
             cases(List) fds:
                 | empty => 
                     #print(acc.reverse())
                     #lam(l, [arg(l, K), arg(l, F)] ,
                     
                    # app(l, id(l, K), [A.s_update_cps(l, id(l, '$ov'),acc.reverse(), id(l, K), id(l,F))])
                    A.s_update_k(l, A.s_obj(l, [A.s_data_field(l, A.s_str(l,K), id(l, K)), A.s_data_field(l, A.s_str(l,F), id(l, F))]), id(l, '$ov'), acc.reverse())
                 | link(mem, rest) =>
                    #Treating mem like they have string names, not going to cps      
                    fname = gensym(mem.name.s) #Assuming string name
                    new_fields = link(A.s_data_field(l, mem.name, id(l, fname)), acc)

                    #print("mem: " + torepr(cps(mem.value)))
                   # lam(l, [arg(l, K)], 
                   # lam(l, [arg(l, K), arg(l,F)],
                    app(l, cps(mem.value), [lam(l, [arg(l, fname)], makeFields(rest, new_fields)), id(l, F)])
            end
        end


        lam(l, [arg(l, K), arg(l,F)],
            app(l, cps(obj), [lam(l, [arg(l, '$ov')], makeFields(fields, [])), id(l, F)]))
       

   #   # lam(l, [arg(l, K), arg(l,F)], 
   #   #    app(l, cps(obj), [lam(l, [arg(l, '$ov')], 
   #   #        for fold(
   #   #            acc from app(l, id(l, K), [A.s_update(l, id(l, '$ov'),acc.reverse())])
   #   #            field from fields)
   #   #                 
   #   #        end)]))
        
    # s_bracket(l, obj, f) =>
    #   #Not cps'ing f because we assume its a static string
    #  lam(l, [arg(l, K), arg(l, F)],
    #       app(l, cps(obj),
    #           [lam(l, [arg(l, '$ov')], 
    #               app(l, id(l, K), [A.s_bracket(l, id(l, '$ov'), f)])), id(l, F)]))

    | s_bracket(l, obj, f) =>
        #Not ps'ing f because we assume its a static string
       lam(l, [arg(l, K), arg(l, F)],
            app(l, cps(obj),
                [lam(l, [arg(l, '$ov')], 
                A.s_bracket_k(l, A.s_obj(l, [A.s_data_field(l, A.s_str(l,K), id(l, K)), A.s_data_field(l, A.s_str(l,F), id(l, F))]), 
                id(l, '$ov'), f)
                ), id(l, F)]))
    | s_colon_bracket(l, obj, f) =>
        #Not ps'ing f because we assume its a static string
       lam(l, [arg(l, K), arg(l, F)],
            app(l, cps(obj),
                [lam(l, [arg(l, '$ov')], 
                A.s_colon_bracket_k(l, A.s_obj(l, [A.s_data_field(l, A.s_str(l,K), id(l, K)), A.s_data_field(l, A.s_str(l,F), id(l, F))]), 
                id(l, '$ov'), f)
                ), id(l, F)]))

    | s_get_bang(l, obj, field) => 
        desugared = A.s_app(l, A.s_bracket(l, A.s_colon_bracket(l, obj, A.s_str(l, field)), A.s_str(l, "get")), [])
        cps(desugared)

    | s_app(l, f, es) =>
     # if es.length() == 1:
     #   lam(l, [arg(l, K)],
     #     app(l, cps(f), [lam(l, [arg(l, "$fv")],
     #       app(l, cps(es.first), [lam(l, [arg(l, "$argv")],
     #         app(l, id(l, "$fv"), [id(l, K), id(l, "$argv")]))]))]))
     # else:
     #   punt()
         fun makeArgChain(args, id-acc :: List<Expr>):
            cases(List) args:
                | empty =>  app(l, id(l, "$fv"), link(id(l,K) , link(id(l, F), id-acc.reverse())))
                | link(first, r) => 
                    varName = gensym("$var") 
                    app(l, cps(first), [lam(l, [arg(l, varName)], makeArgChain(r, link(id(l, varName), id-acc))), id(l, F)])
            end        
         end


         lam(l, [arg(l, K), arg(l, F)],
            app(l, cps(f), [lam(l, [arg(l, "$fv")], makeArgChain(es,[])), id(l, F)]))
    #end

    | s_try(l, body, bind, _except) => 
        lam(l, [arg(l, K), arg(l, F)], 
            app(l, cps(body), [id(l, K), 
                                lam(l, [arg(l, bind.id)], app(l, cps(_except), [id(l, K), id(l, F)]))]))
        
    | s_id(l, d) => 
         lam(l, [arg(l, K), arg(l, F)] , app(l, id(l, K), [ast]))
    | s_assign(l, b, e) =>
         lam(l, [arg(l, K), arg(l, F)] , app(l, cps(e), 
            [lam(l, [arg(l, '$v')], app(l, id(l, K), [A.s_assign(l, b, id(l, "$v"))] )), id(l,F)]))
    # Cheating a little here: Since we explicitly lift the block variables
    # above, we just turn lets and vars into assignment statements, and rely
    # on the well-formedness checking that's already happened
    | s_let(l, b, e) => cps(A.s_assign(l, b.id, e))
    | s_var(l, b, e) => cps(A.s_assign(l, b.id, e))
    | else => 
        print(ast)
        punt()
  end
end

fun expr-to-js(ast):
cases(A.Expr) ast:
    | s_block(_, stmts) =>
      if stmts.length() == 0:
        "NAMESPACE.get('nothing')" else:
        fun sequence-return-last(ss):
          cases(list.List) ss:
            | link(frst, r) =>
              cases(list.List) r:
                | empty => #format("return ~a;", [expr-to-js(frst)])
                  fun ends-in-bind(e):
                    format("~a;\nreturn NAMESPACE.get('nothing');", [expr-to-js(frst)])
                  end
                  cases(A.Expr) frst:
                    | s_let(_, _, _) => ends-in-bind(frst)
                    | s_var(_, _, _) => ends-in-bind(frst)
                    | else => format("return ~a;", [expr-to-js(frst)])
                  end
                | link(_, _) =>
                  format("~a;\n", [expr-to-js(frst)]) + sequence-return-last(r)
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
      #format("RUNTIME.makeFunction(function(~a) {\n~a \nreturn ~a; \n})", [args.map(fun(b): js-id-of(b.id);).join-str(","),args.map(fun(b): format("~a = ~a;\n", [id-access(b.id), js-id-of(b.id)]);).join-str(""),expr-to-js(body)])
      format("RUNTIME.makeFunction(function(~a) {\nreturn \t~a; \n}, ~s)", [args.map(fun(b): js-id-of(b.id);).join-str(","),expr-to-js(body), doc])
    #Should check that body is a block
    | s_method(_, args, _, doc, body, _) =>
      #format("RUNTIME.makeMethod(function(~a) {~a \nreturn ~a; })", [args.map(fun(b): js-id-of(b.id);).join-str(","),args.map(fun(b): format("~a = ~a;\n", [id-access(b.id), js-id-of(b.id)]);).join-str(""),expr-to-js(body)])
      format("RUNTIME.makeMethod(function(~a) {return ~a; }, ~s)", [args.map(fun(b): js-id-of(b.id);).join-str(","),expr-to-js(body), doc])
    | s_app(_, f, args) =>
      format("RUNTIME.applyFunction(~a, [~a])", [expr-to-js(f), args.map(expr-to-js).join-str(",")])
    | s_bracket(_, obj, f) =>
      format("RUNTIME.getField(~a, ~a)", [expr-to-js(obj), f.s])
    | s_colon_bracket(_, obj, f) =>
      format("RUNTIME.getColonField(~a, ~a)", [expr-to-js(obj), f.s])
    | s_colon_bracket_k(_, conts, obj, f) =>
      cases (A.Expr) f:
        | s_str(_, s) => format("RUNTIME.getColonFieldK(~a, '~a', ~a)", [expr-to-js(obj), s, expr-to-js(conts)])
        | else => raise("Non-string lookups not supported")
      end
    | s_bracket_k(_, conts, obj, f) =>
      cases (A.Expr) f:
        | s_str(_, s) => format("RUNTIME.getFieldK(~a, '~a', ~a)", [expr-to-js(obj), s, expr-to-js(conts)])
        | else => raise("Non-string lookups not supported")
      end
    | s_update_k(_, conts, obj, fields) =>
        format("~a.updateWithK(~a, [~a])", [expr-to-js(obj), expr-to-js(conts), fields.map(make-field-list-js).join-str(",\n")])

    | s_id(_, id) => id-access(id)
    | s_var(_, bind, value) =>
      js_id = id-access(bind.id)
      format("var ~a = ~a", [js_id, expr-to-js(value)])
    | s_assign(_, id, value) =>
      format("~a = ~a", [id-access(id), expr-to-js(value)])
    | s_let(_, bind, value) => 
      format("var ~a = ~a",[id-access(bind.id), expr-to-js(value)])
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
    | else => do-block(format("throw new Error('Not yet implemented ~a')", [ast.label()]))
  end
end


#Prints the type of the ast
fun print-type(ast): 
  cases(A.Expr) ast:
    | s_block(_, stmts) => print("block")
    | s_user_block(s, expr) => print("user-block")
    | s_num(_, n) => print("num")
    | s_str(_, s) => print("str")
    | s_bool(_, b) => print("bool")
    | s_lam(_, _, args, _, doc, body, _) => print("lam")
    | s_method(_, args, _, doc, body, _) => print("method")
    | s_app(_, f, args) => print("app")
    | s_bracket(_, obj, f) => print("bracket")
    | s_colon_bracket(_, obj, f) => print("colon")
    | s_colon_bracket_k(_, conts, obj, f) => print("colon_K")
    | s_bracket_k(_, conts, obj, f) => print("bracket_K")
    | s_update_k(_, conts, obj, fields) => print("update_K")
    | s_id(_, id) => print("id")
    | s_var(_, bind, value) => print("var")
    | s_assign(_, id, value) => print("assign")
    | s_let(_, bind, value) =>  print("let")
    | s_obj(_, fields) => print("obj")
    | s_extend(_, obj, fields) => print("extend")
    | s_if_else(_, branches,_else) => print("if")
    | s_try(_, body, bind, _except) => print("try")
    | s_get_bang(l, obj, field) => print("getBang")
    | s_update(l, super, fields) => print("update")
    | else => print("ELSE")
    end
end
