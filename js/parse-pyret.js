define(["./pyret-tokenizer", "./pyret-parser", "./ast", "fs"], function(T, G, ast, fs) {
  return function(RUNTIME, NAMESPACE) {
    //var data = "#lang pyret\n\nif (f(x) and g(y) and h(z) and i(w) and j(u)): true else: false end";
    function translate(node, fileName) {
      // NOTE: This translation could blow the stack for very deep ASTs
      // We might have to rewrite the whole algorithm
      // One possibility is to reuse a stack of {todo: [...], done: [...], doing: fn} nodes
      // where each AST kid that needs to be recursively processed pushes a new frame on the stack
      // (it can eagerly process any primitive values, and defer the rest),
      // and returns a function to be called when all the new todos are done (which gets put into doing)
      // if a todo item is a Pyret value, it just gets pushed across to done
      // if a todo item is an array, then doing = RUNTIME.makeList and it creates a stack frame
      function tr(node) {
        return translators[node.name](node);
      }
      function pos(p) {
        return RUNTIME.makeSrcLoc(fileName, 
                                  p.startRow, p.startCol, p.startChar,
                                  p.endRow, p.endCol, p.endChar);
      }
      function name(tok) { return RUNTIME.makeString(tok.value); }
      function string(tok) { return RUNTIME.makeString(tok.value); } // I'm pretty sure the tokenizer strips off quotes
      function number(tok, positive) { 
        if (positive)
          return RUNTIME.makeNumberFromString(tok.value); 
        else
          return RUNTIME.makeNumberFromString("-" + tok.value);
      }
      const translators = {
        'program': function(node) {
          return RUNTIME.getField(ast, 's_prog')
            .app(pos(node.pos), 
                 RUNTIME.makeList(node.kids[0].map(tr)), // TODO: fix when we split import from provide
                 tr(node.kids[1]));
        },
        'provide-stmt': function(node) {
          if (node.kids.length === 2) {
            // (provide-stmt PROVIDE STAR)
            return RUNTIME.getField(ast, 's_provide_all')
              .app(pos(node.pos));
          } else {
            // (provide-stmt PROVIDE stmt END
            return RUNTIME.getField(ast, 's_provide')
              .app(pos(node.pos), tr(node.kids[1]))
          }
        },
        'import-stmt': function(node) {
          // (import-stmt IMPORT mod AS NAME)
          return RUNTIME.getField(ast, 's_import')
            .app(pos(node.pos), tr(node.kids[1]), name(node.kids[3]));
        },
        'import-name': function(node) {
          // (import-name NAME)
          return RUNTIME.getField(ast, 'import_name')
            .app(pos(node.pos), name(node.kids[0]))
        },
        'import-string': function(node) {
          // (import-string STRING)
          return RUNTIME.getField(ast, 'import_string')
            .app(pos(node.pos), string(node.kids[0]))
        },
        'block': function(node) {
          // (block stmts ...)
          return RUNTIME.getField(ast, 's_block')
            .app(pos(node.pos), RUNTIME.makeList(node.kids.map(tr)));
        },
        'stmt': function(node) {
          // (stmt s)
          return tr(node.kids[0]);
        },
        'data-with': function(node) {
          if (node.kids.length === 0) {
            // (data-with)
            return RUNTIME.makeList([]);
          } else {
            // (data-with WITH fields)
            return RUNTIME.makeList(node.kids.map(tr));
          }
        },
        'data-variant': function(node) {
          if (kids.length === 4) {
            // (data-variant PIPE NAME args with)
            return RUNTIME.getField(ast, 's_variant')
              .app(pos(node.pos), name(node.kids[1]), tr(node.kids[2]), tr(node.kids[3]));
          } else {
            // (data-variant PIPE NAME with)
            return RUNTIME.getField(ast, 's_singleton_variant')
              .app(pos(node.pos), name(node.kids[1]), tr(node.kids[2]));
          }
        },
        'first-data-variant': function(node) {
          if (kids.length === 3) {
            // (first-data-variant NAME args with)
            return RUNTIME.getField(ast, 's_variant')
              .app(pos(node.pos), name(node.kids[0]), tr(node.kids[1]), tr(node.kids[2]));
          } else {
            // (first-data-variant NAME with)
            return RUNTIME.getField(ast, 's_singleton_variant')
              .app(pos(node.pos), name(node.kids[0]), tr(node.kids[1]));
          }
        },
        'datatype-variant': function(node) {
          if (kids.length === 4) {
            // (datatype-variant PIPE NAME args constructor)
            return RUNTIME.getField(ast, 's_datatype_variant')
              .app(pos(node.pos), name(node.kids[1]), tr(node.kids[2]), tr(node.kids[3]));
          } else {
            // (datatype-variant PIPE NAME constructor)
            return RUNTIME.getField(ast, 's_datatype_singleton_variant')
              .app(pos(node.pos), name(node.kids[1]), tr(node.kids[2]));
          }
        },
        'first-datatype-variant': function(node) {
          if (kids.length === 4) {
            // (first-datatype-variant NAME args constructor)
            return RUNTIME.getField(ast, 's_datatype_variant')
              .app(pos(node.pos), name(node.kids[0]), tr(node.kids[1]), tr(node.kids[2]));
          } else {
            // (first-datatype-variant NAME constructor)
            return RUNTIME.getField(ast, 's_datatype_singleton_variant')
              .app(pos(node.pos), name(node.kids[0]), tr(node.kids[0]));
          }
        },
        'data-sharing': function(node) {
          if (node.kids.length === 2) {
            // (data-sharing SHARING fields)
            return RUNTIME.makeList(node.kids.slice(1).map(tr));
          } else {
            // (data-sharing)
            return RUNTIME.makeList([]);
          }
        },
        'constructor-clause': function(node) {
          // (constructor-clause WITHCONSTRUCTOR LPAREN NAME RPAREN COLON block END)
          return RUNTIME.getField(ast, 's_datatype_constructor')
            .app(pos(node.pos), name(node.kids[2]), tr(node.kids[5]));
        },
        'var-expr': function(node) {
          // (var-expr VAR bind EQUALS e)
          return RUNTIME.getField(ast, 's_var')
            .app(pos(node.pos), tr(node.kids[1]), tr(node.kids[3]));
        },
        'let-expr': function(node) {
          // (let-expr VAR bind EQUALS e)
          return RUNTIME.getField(ast, 's_let')
            .app(pos(node.pos), tr(node.kids[1]), tr(node.kids[3]));
        },
        'graph-expr': function(node) {
          // (graph-expr GRAPH bind ... END)
          return RUNTIME.getField(ast, 's_graph')
            .app(pos(node.pos), RUNTIME.makeList(node.kids.slice(1, -1).map(tr)));
        },
        'fun-expr': function(node) {
          // (fun-expr FUN (fun-header params fun-name args return) COLON doc body check END)
          return RUNTIME.getField(ast, 's_fun')
            .app(pos(node.pos), name(node.kids[1].kids[1]),
                 RUNTIME.makeList(node.kids[1].kids[0].map(tr)),
                 RUNTIME.makeList(node.kids[1].kids[2].map(tr)),
                 tr(node.kids[1].kids[3]),
                 tr(node.kids[3]),
                 tr(node.kids[4]),
                 tr(node.kids[5]));
        },
        'data-expr': function(node) {
          // (data-expr DATA NAME params mixins COLON variant ... sharing-part check END)
          return RUNTIME.getField(ast, 's_data')
            .app(pos(node.pos), name(node.kids[1]), tr(node.kids[2]), tr(node.kids[3]),
                 RUNTIME.makeList(node.kids.slice(5, -3).map(tr)), 
                 tr(node.kids[node.kids.length - 3]),
                 tr(node.kids[node.kids.length - 2]));
        },
        'datatype-expr': function(node) {
          // (datatype-expr DATATYPE NAME params COLON variant ... check END)
          return RUNTIME.getField(ast, 's_datatype')
            .app(pos(node.pos), name(node.kids[1]), tr(node.kids[2]),
                 RUNTIME.makeList(node.kids.slice(4, -2).map(tr)),
                 tr(node.kids[node.kids.length - 2]));
        },
        'assign-expr': function(node) {
          // (assign-expr id COLONEQUAL e)
          return RUNTIME.getField(ast, 's_assign')
            .app(pos(node.pos), name(node.kids[0]), tr(node.kids[2]));
        },
        'when-expr': function(node) {
          // (when-expr WHEN test COLON body END)
          return RUNTIME.getField(ast, 's_when')
            .app(pos(node.pos), tr(node.kids[1]), tr(node.kids[3]));
        },
        'check-expr': function(node) {
          // (check-expr CHECK body END)
          return RUNTIME.getField(ast, 's_check')
            .app(pos(node.pos), tr(node.kids[1]));
        },
        'check-test': function(node) {
          if (node.kids.length === 1) {
            // (check-test e)
            return tr(node.kids[0]);
          } else {
            // (check-test left op right)
            return RUNTIME.getField(ast, 's_check_test')
              .app(pos(node.pos), tr(node.kids[1]), tr(node.kids[0]), tr(node.kids[2])); // Op comes first
          }
        },
        'binop-expr': function(node) {
          if (node.kids.length === 1) {
            // (binop-expr e)
            return tr(node.kids[0]);
          } else {
            return RUNTIME.getField(ast, 's_op')
              .app(pos(node.pos), tr(node.kids[1]), tr(node.kids[0]), tr(node.kids[2])); // Op comes first
          }
        },
        'doc-string': function(node) {
          if (node.kids.length === 0) {
            // (doc-string)
            return RUNTIME.makeString("");
          } else {
            // (doc-string DOC str)
            return string(node.kids[1]);
          }
        },
        'where-clause': function(node) {
          if (node.kids.length === 0) {
            // (where-clause)
            return RUNTIME.getField(ast, 's_block')
              .app(pos(node.pos), RUNTIME.makeList([]));
          } else {
            // (where-clause WHERE block)
            return tr(node.kids[1]);
          }
        },
        'check-op': function(node) {
          // (check-op str)
          return opLookup[string(node.kids[0])];
        },
        'expr': function(node) {
          // (expr e)
          return tr(node.kids[0]);
        },
        'not-expr': function(node) {
          // (not-expr NOT e)
          return RUNTIME.getField(ast, 's_not')
            .app(pos(node.pos), tr(node.kids[1]));
        },
        'binop-expr-paren': function(node) {
          if (node.kids[0].name === "paren-nospace-expr") {
            // (binop-expr-paren (paren-nospace-expr _ e _))
            return RUNTIME.getField(ast, 's_paren')
              .app(pos(node.pos), tr(node.kids[0].kids[1]));
          } else {
            // (binop-expr-paren e)
            return tr(node.kids[0]);
          }
        },
        'binop': function(node) {
          // (binop str)
          return opLookup[string(node.kids[0])];
        },
        'return-ann': function(node) {
          if (node.kids.length === 0) {
            // (return-ann)
            return RUNTIME.getField(ast, 'a_blank').app();
          } else {
            // (return-ann THINARROW ann)
            return tr(node.kids[1]);
          }
        },
        'binding': function(node) {
          if (node.kids.length === 1) {
            // (binding name)
            return RUNTIME.getField(ast, 's_bind')
              .app(pos(node.pos), RUNTIME.pyretFalse, name(node.kids[0]), 
                   RUNTIME.getField(ast, 'a_blank').app());
          } else if (node.kids.length === 3) {
            // (binding name COLONCOLON ann)
            return RUNTIME.getField(ast, 's_bind')
              .app(pos(node.pos), RUNTIME.pyretFalse, name(node.kids[0]), tr(node.kids[2]));
          } else if (node.kids.length === 2) {
            // (binding SHADOW name)
            return RUNTIME.getField(ast, 's_bind')
              .app(pos(node.pos), RUNTIME.pyretTrue, name(node.kids[1]), 
                   RUNTIME.getField(ast, 'a_blank').app());
          } else {
            // (binding SHADOW name COLONCOLON ann)
            return RUNTIME.getField(ast, 's_bind')
              .app(pos(node.pos), RUNTIME.pyretTrue, name(node.kids[1]), tr(node.kids[3]));
          }
        },
        'args': function(node) {
          if (node.kids.length === 2) {
            // (args LPAREN RPAREN)
            return RUNTIME.makeList([]);
          } else {
            // (args LPAREN (list-arg-elt arg COMMA) ... lastarg RPAREN)
            return RUNTIME.makeList(node.kids.slice(1, -1).map(tr));
          }
        },
        'list-arg-elt': function(node) {
          // (list-arg-elt arg COMMA)
          return tr(node.kids[0]);
        },
        'variant-member': function(node) {
          if (node.kids.length === 1) {
            // (variant-member b)
            return RUNTIME.getField(ast, 's_variant_member')
              .app(pos(node.pos), RUNTIME.makeString("normal"), tr(node.kids[0]));
          } else {
            if (node.kids[0].name === "MUTABLE") {
              return RUNTIME.getField(ast, 's_variant_member')
                .app(pos(node.pos), RUNTIME.makeString("mutable"), tr(node.kids[0]));
            } else {
              return RUNTIME.getField(ast, 's_variant_member')
                .app(pos(node.pos), RUNTIME.makeString("cyclic"), tr(node.kids[0]));
            }
          }
        },
        'variant-members': function(node) {
          if (node.kids.length === 2) {
            // (variant-members LPAREN RPAREN)
            return RUNTIME.makeList([]);
          } else {
            // (variant-members LPAREN (list-variant-member mem COMMA) ... lastmem RPAREN)
            return RUNTIME.makeList(node.kids.slice(1, -1).map(tr));
          }          
        },
        'list-variant-member': function(node) {
          // (list-variant-member mem COMMA)
          return tr(node.kids[0]);
        },
        'key': function(node) {
          if (node.kids.length === 3) {
            // (key LBRACK e RBRACK)
            return tr(node.kids[1]);
          } else {
            // (key name)
            return RUNTIME.getField(ast, 's_str')
              .app(pos(node.pos), name(node.kids[0]));
          }
        },
        'obj-field': function(node) {
          if (node.kids.length === 4) {
            // (obj-field MUTABLE key COLON value)
            return RUNTIME.getField(ast, 's_mutable_field')
              .app(pos(node.pos), tr(node.kids[1]), RUNTIME.getField(ast, 'a_blank').app(), tr(node.kids[3]));
          } else if (node.kids.length === 6) {
            // (obj-field MUTABLE key COLONCOLON ann COLON value)
            return RUNTIME.getField(ast, 's_mutable_field')
              .app(pos(node.pos), tr(node.kids[1]), tr(node.kids[2]), tr(node.kids[3]));
          } else if (node.kids.length === 3) {
            // (obj-field key COLON value)
            return RUNTIME.getField(ast, 's_data_field')
              .app(pos(node.pos), tr(node.kids[0]), tr(node.kids[2]));
          } else {
            // (obj-field key args ret COLON doc body check END)
            return RUNTIME.getField(ast, 's_method_field')
              .app(pos(node.pos), tr(node.kids[0]), tr(node.kids[1]), tr(node.kids[2]),
                   tr(node.kids[4]), tr(node.kids[5]), tr(node.kids[6]));
          }
        },
        'obj-fields': function(node) {
          if (node.kids[node.kids.length - 1] instanceof Token) {
            // (obj-fields (list-obj-field f1 COMMA) ... lastField COMMA)
            return RUNTIME.makeList(node.kids.slice(0, -1).map(tr));
          } else {
            // (fields (list-obj-field f1 COMMA) ... lastField)
            return RUNTIME.makeList(node.kids.map(tr));
          }
        },
        'list-obj-field': function(node) {
          // (list-obj-field f COMMA)
          return tr(node.kids[0]);
        },
        'field': function(node) {
          if (node.kids.length === 3) {
            // (field key COLON value)
            return RUNTIME.getField(ast, "s_data_field")
              .app(pos(node.pos), tr(node.kids[0]), tr(node.kids[2]));
          } else {
            // (field key args ret COLON doc body check END)
            return RUNTIME.getField(ast, "s_method_field")
              .app(pos(node.pos), tr(node.kids[0]), tr(node.kids[1]), tr(node.kids[2]),
                   tr(node.kids[4]), tr(node.kids[5]), tr(node.kids[6]));
          }
        },
        'fields': function(node) {
          if (node.kids[node.kids.length - 1] instanceof Token) {
            // (fields (list-field f1 COMMA) ... lastField COMMA)
            return RUNTIME.makeList(node.kids.slice(0, -1).map(tr));
          } else {
            // (fields (list-field f1 COMMA) ... lastField)
            return RUNTIME.makeList(node.kids.map(tr));
          }
        },
        'list-field': function(node) {
          // (list-field f COMMA)
          return tr(node.kids[0]);
        },
        'data-mixins': function(node) {
          if (node.kids.length === 0) {
            // (data-mixins)
            return RUNTIME.makeList([]);
          } else {
            // (data-mixins DERIVING mixins)
            return tr(node.kids[1]);
          }
        },
        'mixins': function(node) {
          // (mixins (list-mixin m COMMA) ... lastmixin)
          return RUNTIME.makeList(node.kids.map(tr));
        },
        'list-mixin': function(node) {
          // (list-mixin m COMMA)
          return tr(node.kids[0]);
        },
        'app-args': function(node) {
          if (node.kids.length === 2) {
            // (app-args LPAREN RPAREN)
            return RUNTIME.makeList([]);
          } else {
            // (app-args LPAREN (app-arg-elt e COMMA) ... elast RPAREN)
            return RUNTIME.makeList(node.kids.slice(1, -1).map(tr));
          }
        },
        'app-arg-elt': function(node) {
          // (app-arg-elt e COMMA)
          return tr(node.kids[0]);
        },
        'cases-branch': function(node) {
          if (node.kids.length === 4) {
            // (cases-branch PIPE NAME THICKARROW body)
            return RUNTIME.getField(ast, 's_cases_branch')
              .app(pos(node.pos), name(node.kids[1]), tr(node.kids[3]));
          } else {
            // (cases-branch PIPE NAME args THICKARROW body)
            return RUNTIME.getField(ast, 's_cases_branch')
              .app(pos(node.pos), name(node.kids[1]), tr(node.kids[2]), tr(node.kids[3]));
          }
        },
        'else-if': function(node) {
          // (else-if ELSEIF test COLON body)
          return RUNTIME.getField(ast, 's_if_branch')
            .app(pos(node.pos), tr(node.kids[1]), tr(node.kids[3]));
        },
        'else': function(node) {
          // (else ELSE body)
          return RUNTIME.getField(ast, 's_else')
            .app(pos(node.pos), tr(node.kids[1]));
        },
        'ty-params': function(node) {
          if (node.kids.length === 0) {
            // (ty-params)
            return RUNTIME.makeList([]);
          } else {
            // (ty-params LANGLE (list-ty-param p COMMA) ... last RANGLE)
            return RUNTIME.makeList(node.kids.slice(1, -1).map(tr));
          }
        },
        'list-ty-param': function(node) {
          // (list-ty-param p COMMA)
          return tr(node.kids[0]);
        },
        'left-app-fun-expr': function(node) {
          if (node.kids.length === 1) {
            // (left-app-fun-expr id)
            return tr(node.kids[0]);
          } else {
            // (left-app-fun-expr id PERIOD name)
            return RUNTIME.getField(ast, 's_dot')
              .app(pos(node.pos), tr(node.kids[0]), name(node.kids[2]));
          }
        },
        'for-bind': function(node) {
          // (for-bind name FROM e)
          return RUNTIME.getField(ast, 's_for_bind')
            .app(pos(node.pos), tr(node.kids[0]), tr(node.kids[2]));
        },
        'prim-expr': function(node) {
          // (prim-expr e)
          return tr(node.kids[0]);
        },
        'obj-expr': function(node) {
          if (node.kids.length === 2) {
            // (obj-expr LBRACE RBRACE)
            return RUNTIME.getField(ast, 's_obj')
              .app(pos(node.pos), RUNTIME.makeList([]));
          } else {
            // (obj-expr LBRACE obj-fields RBRACE)
            return RUNTIME.getField(ast, 's_obj')
              .app(pos(node.pos), tr(node.kids[1]));
          }
        },
        'list-expr': function(node) {
          if (node.kids.length === 2) {
            // (list-expr LBRACK RBRACK)
            return RUNTIME.getField(ast, 's_list')
              .app(pos(node.pos), RUNTIME.makeList([]));
          } else {
            // (list-expr LBRACK list-fields RBRACK)
            return RUNTIME.getField(ast, 's_list')
              .app(pos(node.pos), tr(node.kids[1]));
          }
        },
        'app-expr': function(node) {
          // (app-expr f args)
          return RUNTIME.getField(ast, 's_app')
            .app(pos(node.pos), tr(node.kids[0]), tr(node.kids[1]));
        },
        'id-expr': function(node) {
          // (id-expr x)
          return RUNTIME.getField(ast, 's_id')
            .app(pos(node.pos), name(node.kids[0]));
        },
        'dot-expr': function(node) {
          // (dot-expr obj PERIOD field)
          return RUNTIME.getField(ast, 's_dot')
            .app(pos(node.pos), tr(node.kids[0]), name(node.kids[2]));
        },
        'get-bang-expr': function(node) {
          // (get-bang-expr obj BANG field)
          return RUNTIME.getField(ast, 's_get_bang')
            .app(pos(node.pos), tr(node.kids[0]), name(node.kids[2]));
        },
        'bracket-expr': function(node) {
          // (bracket-expr obj PERIOD LBRACK field RBRACK)
          return RUNTIME.getField(ast, 's_bracket')
            .app(pos(node.pos), tr(node.kids[0]), tr(node.kids[3]));
        },
        'colon-expr': function(node) {
          // (colon-expr obj COLON field)
          return RUNTIME.getField(ast, 's_colon')
            .app(pos(node.pos), tr(node.kids[0]), name(node.kids[2]));
        },
        'colon-bracket-expr': function(node) {
          // (colon-bracket-expr obj COLON LBRACK field RBRACK)
          return RUNTIME.getField(ast, 's_colon')
            .app(pos(node.pos), tr(node.kids[0]), name(node.kids[3]));
        },
        'cases-expr': function(node) {
          if (node.kids[node.kids.length - 4].name === "ELSE") {
            // (cases-expr CASES LPAREN type RPAREN val COLON branch ... PIPE ELSE THICKARROW elseblock END)
            return RUNTIME.getField(ast, 's_cases_else')
              .app(pos(node.pos), tr(node.kids[2]), tr(node.kids[4]),
                   RUNTIME.makeList(node.kids.slice(6, -5).map(tr)), tr(node.kids[node.kids.length - 2]));
          } else {
            // (cases-expr CASES LPAREN type RPAREN val COLON branch ... END)
            return RUNTIME.getField(ast, 's_cases')
              .app(pos(node.pos), tr(node.kids[2]), tr(node.kids[4]),
                   RUNTIME.makeList(node.kids.slice(6, -1).map(tr)));
          }
        },
        'if-expr': function(node) {
          if (node.kids[node.kids.length - 3].name === "ELSE") {
            // (if-expr IF test COLON body branch ... ELSE else END)
            return RUNTIME.getField(ast, 's_if_else')
              .app(pos(node.pos), RUNTIME.makeList(
                [RUNTIME.getField(ast, 's_if_branch')
                 .app(pos(node.kids[1].pos), tr(node.kids[1]), tr(node.kids[3]))]
                  .concat(node.kids.slice(4, -3).map(tr))),
                   tr(node.kids[node.kids.length - 2]));
          } else {
            // (if-expr IF test COLON body branch ... END)
            return RUNTIME.getField(ast, 's_if')
              .app(pos(node.pos), RUNTIME.makeList(
                [RUNTIME.getField(ast, 's_if_branch')
                 .app(pos(node.kids[1].pos), tr(node.kids[1]), tr(node.kids[3]))]
                  .concat(node.kids.slice(4, -3).map(tr))));
          }
        },
        'for-expr': function(node) {
          // (for-expr FOR iter LPAREN binds ... RPAREN return COLON body END)
          return RUNTIME.getField(ast, 's_for')
            .app(pos(node.pos), tr(node.kids[1]), RUNTIME.makeList(node.kids.slice(3, -5).map(tr)),
                 tr(node.kids[node.kids.length - 4]), tr(node.kids[node.kids.length - 2]));
        },
        'for-bind-elt': function(node) {
          // (for-bind-elt b COMMA)
          return tr(node.kids[0]);
        },
        'try-expr': function(node) {
          // (try-expr TRY body EXCEPT LPAREN arg RPAREN COLON except END)
          return RUNTIME.getField(ast, 's_try')
            .app(pos(node.pos), tr(node.kids[1]), tr(node.kids[4]), tr(node.kids[7]));
        },
        'user-block-expr': function(node) {
          // (user-block-expr BLOCK body END)
          return RUNTIME.getField(ast, 's_user_block')
            .app(pos(node.pos), tr(node.kids[1]));
        },
        'lambda-expr': function(node) {
          if (node.kids.length === 9) {
            // (lambda-expr FUN ty-params args return-ann COLON doc body check END)
            return RUNTIME.getField(ast, 's_lam')
              .app(pos(node.pos), tr(node.kids[1]), tr(node.kids[2]), tr(node.kids[3]),
                   tr(node.kids[5]), tr(node.kids[6]), tr(node.kids[7]));
          } else {
            // (lambda-expr FUN ty-params return-ann COLON doc body check END)
            return RUNTIME.getField(ast, 's_lam')
              .app(pos(node.pos), tr(node.kids[1]), RUNTIME.makeList([]), tr(node.kids[2]),
                   tr(node.kids[4]), tr(node.kids[5]), tr(node.kids[6]));
          }
        },
        'method-expr': function(node) {
          // (method-expr METHOD args return-ann COLON doc body check END)
          return RUNTIME.getField(ast, 's_method')
            .app(pos(node.pos), tr(node.kids[1]), tr(node.kids[2]),
                 tr(node.kids[4]), tr(node.kids[5]), tr(node.kids[6]));
        },
        'extend-expr': function(node) {
          // (extend-expr e PERIOD LBRACE fields RBRACE)
          return RUNTIME.getField(ast, 's_extend')
            .app(pos(node.pos), tr(node.kids[0]), tr(node.kids[3]));
        },
        'update-expr': function(node) {
          // (update-expr e BANG LBRACE fields RBRACE)
          return RUNTIME.getField(ast, 's_update')
            .app(pos(node.pos), tr(node.kids[0]), tr(node.kids[3]));
        },
        'left-app-expr': function(node) {
          // (left-app-expr e CARET f args)
          return RUNTIME.getField(ast, 's_left_app')
            .app(pos(node.pos), tr(node.kids[0]), tr(node.kids[2]), tr(node.kids[3]));
        },
        'paren-expr': function(node) {
          // (paren-expr LPAREN e RPAREN)
          return RUNTIME.getField(ast, 's_paren')
            .app(pos(node.pos), tr(node.kids[1]));
        },
        'paren-nospace-expr': function(node) {
          // (paren-nospace-expr LPAREN e RPAREN)
          return RUNTIME.getField(ast, 's_paren')
            .app(pos(node.pos), tr(node.kids[1]));
        },
        'inst-expr': function(node) {
          // (inst-expr e LANGLE (inst-elt a COMMA) ... alast RANGLE)
          return RUNTIME.getField(ast, 's_instantiate')
            .app(pos(node.pos), tr(node.kids[0]), RUNTIME.makeList(node.kids.slice(2, -1).map(tr)));
        },
        'inst-elt': function(node) {
          // (inst-elt a COMMA)
          return tr(node.kids[0]);
        },
        'bool-expr': function(node) {
          if (node.kids[0].name === "TRUE") {
            return RUNTIME.getField(ast, 's_bool')
              .app(pos(node.pos), RUNTIME.pyretTrue);
          } else {
            return RUNTIME.getField(ast, 's_bool')
              .app(pos(node.pos), RUNTIME.pyretFalse);
          }
        },
        'num-expr': function(node) {
          if (node.kids.length === 1) {
            // (num-expr n)
            return RUNTIME.getField(ast, 's_num')
              .app(pos(node.pos), number(node.kids[0], true));
          } else {
            // (num-expr MINUS n) {
            return RUNTIME.getField(ast, 's_num')
              .app(pos(node.pos), number(node.kids[0], false));
          }
        },
        'string-expr': function(node) {
          return RUNTIME.getField(ast, 's_str')
            .app(pos(node.pos), string(node.kids[0]));
        },
        'ann-field': function(node) {
          // (ann-field n COLON ann) or (ann-field n COLONCOLON ann)
          return RUNTIME.getField(ast, 'a_field')
            .app(pos(node.pos), name(node.kids[0]), tr(node.kids[2]));
        },
        'name-ann': function(node) {
          if (node.kids[0].value === "Any") {
            return RUNTIME.getField(ast, 'a_any').app();
          } else {
            return RUNTIME.getField(ast, 'a_name')
              .app(pos(node.pos), name(node.kids[0]));
          }
        },
        'record-ann': function(node) {
          // (record-ann LBRACE fields ... RBRACE)
          return RUNTIME.getField(ast, 'a_record')
            .app(pos(node.pos), RUNTIME.makeList(node.kids.slice(1, -1).map(tr)));
        },
        'arrow-ann': function(node) {
          // (arrow-ann LPAREN args ... THINARROW result RPAREN)
          return RUNTIME.getField(ast, 'a_arrow')
            .app(pos(node.pos), RUNTIME.makeList(node.kids.slice(1, -3).map(tr)));
        },
        'app-ann': function(node) {
          // (app-ann ann LANGLE args ... RANGLE)
          return RUNTIME.getField(ast, 'a_app')
            .app(pos(node.pos), tr(node.kids[0]), RUNTIME.makeList(node.kids.slice(2, -1).map(tr)));
        },
        'pred-ann': function(node) {
          // (pred-ann ann LPAREN exp RPAREN)
          return RUNTIME.getField(ast, 'a_pred')
            .app(pos(node.pos), tr(node.kids[0]), tr(node.kids[2]));
        },
        'dot-ann': function(node) {
          // (dot-ann n1 PERIOD n2)
          return RUNTIME.getField(ast, 'a_dot')
            .app(pos(node.pos), tr(node.kids[0]), tr(node.kids[2]));
        },
        'ann': function(node) {
          // (ann a)
          return tr(node.kids[0]);
        },
        'list-ann-field': function(node) {
          // (list-ann-field f COMMA)
          return tr(node.kids[0]);
        },
        'arrow-ann-elt': function(node) {
          // (arrow-ann-elt ann COMMA)
          return tr(node.kids[0]);
        },
        'app-ann-elt': function(node) {
          // (app-ann-elt ann COMMA)
          return tr(node.kids[0]);
        }
      };
    }

    const opLookup = {
      "+": RUNTIME.makeString("op+"),
      "-": RUNTIME.makeString("op-"),
      "*": RUNTIME.makeString("op*"),
      "/": RUNTIME.makeString("op/"),
      "<=": RUNTIME.makeString("op<="),
      "<": RUNTIME.makeString("op<"),
      ">=": RUNTIME.makeString("op>="),
      ">": RUNTIME.makeString("op>"),
      "==": RUNTIME.makeString("op=="),
      "<>": RUNTIME.makeString("op<>"),
      "and": RUNTIME.makeString("opand"),
      "or": RUNTIME.makeString("opor"),
      "is": RUNTIME.makeString("opis"),
      "raises": RUNTIME.makeString("opraises"),
      "satisfies": RUNTIME.makeString("opsatisfies"),
    }

    function parseDataRaw(data, fileName) {
      const toks = T.Tokenizer;
      toks.tokenizeFrom(data);
      // while (toks.hasNext())
      //   console.log(toks.next().toString(true));
      var parsed = G.PyretGrammar.parse(toks);
      console.log("Result:");
      var countParses = G.PyretGrammar.countAllParses(parsed);
      console.log("There are " + countParses + " potential parses");
      var posViolations = G.PyretGrammar.checkPositionContainment(parsed);
      if (posViolations) {
        console.log("Not all nodes conain their children!");
      } else {
        if (countParses === 1) {
          var ast = G.PyretGrammar.constructUniqueParse(parsed);
          console.log(ast.toString());
          return translate(ast, fileName);
        } else {
          var asts = G.PyretGrammar.constructAllParses(parsed);
          for (var i = 0; i < asts.length; i++) {
            console.log("Parse " + i + ": " + asts[i].toString());
            console.log(("" + asts[i]) === ("" + asts2[i]));
          }
          return translate(ast, fileName);
        }
      }
    }
    
    function parseFile(file) {
      RUNTIME.checkIf(file, RUNTIME.isString);
      var fileName = RUNTIME.unwrap(file);
      return parseDataRaw(fs.readFileSync(fileName, {encoding: "utf-8"}), fileName);
    }
    function parseData(data, fileName) {
      RUNTIME.checkIf(data, RUNTIME.isString);
      RUNTIME.checkIf(fileName, RUNTIME.isString);
      return parseDataRaw(RUNTIME.unwrap(data), RUNTIME.unwrap(fileName));
    }
    

    return RUNTIME.makeObject({
      provide: RUNTIME.makeObject({
        'parseFile': RUNTIME.makeFunction(parseFile),
        'parseData': RUNTIME.makeFunction(parseData)
      }),
      answer: NAMESPACE.get("nothing")
    });
  }
});



