define(["js/runtime-util", "js/ffi-helpers", "trove/ast", "trove/srcloc", "js/dialects-lib"], function(util, ffi, astLib, srclocLib, dialectsLib) {
  return util.memoModule("parse-pyret", function(RUNTIME, NAMESPACE) {
    var F = ffi(RUNTIME, NAMESPACE);
    var srcloc = RUNTIME.getField(srclocLib(RUNTIME, NAMESPACE), "provide");
    var ast = RUNTIME.getField(astLib(RUNTIME, NAMESPACE), "provide");

    var dialects = dialectsLib(RUNTIME, NAMESPACE);
    
    //var data = "#lang pyret\n\nif (f(x) and g(y) and h(z) and i(w) and j(u)): true else: false end";
    function makePyretPos(fileName, p) {
      var n = RUNTIME.makeNumber;
      return RUNTIME.getField(srcloc, "srcloc").app(
          RUNTIME.makeString(fileName),
          n(p.startRow),
          n(p.startCol),
          n(p.startChar),
          n(p.endRow),
          n(p.endCol),
          n(p.endChar)
        );
    }
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
      var pos = function(p) { return makePyretPos(fileName, p); };
      var makeList = F.makeList;
      function name(tok) {
        if (tok.value === "_")
          return RUNTIME.getField(ast, 's-underscore').app(pos(tok.pos));
        else
          return RUNTIME.getField(ast, 's-name').app(pos(tok.pos), RUNTIME.makeString(tok.value));
      }
      function symbol(tok) {
        return RUNTIME.makeString(tok.value);
      }
      function string(tok) { 
        if (tok.value.substring(0, 3) === "```")
          return RUNTIME.makeString(tok.value.slice(3, -3).trim());
        else
          return RUNTIME.makeString(tok.value.slice(1, -1));
      }
      function number(tok) { return RUNTIME.makeNumberFromString(tok.value); }
      const translators = {
        'program': function(node) {
          var prelude = tr(node.kids[0]);
          var body = tr(node.kids[1]);
          return RUNTIME.getField(ast, 's-program')
            .app(pos(node.pos), prelude.provide, prelude.provideTypes, prelude.imports, body);
        },
        'prelude': function(node) {
          var provide;
          var provideTypes;
          var kids = node.kids.slice(0);
          if (kids.length > 0 && kids[0].name === "provide-stmt") {
            provide = tr(kids.shift());
          } else {
            provide = RUNTIME.getField(ast, 's-provide-none').app(pos(node.pos));
          }
          if (kids.length > 0 && kids[0].name === "provide-types-stmt") {
            provideTypes = tr(kids.shift());
          } else {
            provideTypes = RUNTIME.getField(ast, 's-provide-types-none').app(pos(node.pos));
          }
          return {
            provide : provide,
            provideTypes : provideTypes,
            imports : makeList(kids.map(tr))
          };
        },
        'provide-stmt': function(node) {
          if (node.kids.length === 2) {
            // (provide-stmt PROVIDE STAR)
            return RUNTIME.getField(ast, 's-provide-all')
              .app(pos(node.pos));
          } else {
            // (provide-stmt PROVIDE stmt END
            return RUNTIME.getField(ast, 's-provide')
              .app(pos(node.pos), tr(node.kids[1]))
          }
        },
        'provide-types-stmt': function(node) {
          if (node.kids[1].name === "STAR") {
            return RUNTIME.getField(ast, 's-provide-types-all').app(pos(node.pos));
          } else {
            // will produce record-ann
            var rec = tr(node.kids[1]);
            // Get the fields out of it
            return RUNTIME.getField(ast, 's-provide-types')
              .app(pos(node.pos), RUNTIME.getField(rec, 'fields'));
          }
        },
        'import-stmt': function(node) {
          if (node.kids[node.kids.length - 2].name === "AS") {
            if (node.kids.length == 4) {
              // (import-stmt IMPORT mod AS NAME)
              return RUNTIME.getField(ast, 's-import')
                .app(pos(node.pos), tr(node.kids[1]), name(node.kids[3]));
            } else {
              // (import-stmt IMPORT mod AS NAME, TYPES)
              return RUNTIME.getField(ast, 's-import-types')
                .app(pos(node.pos), tr(node.kids[1]), name(node.kids[3]), name(node.kids[5]));
            }
          } else {
            // (import-stmt IMPORT NAME (COMMA NAME)* FROM mod)
            var names = [];
            for (var i = 1; i < node.kids.length - 2; i += 2) {
              names.push(name(node.kids[i]));
            }
            return RUNTIME.getField(ast, 's-import-fields')
              .app(pos(node.pos), makeList(names), tr(node.kids[node.kids.length - 1]));
          }
        },
        'import-name': function(node) {
          // (import-name NAME)
          return RUNTIME.getField(ast, 's-const-import')
            .app(pos(node.pos), symbol(node.kids[0]))
        },
        'import-string': function(node) {
          // (import-string STRING)
          return RUNTIME.getField(ast, 's-file-import')
            .app(pos(node.pos), string(node.kids[0]))
        },
        'block': function(node) {
          // (block stmts ...)
          return RUNTIME.getField(ast, 's-block')
            .app(pos(node.pos), makeList(node.kids.map(tr)));
        },
        'stmt': function(node) {
          // (stmt s)
          return tr(node.kids[0]);
        },
        'data-with': function(node) {
          if (node.kids.length === 0) {
            // (data-with)
            return makeList([]);
          } else {
            // (data-with WITH fields)
            return tr(node.kids[1]);
          }
        },
        'variant-constructor': function(node) {
          // (variant-constructor NAME variant-members)
          return {
            pos: pos(node.pos),
            name: symbol(node.kids[0]),
            args: tr(node.kids[1])
          }
        },
        'data-variant': function(node) {
          if (node.kids[1].value !== undefined) {
            // (data-variant PIPE NAME with)
            return RUNTIME.getField(ast, 's-singleton-variant')
              .app(pos(node.pos), symbol(node.kids[1]), tr(node.kids[2]));
          } else {
            // (data-variant PIPE variant-constructor with)
            var constr = tr(node.kids[1])
            return RUNTIME.getField(ast, 's-variant')
              .app(pos(node.pos), constr.pos, constr.name, constr.args, tr(node.kids[2]));
          }
        },
        'first-data-variant': function(node) {
          if (node.kids[0].value !== undefined) {
            // (first-data-variant NAME with)
            return RUNTIME.getField(ast, 's-singleton-variant')
              .app(pos(node.pos), symbol(node.kids[0]), tr(node.kids[1]));
          } else {
            // (first-data-variant variant-constructor with)
            var constr = tr(node.kids[0])
            return RUNTIME.getField(ast, 's-variant')
              .app(pos(node.pos), constr.pos, constr.name, constr.args, tr(node.kids[1]));
          }
        },
        'datatype-variant': function(node) {
          if (node.kids[1].value !== undefined) {
            // (datatype-variant PIPE NAME constructor)
            return RUNTIME.getField(ast, 's-datatype-singleton-variant')
              .app(pos(node.pos), symbol(node.kids[1]), tr(node.kids[2]));
          } else {
            // (datatype-variant PIPE variant-constructor constructor)
            var constr = tr(node.kids[1])
            return RUNTIME.getField(ast, 's-datatype-variant')
              .app(pos(node.pos), constr.pos, constr.name, constr.args, tr(node.kids[2]));
          }
        },
        'first-datatype-variant': function(node) {
          if (node.kids[0].value !== undefined) {
            // (datatype-variant NAME constructor)
            return RUNTIME.getField(ast, 's-datatype-singleton-variant')
              .app(pos(node.pos), symbol(node.kids[0]), tr(node.kids[1]));
          } else {
            // (datatype-variant variant-constructor constructor)
            var constr = tr(node.kids[0])
            return RUNTIME.getField(ast, 's-datatype-variant')
              .app(pos(node.pos), constr.pos, constr.name, constr.args, tr(node.kids[1]));
          }
        },
        'data-sharing': function(node) {
          if (node.kids.length === 2) {
            // (data-sharing SHARING fields)
            return tr(node.kids[1]);
          } else {
            // (data-sharing)
            return makeList([]);
          }
        },
        'constructor-clause': function(node) {
          // (constructor-clause WITHCONSTRUCTOR LPAREN NAME RPAREN COLON block END)
          return RUNTIME.getField(ast, 's-datatype-constructor')
            .app(pos(node.pos), symbol(node.kids[2]), tr(node.kids[5]));
        },
        'type-expr': function(node) {
          return RUNTIME.getField(ast, 's-type')
            .app(pos(node.pos), name(node.kids[1]), tr(node.kids[3]));
        },
        'newtype-expr': function(node) {
          return RUNTIME.getField(ast, 's-newtype')
            .app(pos(node.pos), name(node.kids[1]), name(node.kids[3]));
        },
        'var-expr': function(node) {
          // (var-expr VAR bind EQUALS e)
          return RUNTIME.getField(ast, 's-var')
            .app(pos(node.pos), tr(node.kids[1]), tr(node.kids[3]));
        },
        'let-expr': function(node) {
          if (node.kids.length === 3) {
            // (let-expr bind EQUALS e)
            return RUNTIME.getField(ast, 's-let')
              .app(pos(node.pos), tr(node.kids[0]), tr(node.kids[2]), RUNTIME.makeBoolean(false));
          } else {
            // (let-expr VAL bind EQUALS e)
            return RUNTIME.getField(ast, 's-let')
              .app(pos(node.pos), tr(node.kids[1]), tr(node.kids[3]), RUNTIME.makeBoolean(true));
          }
        },
        'newtype-bind': function(node) {
          return RUNTIME.getField(ast, 's-newtype-bind')
            .app(pos(node.pos), name(node.kids[1]), name(node.kids[3]));
        },
        'type-bind': function(node) {
          return RUNTIME.getField(ast, 's-type-bind')
            .app(pos(node.pos), name(node.kids[0]), tr(node.kids[2]));
        },
        'type-let-bind': function(node) {
          return tr(node.kids[0]);
        },
        'type-let-bind-elt': function(node) {
          return tr(node.kids[0]);
        },
        'type-let-expr': function(node) {
          return RUNTIME.getField(ast, 's-type-let-expr')
            .app(pos(node.pos),
                 makeList(node.kids.slice(1, -3).map(tr)),
                 tr(node.kids[node.kids.length - 2]));
        },
        'multi-let-expr': function(node) {
          // (multi-let-expr LET let-binding-elt* let-binding COLON block END)
          // Note that we override the normal name dispatch here, because we don't want
          // to create the default let-expr or var-expr constructions
          return RUNTIME.getField(ast, 's-let-expr')
            .app(pos(node.pos), 
                 makeList(node.kids.slice(1, -3).map(translators["let-binding"])),
                 tr(node.kids[node.kids.length - 2]));
        },
        'letrec-expr': function(node) {
          // (letrec-expr LETREC letrec-binding* let-expr COLON block END)
          // Note that we override the normal name dispatch here, because we don't want
          // to create the default let-expr constructions
          return RUNTIME.getField(ast, 's-letrec')
            .app(pos(node.pos), 
                 makeList(node.kids.slice(1, -3).map(translators["letrec-binding"])), 
                 tr(node.kids[node.kids.length - 2]));
        },
        'let-binding': function(node) {
          if (node.name === "let-binding-elt") {
            // (let-binding-elt let-binding COMMA)
            node = node.kids[0];
          }
          if (node.name === "let-binding") {
            // (let-binding let-expr) or (let-binding var-expr)
            node = node.kids[0]
          }
          if (node.name === "let-expr") {
            // (let-expr binding EQUALS binop-expr)
            return RUNTIME.getField(ast, 's-let-bind')
              .app(pos(node.pos), tr(node.kids[0]), tr(node.kids[2]));
          } else if (node.name === "var-expr") {
            // (var-expr VAR binding EQUALS binop-expr)
            return RUNTIME.getField(ast, 's-var-bind')
              .app(pos(node.pos), tr(node.kids[1]), tr(node.kids[3]));
          }
        },
        'letrec-binding': function(node) {
          if (node.name === "letrec-binding") {
            node = node.kids[0];
          }
          // (let-expr binding EQUALS binop-expr)
          return RUNTIME.getField(ast, 's-letrec-bind')
            .app(pos(node.pos), tr(node.kids[0]), tr(node.kids[2]));
        },
        'contract-stmt': function(node) {
          // (contract-stmt NAME COLONCOLON ann)
          return RUNTIME.getField(ast, 's-contract')
            .app(pos(node.pos), name(node.kids[0]), tr(node.kids[2]));
        },
        'graph-expr': function(node) {
          // (graph-expr GRAPH bind ... END)
          return RUNTIME.getField(ast, 's-graph')
            .app(pos(node.pos), makeList(node.kids.slice(1, -1).map(tr)));
        },
        'fun-expr': function(node) {
          // (fun-expr FUN (fun-header params fun-name args return) COLON doc body check END)
          return RUNTIME.getField(ast, 's-fun')
            .app(pos(node.pos), symbol(node.kids[1].kids[1]),
                 tr(node.kids[1].kids[0]),
                 tr(node.kids[1].kids[2]),
                 tr(node.kids[1].kids[3]),
                 tr(node.kids[3]),
                 tr(node.kids[4]),
                 tr(node.kids[5]));
        },
        'data-expr': function(node) {
          // (data-expr DATA NAME params mixins COLON variant ... sharing-part check END)
          return RUNTIME.getField(ast, 's-data')
            .app(pos(node.pos), symbol(node.kids[1]), tr(node.kids[2]), tr(node.kids[3]),
                 makeList(node.kids.slice(5, -3).map(tr)), 
                 tr(node.kids[node.kids.length - 3]),
                 tr(node.kids[node.kids.length - 2]));
        },
        'datatype-expr': function(node) {
          // (datatype-expr DATATYPE NAME params COLON variant ... check END)
          return RUNTIME.getField(ast, 's-datatype')
            .app(pos(node.pos), symbol(node.kids[1]), tr(node.kids[2]),
                 makeList(node.kids.slice(4, -2).map(tr)),
                 tr(node.kids[node.kids.length - 2]));
        },
        'assign-expr': function(node) {
          // (assign-expr id COLONEQUAL e)
          return RUNTIME.getField(ast, 's-assign')
            .app(pos(node.pos), name(node.kids[0]), tr(node.kids[2]));
        },
        'when-expr': function(node) {
          // (when-expr WHEN test COLON body END)
          return RUNTIME.getField(ast, 's-when')
            .app(pos(node.pos), tr(node.kids[1]), tr(node.kids[3]));
        },
        'check-expr': function(node) {
          if (node.kids.length === 3) {
            // (check-expr CHECKCOLON body END)
            return RUNTIME.getField(ast, 's-check')
              .app(pos(node.pos), F.makeNone(), tr(node.kids[1]), 
                   RUNTIME.makeBoolean(node.kids[0].name === "CHECKCOLON"));
          } else {
            // (check-expr CHECK STRING COLON body END)
            return RUNTIME.getField(ast, 's-check')
              .app(pos(node.pos), F.makeSome(string(node.kids[1])), tr(node.kids[3]), 
                   RUNTIME.makeBoolean(node.kids[0].name === "CHECK"));
          }
        },
        'check-test': function(node) {
          if (node.kids.length === 1) {
            // (check-test e)
            return tr(node.kids[0]);
          } else {
            // (check-test left op right)
            return RUNTIME.getField(ast, 's-check-test')
              .app(pos(node.pos), tr(node.kids[1]), tr(node.kids[0]), tr(node.kids[2])); // Op comes first
          }
        },
        'binop-expr': function(node) {
          if (node.kids.length === 1) {
            // (binop-expr e)
            return tr(node.kids[0]);
          } else {
            var mkOp = RUNTIME.getField(ast, 's-op').app;
            var expr = mkOp(pos(node.pos), tr(node.kids[1]), tr(node.kids[0]), tr(node.kids[2]));
            for(var i = 4; i < node.kids.length; i += 2) {
              expr = mkOp(pos(node.pos), tr(node.kids[i - 1]), expr, tr(node.kids[i]));
            }
            return expr;
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
            return F.makeNone();
          } else {
            // (where-clause WHERE block)
            return F.makeSome(tr(node.kids[1]));
          }
        },
        'check-op': function(node) {
          // (check-op str)
          var opname = String(node.kids[0].value).trim();
          if(opLookup[opname]) {
            return opLookup[opname];
          }
          else {
            throw "Unknown operator: " + opname;
          }
        },
        'expr': function(node) {
          // (expr e)
          return tr(node.kids[0]);
        },
        'binop-expr-paren': function(node) {
          if (node.kids[0].name === "paren-nospace-expr") {
            // (binop-expr-paren (paren-nospace-expr _ e _))
            return RUNTIME.getField(ast, 's-paren')
              .app(pos(node.pos), tr(node.kids[0].kids[1]));
          } else {
            // (binop-expr-paren e)
            return tr(node.kids[0]);
          }
        },
        'binop': function(node) {
          // (binop str)
          var opname = String(node.kids[0].value).trim();
          if(opLookup[opname]) {
            return opLookup[opname];
          }
          else {
            throw "Unknown operator: " + opname;
          }
        },
        'return-ann': function(node) {
          if (node.kids.length === 0) {
            // (return-ann)
            return RUNTIME.getField(ast, 'a-blank');
          } else {
            // (return-ann THINARROW ann)
            return tr(node.kids[1]);
          }
        },
        'binding': function(node) {
          if (node.kids.length === 1) {
            // (binding name)
            return RUNTIME.getField(ast, 's-bind')
              .app(pos(node.pos), RUNTIME.pyretFalse, name(node.kids[0]), 
                   RUNTIME.getField(ast, 'a-blank'));
          } else if (node.kids.length === 3) {
            // (binding name COLONCOLON ann)
            return RUNTIME.getField(ast, 's-bind')
              .app(pos(node.pos), RUNTIME.pyretFalse, name(node.kids[0]), tr(node.kids[2]));
          } else if (node.kids.length === 2) {
            // (binding SHADOW name)
            return RUNTIME.getField(ast, 's-bind')
              .app(pos(node.pos), RUNTIME.pyretTrue, name(node.kids[1]), 
                   RUNTIME.getField(ast, 'a-blank'));
          } else {
            // (binding SHADOW name COLONCOLON ann)
            return RUNTIME.getField(ast, 's-bind')
              .app(pos(node.pos), RUNTIME.pyretTrue, name(node.kids[1]), tr(node.kids[3]));
          }
        },
        'toplevel-binding': function(node) {
          if (node.kids.length === 1) {
            // is actually a binding
            return tr(node.kids[0]);
          } else if (node.kids.length === 4) {
            // (toplevel-binding SHADOW NAME COLONCOLON noparen-arrow-ann)
            return RUNTIME.getField(ast, 's-bind')
              .app(pos(node.pos), RUNTIME.pyretTrue, name(node.kids[1]), tr(node.kids[3]));
          } else {
            // (toplevel-binding NAME COLONCOLON noparen-arrow-ann)
            return RUNTIME.getField(ast, 's-bind')
              .app(pos(node.pos), RUNTIME.pyretFalse, name(node.kids[0]), tr(node.kids[2]));
          }
        },
        'args': function(node) {
          if (node.kids.length === 2) {
            // (args LPAREN RPAREN)
            return makeList([]);
          } else {
            // (args LPAREN (list-arg-elt arg COMMA) ... lastarg RPAREN)
            return makeList(node.kids.slice(1, -1).map(tr));
          }
        },
        'list-arg-elt': function(node) {
          // (list-arg-elt arg COMMA)
          return tr(node.kids[0]);
        },
        'variant-member': function(node) {
          if (node.kids.length === 1) {
            // (variant-member b)
            return RUNTIME.getField(ast, 's-variant-member')
              .app(pos(node.pos), RUNTIME.getField(ast, "s-normal"), tr(node.kids[0]));
          } else {
            if (node.kids[0].name === "MUTABLE") {
              return RUNTIME.getField(ast, 's-variant-member')
                .app(pos(node.pos), RUNTIME.getField(ast, "s-mutable"), tr(node.kids[1]));
            } else {
              return RUNTIME.getField(ast, 's-variant-member')
                .app(pos(node.pos), RUNTIME.getField(ast, "s-cyclic"), tr(node.kids[1]));
            }
          }
        },
        'variant-members': function(node) {
          if (node.kids.length === 2) {
            // (variant-members LPAREN RPAREN)
            return makeList([]);
          } else {
            // (variant-members LPAREN (list-variant-member mem COMMA) ... lastmem RPAREN)
            return makeList(node.kids.slice(1, -1).map(tr));
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
            return RUNTIME.getField(ast, 's-str')
              .app(pos(node.pos), symbol(node.kids[0]));
          }
        },
        'obj-field': function(node) {
          if (node.kids.length === 4) {
            // (obj-field MUTABLE key COLON value)
            return RUNTIME.getField(ast, 's-mutable-field')
              .app(pos(node.pos), tr(node.kids[1]), RUNTIME.getField(ast, 'a-blank'), tr(node.kids[3]));
          } else if (node.kids.length === 6) {
            // (obj-field MUTABLE key COLONCOLON ann COLON value)
            return RUNTIME.getField(ast, 's-mutable-field')
              .app(pos(node.pos), tr(node.kids[1]), tr(node.kids[2]), tr(node.kids[3]));
          } else if (node.kids.length === 3) {
            // (obj-field key COLON value)
            return RUNTIME.getField(ast, 's-data-field')
              .app(pos(node.pos), tr(node.kids[0]), tr(node.kids[2]));
          } else {
            // (obj-field key args ret COLON doc body check END)
            return RUNTIME.getField(ast, 's-method-field')
              .app(pos(node.pos), tr(node.kids[0]), tr(node.kids[1]), tr(node.kids[2]),
                   tr(node.kids[4]), tr(node.kids[5]), tr(node.kids[6]));
          }
        },
        'obj-fields': function(node) {
          if (node.kids[node.kids.length - 1].name !== "obj-field") {
            // (obj-fields (list-obj-field f1 COMMA) ... lastField COMMA)
            return makeList(node.kids.slice(0, -1).map(tr));
          } else {
            // (fields (list-obj-field f1 COMMA) ... lastField)
            return makeList(node.kids.map(tr));
          }
        },
        'list-obj-field': function(node) {
          // (list-obj-field f COMMA)
          return tr(node.kids[0]);
        },
        'field': function(node) {
          if (node.kids.length === 3) {
            // (field key COLON value)
            return RUNTIME.getField(ast, "s-data-field")
              .app(pos(node.pos), tr(node.kids[0]), tr(node.kids[2]));
          } else {
            // (field key args ret COLON doc body check END)
            return RUNTIME.getField(ast, "s-method-field")
              .app(pos(node.pos), tr(node.kids[0]), tr(node.kids[1]), tr(node.kids[2]),
                   tr(node.kids[4]), tr(node.kids[5]), tr(node.kids[6]));
          }
        },
        'fields': function(node) {
          if (node.kids[node.kids.length - 1].name !== "field") {
            // (fields (list-field f1 COMMA) ... lastField COMMA)
            return makeList(node.kids.slice(0, -1).map(tr));
          } else {
            // (fields (list-field f1 COMMA) ... lastField)
            return makeList(node.kids.map(tr));
          }
        },
        'list-field': function(node) {
          // (list-field f COMMA)
          return tr(node.kids[0]);
        },
        'data-mixins': function(node) {
          if (node.kids.length === 0) {
            // (data-mixins)
            return makeList([]);
          } else {
            // (data-mixins DERIVING mixins)
            return tr(node.kids[1]);
          }
        },
        'mixins': function(node) {
          // (mixins (list-mixin m COMMA) ... lastmixin)
          return makeList(node.kids.map(tr));
        },
        'list-mixin': function(node) {
          // (list-mixin m COMMA)
          return tr(node.kids[0]);
        },
        'app-args': function(node) {
          if (node.kids.length === 2) {
            // (app-args LPAREN RPAREN)
            return makeList([]);
          } else {
            // (app-args LPAREN (app-arg-elt e COMMA) ... elast RPAREN)
            return makeList(node.kids.slice(1, -1).map(tr));
          }
        },
        'app-arg-elt': function(node) {
          // (app-arg-elt e COMMA)
          return tr(node.kids[0]);
        },
        'cases-branch': function(node) {
          if (node.kids.length === 4) {
            // (cases-branch PIPE NAME THICKARROW body)
            return RUNTIME.getField(ast, 's-cases-branch')
              .app(pos(node.pos), symbol(node.kids[1]), makeList([]), tr(node.kids[3]));
          } else {
            // (cases-branch PIPE NAME args THICKARROW body)
            return RUNTIME.getField(ast, 's-cases-branch')
              .app(pos(node.pos), symbol(node.kids[1]), tr(node.kids[2]), tr(node.kids[4]));
          }
        },
        'if-pipe-branch': function(node) {
          // (if-pipe-branch BAR binop-expr THENCOLON block)
          return RUNTIME.getField(ast, 's-if-pipe-branch')
            .app(pos(node.pos), tr(node.kids[1]), tr(node.kids[3]));
        },
        'else-if': function(node) {
          // (else-if ELSEIF test COLON body)
          return RUNTIME.getField(ast, 's-if-branch')
            .app(pos(node.pos), tr(node.kids[1]), tr(node.kids[3]));
        },
        'else': function(node) {
          // (else ELSECOLON body)
          return RUNTIME.getField(ast, 's-else')
            .app(pos(node.pos), tr(node.kids[1]));
        },
        'ty-params': function(node) {
          if (node.kids.length === 0) {
            // (ty-params)
            return makeList([]);
          } else {
            // (ty-params LANGLE (list-ty-param p COMMA) ... last RANGLE)
            return makeList(node.kids.slice(1, -2).map(tr).concat([name(node.kids[node.kids.length - 2])]));
          }
        },
        'list-ty-param': function(node) {
          // (list-ty-param NAME COMMA)
          return name(node.kids[0]);
        },
        'for-bind': function(node) {
          // (for-bind name FROM e)
          return RUNTIME.getField(ast, 's-for-bind')
            .app(pos(node.pos), tr(node.kids[0]), tr(node.kids[2]));
        },
        'prim-expr': function(node) {
          // (prim-expr e)
          return tr(node.kids[0]);
        },
        'obj-expr': function(node) {
          if (node.kids.length === 2) {
            // (obj-expr LBRACE RBRACE)
            return RUNTIME.getField(ast, 's-obj')
              .app(pos(node.pos), makeList([]));
          } else {
            // (obj-expr LBRACE obj-fields RBRACE)
            return RUNTIME.getField(ast, 's-obj')
              .app(pos(node.pos), tr(node.kids[1]));
          }
        },
        'list-elt': function(node) {
          return tr(node.kids[0]);
        },
        'bless-expr': function(node) {
          return RUNTIME.getField(ast, 's-bless')
            .app(pos(node.pos), tr(node.kids[1]), name(node.kids[3]));
        },
        'confirm-expr': function(node) {
          return RUNTIME.getField(ast, 's-confirm')
            .app(pos(node.pos), tr(node.kids[1]), name(node.kids[3]));
        },
        'construct-expr': function(node) {
          return RUNTIME.getField(ast, 's-construct')
            .app(pos(node.pos), tr(node.kids[1]), tr(node.kids[2]), makeList(node.kids.slice(4, -1).map(tr)))
        },
        'construct-modifier': function(node) {
          if (node.kids.length === 0) {
            return RUNTIME.getField(ast, 's-construct-normal');
          } else if (node.kids.length === 1) {
            if (node.kids[0].name === "LAZY") {
              return RUNTIME.getField(ast, 's-construct-lazy');
            }
          }
        },
        'app-expr': function(node) {
          // (app-expr f args)
          return RUNTIME.getField(ast, 's-app')
            .app(pos(node.pos), tr(node.kids[0]), tr(node.kids[1]));
        },
        'id-expr': function(node) {
          // (id-expr x)
          return RUNTIME.getField(ast, 's-id')
            .app(pos(node.pos), name(node.kids[0]));
        },
        'dot-expr': function(node) {
          // (dot-expr obj PERIOD field)
          return RUNTIME.getField(ast, 's-dot')
            .app(pos(node.pos), tr(node.kids[0]), symbol(node.kids[2]));
        },
        'get-bang-expr': function(node) {
          // (get-bang-expr obj BANG field)
          return RUNTIME.getField(ast, 's-get-bang')
            .app(pos(node.pos), tr(node.kids[0]), symbol(node.kids[2]));
        },
        'bracket-expr': function(node) {
          // (bracket-expr obj PERIOD LBRACK field RBRACK)
          return RUNTIME.getField(ast, 's-bracket')
            .app(pos(node.pos), tr(node.kids[0]), tr(node.kids[3]));
        },
        'cases-expr': function(node) {
          if (node.kids[node.kids.length - 4].name === "ELSE") {
            // (cases-expr CASES LPAREN type RPAREN val COLON branch ... PIPE ELSE THICKARROW elseblock END)
            return RUNTIME.getField(ast, 's-cases-else')
              .app(pos(node.pos), tr(node.kids[2]), tr(node.kids[4]),
                   makeList(node.kids.slice(6, -5).map(tr)), tr(node.kids[node.kids.length - 2]));
          } else {
            // (cases-expr CASES LPAREN type RPAREN val COLON branch ... END)
            return RUNTIME.getField(ast, 's-cases')
              .app(pos(node.pos), tr(node.kids[2]), tr(node.kids[4]),
                   makeList(node.kids.slice(6, -1).map(tr)));
          }
        },
        'if-pipe-expr': function(node) {
          if (node.kids[node.kids.length - 3].name === "OTHERWISECOLON") {
            // (if-pipe-expr IFCOLON branch ... BAR OTHERWISECOLON else END)
            return RUNTIME.getField(ast, 's-if-pipe-else')
              .app(pos(node.pos), makeList(node.kids.slice(1, -4).map(tr)),
                   tr(node.kids[node.kids.length - 2]));
          } else {
            // (if-expr IFCOLON branch ... END)
            return RUNTIME.getField(ast, 's-if-pipe')
              .app(pos(node.pos), makeList(node.kids.slice(1, -1).map(tr)));
          }
        },
        'if-expr': function(node) {
          if (node.kids[node.kids.length - 3].name === "ELSECOLON") {
            // (if-expr IF test COLON body branch ... ELSECOLON else END)
            return RUNTIME.getField(ast, 's-if-else')
              .app(pos(node.pos), makeList(
                [RUNTIME.getField(ast, 's-if-branch')
                 .app(pos(node.kids[1].pos), tr(node.kids[1]), tr(node.kids[3]))]
                  .concat(node.kids.slice(4, -3).map(tr))),
                   tr(node.kids[node.kids.length - 2]));
          } else {
            // (if-expr IF test COLON body branch ... END)
            return RUNTIME.getField(ast, 's-if')
              .app(pos(node.pos), makeList(
                [RUNTIME.getField(ast, 's-if-branch')
                 .app(pos(node.kids[1].pos), tr(node.kids[1]), tr(node.kids[3]))]
                  .concat(node.kids.slice(4, -1).map(tr))));
          }
        },
        'for-expr': function(node) {
          // (for-expr FOR iter LPAREN binds ... RPAREN return COLON body END)
          return RUNTIME.getField(ast, 's-for')
            .app(pos(node.pos), tr(node.kids[1]), makeList(node.kids.slice(3, -5).map(tr)),
                 tr(node.kids[node.kids.length - 4]), tr(node.kids[node.kids.length - 2]));
        },
        'for-bind-elt': function(node) {
          // (for-bind-elt b COMMA)
          return tr(node.kids[0]);
        },
        'try-expr': function(node) {
          // (try-expr TRY body EXCEPT LPAREN arg RPAREN COLON except END)
          return RUNTIME.getField(ast, 's-try')
            .app(pos(node.pos), tr(node.kids[1]), tr(node.kids[4]), tr(node.kids[7]));
        },
        'user-block-expr': function(node) {
          // (user-block-expr BLOCK body END)
          return RUNTIME.getField(ast, 's-user-block')
            .app(pos(node.pos), tr(node.kids[1]));
        },
        'lambda-expr': function(node) {
          // (lambda-expr LAM ty-params args return-ann COLON doc body check END)
          return RUNTIME.getField(ast, 's-lam')
            .app(pos(node.pos), tr(node.kids[1]), tr(node.kids[2]), tr(node.kids[3]),
                 tr(node.kids[5]), tr(node.kids[6]), tr(node.kids[7]));
        },
        'method-expr': function(node) {
          // (method-expr METHOD args return-ann COLON doc body check END)
          return RUNTIME.getField(ast, 's-method')
            .app(pos(node.pos), tr(node.kids[1]), tr(node.kids[2]),
                 tr(node.kids[4]), tr(node.kids[5]), tr(node.kids[6]));
        },
        'extend-expr': function(node) {
          // (extend-expr e PERIOD LBRACE fields RBRACE)
          return RUNTIME.getField(ast, 's-extend')
            .app(pos(node.pos), tr(node.kids[0]), tr(node.kids[3]));
        },
        'update-expr': function(node) {
          // (update-expr e BANG LBRACE fields RBRACE)
          return RUNTIME.getField(ast, 's-update')
            .app(pos(node.pos), tr(node.kids[0]), tr(node.kids[3]));
        },
        'paren-expr': function(node) {
          // (paren-expr LPAREN e RPAREN)
          return RUNTIME.getField(ast, 's-paren')
            .app(pos(node.pos), tr(node.kids[1]));
        },
        'paren-nospace-expr': function(node) {
          // (paren-nospace-expr LPAREN e RPAREN)
          return RUNTIME.getField(ast, 's-paren')
            .app(pos(node.pos), tr(node.kids[1]));
        },
        'inst-expr': function(node) {
          // (inst-expr e LANGLE (inst-elt a COMMA) ... alast RANGLE)
          return RUNTIME.getField(ast, 's-instantiate')
            .app(pos(node.pos), tr(node.kids[0]), makeList(node.kids.slice(2, -1).map(tr)));
        },
        'inst-elt': function(node) {
          // (inst-elt a COMMA)
          return tr(node.kids[0]);
        },
        'bool-expr': function(node) {
          if (node.kids[0].name === "TRUE") {
            return RUNTIME.getField(ast, 's-bool')
              .app(pos(node.pos), RUNTIME.pyretTrue);
          } else {
            return RUNTIME.getField(ast, 's-bool')
              .app(pos(node.pos), RUNTIME.pyretFalse);
          }
        },
        'num-expr': function(node) {
          // (num-expr n)
          return RUNTIME.getField(ast, 's-num')
            .app(pos(node.pos), number(node.kids[0]));
        },
        'frac-expr': function(node) {
          // (frac-expr n)
          var numden = node.kids[0].value.split("/");
          return RUNTIME.getField(ast, 's-frac')
            .app(pos(node.pos), RUNTIME.makeNumberFromString(numden[0]), RUNTIME.makeNumberFromString(numden[1]));
        },
        'string-expr': function(node) {
          return RUNTIME.getField(ast, 's-str')
            .app(pos(node.pos), string(node.kids[0]));
        },
        'ann-field': function(node) {
          // (ann-field n COLON ann) or (ann-field n COLONCOLON ann)
          return RUNTIME.getField(ast, 'a-field')
            .app(pos(node.pos), symbol(node.kids[0]), tr(node.kids[2]));
        },
        'name-ann': function(node) {
          if (node.kids[0].value === "Any") {
            return RUNTIME.getField(ast, 'a-any');
          } else {
            return RUNTIME.getField(ast, 'a-name')
              .app(pos(node.pos), name(node.kids[0]));
          }
        },
        'record-ann': function(node) {
          // (record-ann LBRACE fields ... RBRACE)
          return RUNTIME.getField(ast, 'a-record')
            .app(pos(node.pos), makeList(node.kids.slice(1, -1).map(tr)));
        },
        'noparen-arrow-ann': function(node) {
          // (noparen-arrow-ann args ... THINARROW result)
          return RUNTIME.getField(ast, 'a-arrow')
            .app(pos(node.pos), makeList(node.kids.slice(0, -2).map(tr)), tr(node.kids[node.kids.length - 1]),
                RUNTIME.pyretFalse);
        },
        'arrow-ann': function(node) {
          // (arrow-ann LPAREN args ... THINARROW result RPAREN)
          return RUNTIME.getField(ast, 'a-arrow')
            .app(pos(node.pos), makeList(node.kids.slice(1, -3).map(tr)), tr(node.kids[node.kids.length - 2]),
                RUNTIME.pyretTrue);
        },
        'app-ann': function(node) {
          // (app-ann ann LANGLE args ... RANGLE)
          return RUNTIME.getField(ast, 'a-app')
            .app(pos(node.pos), tr(node.kids[0]), makeList(node.kids.slice(2, -1).map(tr)));
        },
        'pred-ann': function(node) {
          // (pred-ann ann PERCENT LPAREN exp RPAREN)
          return RUNTIME.getField(ast, 'a-pred')
            .app(pos(node.pos), tr(node.kids[0]), tr(node.kids[3]));
        },
        'dot-ann': function(node) {
          // (dot-ann n1 PERIOD n2)
          return RUNTIME.getField(ast, 'a-dot')
            .app(pos(node.pos), name(node.kids[0]), symbol(node.kids[2]));
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
      return tr(node);
    }

    const opLookup = {
      "+": RUNTIME.makeString("op+"),
      "-": RUNTIME.makeString("op-"),
      "*": RUNTIME.makeString("op*"),
      "/": RUNTIME.makeString("op/"),
      "$": RUNTIME.makeString("op^"),
      "^": RUNTIME.makeString("op^"),
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

    function parseDataRaw(dialect, data, fileName) {
      const toks = dialects.dialects[dialect].Tokenizer;
      const grammar = dialects.dialects[dialect].Grammar;
      toks.tokenizeFrom(data);
      // while (toks.hasNext())
      //   console.log(toks.next().toString(true));
      var parsed = grammar.parse(toks);
      //console.log("Result:");
      var countParses = grammar.countAllParses(parsed);
      if (countParses == 0) {
        var nextTok = toks.curTok; 
        console.error("There were " + countParses + " potential parses.\n" +
                      "Parse failed, next token is " + nextTok.toString(true) +
                      " at " + nextTok.pos.toString(true));
        console.log(nextTok);
        if (toks.isEOF(nextTok))
          RUNTIME.ffi.throwParseErrorEOF(makePyretPos(fileName, nextTok.pos));
        else
          RUNTIME.ffi.throwParseErrorNextToken(makePyretPos(fileName, nextTok.pos), nextTok.value || nextTok.toString(true));
      }
      //console.log("There were " + countParses + " potential parses");
      if (countParses === 1) {
        var ast = grammar.constructUniqueParse(parsed);
        //          console.log(ast.toString());
        return translate(ast, fileName);
      } else {
        var asts = grammar.constructAllParses(parsed);
        throw "Non-unique parse";
        for (var i = 0; i < asts.length; i++) {
          //console.log("Parse " + i + ": " + asts[i].toString());
          //            console.log(("" + asts[i]) === ("" + asts2[i]));
        }
        return translate(ast, fileName);
      }
    }
    
    function parseDataDialect(dialect, data, fileName) {
      F.checkArity(3, arguments, "parse-dialect");
      RUNTIME.checkString(dialect);
      RUNTIME.checkString(data);
      RUNTIME.checkString(fileName);
      return parseDataRaw(RUNTIME.unwrap(dialect), RUNTIME.unwrap(data), RUNTIME.unwrap(fileName));
    }
    function parsePyret(data, fileName) {
      F.checkArity(2, arguments, "surface-parse");
      RUNTIME.checkString(data);
      RUNTIME.checkString(fileName);
      return parseDataRaw("Pyret", RUNTIME.unwrap(data), RUNTIME.unwrap(fileName));
    }
    function parseBootstrap(data, fileName) {
      F.checkArity(2, arguments, "parse-bootstrap");
      RUNTIME.checkString(data);
      RUNTIME.checkString(fileName);
      return parseDataRaw("Bootstrap", RUNTIME.unwrap(data), RUNTIME.unwrap(fileName));
    }

    return RUNTIME.makeObject({
      provide: RUNTIME.makeObject({
        'parse-dialect': RUNTIME.makeFunction(parseDataDialect),
        'surface-parse': RUNTIME.makeFunction(parsePyret), // TODO: Rename this eventually
        'parse-bootstrap': RUNTIME.makeFunction(parseBootstrap)
      }),
      answer: NAMESPACE.get("nothing")
    });
  });
});



