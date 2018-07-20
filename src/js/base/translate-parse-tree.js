define("pyret-base/js/translate-parse-tree", [], function() {
  
  function translate(ast, fileName, constructors) {
    let makeNode             = constructors.makeNode;
    let opLookup             = constructors.opLookup;
    let makeLink             = constructors.makeLink;
    let makeEmpty            = constructors.makeEmpty;
    let makeString           = constructors.makeString;
    let makeNumberFromString = constructors.makeNumberFromString;
    let makeBoolean          = constructors.makeBoolean;
    let makeNone             = constructors.makeNone; // RT.ffi.makeNone
    let makeSome             = constructors.makeSome; // RT.ffi.makeSome
    let getRecordFields      = constructors.getRecordFields; // getField(rec, 'fields')
    let makePyretPos         = constructors.makePyretPos;
    let combinePyretPos      = constructors.combinePyretPos;
    let detectAndComplainAboutOperatorWhitespace
      = constructors.detectAndComplainAboutOperatorWhitespace;

    // NOTE: This translation could blow the stack for very deep ASTs
    // We might have to rewrite the whole algorithm
    // One possibility is to reuse a stack of {todo: [...], done: [...], doing: fn} nodes
    // where each AST kid that needs to be recursively processed pushes a new frame on the stack
    // (it can eagerly process any primitive values, and defer the rest),
    // and returns a function to be called when all the new todos are done (which gets put into doing)
    // if a todo item is a Pyret value, it just gets pushed across to done
    // if a todo item is an array, then doing = RUNTIME.makeList and it creates a stack frame
    function tr(node) {
      if (translators[node.name] === undefined)
        throw new Error("Cannot find " + node.name + " in translators");
      return translators[node.name](node);
    }
    var pos = function(p) { return makePyretPos(fileName, p); };
    var pos2 = function(p1, p2) { return combinePyretPos(fileName, p1, p2); };
    function makeListTr(arr, start, end, onto, f) {
      var ret = onto || makeEmpty();
      start = start || 0;
      end = end || arr.length;
      f = f || tr;
      for (var i = end - 1; i >= start; i--)
        ret = makeLink(f(arr[i]), ret);
      return ret;
    }
    function makeListComma(arr, start, end, f) {
      var ret = makeEmpty();
      start = start || 0;
      end = end || arr.length;
      f = f || tr;
      for (var i = end - 1; i >= start; i -= 2)
        ret = makeLink(f(arr[i]), ret);
      return ret;
    }
    function makeList(arr, start, end, onto) {
      var ret = onto || makeEmpty();
      start = start || 0;
      end = end || arr.length;
      for (var i = end - 1; i >= start; i--)
        ret = makeLink(arr[i], ret);
      return ret;
    }
    function name(tok) {
      if (tok.value === "_")
        return makeNode('s-underscore', pos(tok.pos));
      else
        return makeNode('s-name', pos(tok.pos), makeString(tok.value));
    }
    function symbol(tok) {
      return makeString(tok.value);
    }
    function string(tok) {
      if (tok.value.substring(0, 3) === "```")
        return makeString(tok.value.slice(3, -3).trim());
      else
        return makeString(tok.value.slice(1, -1));
    }
    function number(tok) { return makeNumberFromString(tok.value); }
    const translators = {
      'program': function(node) {
        var prelude = tr(node.kids[0]);
        var body = tr(node.kids[1]);
        return makeNode('s-program',
                    pos(node.pos), prelude.provide, prelude.provideTypes, prelude.imports, body);
      },
      'prelude': function(node) {
        var provide;
        var provideTypes;
        var kids = node.kids.slice(0);
        if (kids.length > 0 && kids[0].name === "provide-stmt") {
          provide = tr(kids.shift());
        } else {
          provide = makeNode('s-provide-none', pos(node.pos));
        }
        if (kids.length > 0 && kids[0].name === "provide-types-stmt") {
          provideTypes = tr(kids.shift());
        } else {
          provideTypes = makeNode('s-provide-types-none', pos(node.pos));
        }
        return {
          provide : provide,
          provideTypes : provideTypes,
          imports : makeListTr(kids)
        };
      },
      'provide-stmt': function(node) {
        if (node.kids.length === 2) {
          // (provide-stmt PROVIDE STAR)
          return makeNode('s-provide-all', pos(node.pos));
        } else {
          // (provide-stmt PROVIDE stmt END)
          return makeNode('s-provide', pos(node.pos), tr(node.kids[1]));
        }
      },
      'provide-types-stmt': function(node) {
        if (node.kids[1].name === "STAR") {
          return makeNode('s-provide-types-all', pos(node.pos));
        } else {
          // will produce record-ann
          var rec = tr(node.kids[1]);
          // Get the fields out of it
          return makeNode('s-provide-types',
                      pos(node.pos), getRecordFields(rec));
        }
      },
      'import-stmt': function(node) {
        if (node.kids[node.kids.length - 2].name === "AS") {
          if (node.kids.length == 4) {
            // (import-stmt IMPORT import-source AS NAME)
            return makeNode('s-import',
                        pos(node.pos), tr(node.kids[1]), name(node.kids[3]));
          } else {
            // (import-stmt IMPORT import-source AS NAME, TYPES)
            return makeNode('s-import-types',
                        pos(node.pos), tr(node.kids[1]), name(node.kids[3]), name(node.kids[5]));
          }
        } else if (node.kids[0].name === "INCLUDE") {
          // (import-stmt INCLUDE import-source)
          return makeNode('s-include', pos(node.pos), tr(node.kids[1]));
        } else {
          // (import-stmt IMPORT comma-names FROM mod)
          return makeNode('s-import-fields',
                      pos(node.pos), tr(node.kids[1]), tr(node.kids[3]));
        }
      },
      'import-source': function(node) {
        return tr(node.kids[0]);
      },
      // (import-special NAME LPAREN STRING (COMMA STRING)* RPAREN)
      'import-special': function(node) {
        return makeNode('s-special-import',
                    pos(node.pos), symbol(node.kids[0]),
                    makeListComma(node.kids, 2, node.kids.length - 1, string))
      },
      'import-name': function(node) {
        // (import-name NAME)
        return makeNode('s-const-import',
                    pos(node.pos), symbol(node.kids[0]))
      },
      'block': function(node) {
        // (block stmts ...)
        detectAndComplainAboutOperatorWhitespace(node.kids, fileName);
        return makeNode('s-block',
                    pos(node.pos), makeListTr(node.kids));
      },
      'stmt': function(node) {
        // (stmt s)
        return tr(node.kids[0]);
      },
      'spy-stmt': function(node) {
        // (spy [label] COLON contents END)
        var label, contents;
        if (node.kids[1].name === "binop-expr") {
          label = makeSome(tr(node.kids[1]));
        } else {
          label = makeNone();
        }
        if (node.kids[node.kids.length - 2].name === "COLON") {
          contents = makeEmpty();
        } else {
          contents = tr(node.kids[node.kids.length - 2]);
        }
        return makeNode('s-spy-block',
                    pos(node.pos), label, contents);
      },
      'spy-contents': function(node) {
        return makeListComma(node.kids);
      },
      'spy-field': function(node) {
        if (node.kids.length === 1) {
          return makeNode('s-spy-expr',
                      pos(node.pos), symbol(node.kids[0].kids[0]), tr(node.kids[0]), makeBoolean(true));
        } else {
          return makeNode('s-spy-expr',
                      pos(node.pos), symbol(node.kids[0]), tr(node.kids[2]), makeBoolean(false));
        }
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
          return makeNode('s-singleton-variant',
                      pos(node.pos), symbol(node.kids[1]), tr(node.kids[2]));
        } else {
          // (data-variant PIPE variant-constructor with)
          var constr = tr(node.kids[1])
          return makeNode('s-variant',
                      pos(node.pos), constr.pos, constr.name, constr.args, tr(node.kids[2]));
        }
      },
      'first-data-variant': function(node) {
        if (node.kids[0].value !== undefined) {
          // (first-data-variant NAME with)
          return makeNode('s-singleton-variant',
                      pos(node.pos), symbol(node.kids[0]), tr(node.kids[1]));
        } else {
          // (first-data-variant variant-constructor with)
          var constr = tr(node.kids[0])
          return makeNode('s-variant',
                      pos(node.pos), constr.pos, constr.name, constr.args, tr(node.kids[1]));
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
      'type-expr': function(node) {
        return makeNode('s-type',
                    pos(node.pos),
                    name(node.kids[1]),
                    tr(node.kids[2]),
                    tr(node.kids[4]));
      },
      'newtype-expr': function(node) {
        return makeNode('s-newtype',
                    pos(node.pos), name(node.kids[1]), name(node.kids[3]));
      },
      'var-expr': function(node) {
        // (var-expr VAR bind EQUALS e)
        return makeNode('s-var',
                    pos(node.pos), tr(node.kids[1]), tr(node.kids[3]));
      },
      'rec-expr': function(node) {
        // (rec-expr REC bind EQUALS e)
        return makeNode('s-rec',
                    pos(node.pos), tr(node.kids[1]), tr(node.kids[3]));
      },
      'let-expr': function(node) {
        if (node.kids.length === 3) {
          // (let-expr bind EQUALS e)
          return makeNode('s-let',
                      pos(node.pos), tr(node.kids[0]), tr(node.kids[2]), makeBoolean(false));
        } else {
          // (let-expr VAL bind EQUALS e)
          return makeNode('s-let',
                      pos(node.pos), tr(node.kids[1]), tr(node.kids[3]), makeBoolean(true));
        }
      },
      'newtype-bind': function(node) {
        return makeNode('s-newtype-bind',
                    pos(node.pos), name(node.kids[1]), name(node.kids[3]));
      },
      'type-bind': function(node) {
        return makeNode('s-type-bind',
                    pos(node.pos),
                    name(node.kids[0]),
                    tr(node.kids[1]),
                    tr(node.kids[3]));
      },
      'type-let-bind': function(node) {
        return tr(node.kids[0]);
      },
      'type-let-binds': function(node) {
        // (type-let-binds COMMA type-let-bind)
        return tr(node.kids[1]);
      },
      'type-let-expr': function(node) {
        // (type-let-expr TYPE-LET type-let-bind (COMMA type-let-bind)* (BLOCK|COLON) block end
        var isBlock = (node.kids[node.kids.length - 3].name === "BLOCK");
        return makeNode('s-type-let-expr',
                    pos(node.pos),
                    makeListComma(node.kids, 1, node.kids.length - 3),
                    tr(node.kids[node.kids.length - 2]), isBlock);
      },
      'multi-let-expr': function(node) {
        // (multi-let-expr LET let-binding (COMMA let-binding)* COLON block END)
        // Note that we override the normal name dispatch here, because we don't want
        // to create the default let-expr or var-expr constructions
        var isBlock = (node.kids[node.kids.length - 3].name === "BLOCK");
        return makeNode('s-let-expr',
                    pos(node.pos),
                    makeListComma(node.kids, 1, node.kids.length - 3, translators["let-binding"]),
                    tr(node.kids[node.kids.length - 2]), isBlock);
      },
      'letrec-expr': function(node) {
        // (letrec-expr LETREC let-expr (COMMA let-expr)* (BLOCK|COLON0 block END)
        // Note that we override the normal name dispatch here, because we don't want
        // to create the default let-expr constructions
        var isBlock = (node.kids[node.kids.length - 3].name === "BLOCK");
        return makeNode('s-letrec',
                    pos(node.pos),
                    makeListComma(node.kids, 1, node.kids.length - 3, translators["letrec-binding"]),
                    tr(node.kids[node.kids.length - 2]), isBlock);
      },
      'let-binding': function(node) {
        if (node.name === "let-binding") {
          // (let-binding let-expr) or (let-binding var-expr)
          node = node.kids[0]
        }
        if (node.name === "let-expr") {
          // (let-expr binding EQUALS binop-expr)
          return makeNode('s-let-bind',
                      pos(node.pos), tr(node.kids[0]), tr(node.kids[2]));
        } else if (node.name === "var-expr") {
          // (var-expr VAR binding EQUALS binop-expr)
          return makeNode('s-var-bind',
                      pos(node.pos), tr(node.kids[1]), tr(node.kids[3]));
        }
      },
      'letrec-binding': function(node) {
        // (let-expr binding EQUALS binop-expr)
        return makeNode('s-letrec-bind',
                    pos(node.pos), tr(node.kids[0]), tr(node.kids[2]));
      },
      'contract-stmt': function(node) {
        // (contract-stmt NAME COLONCOLON ty-params ann)
        return makeNode('s-contract',
                    pos(node.pos), name(node.kids[0]), tr(node.kids[2]), tr(node.kids[3]));
      },
      'fun-header': function(node) {
        // (fun-header ty-params args return-ann)
        return {
          tyParams: tr(node.kids[0]),
          args: tr(node.kids[1]),
          returnAnn: tr(node.kids[2])
        };
      },
      'fun-expr': function(node) {
        // (fun-expr FUN fun-name fun-header COLON doc body check END)
        var isBlock = (node.kids[3].name === "BLOCK");
        var header = tr(node.kids[2]);
        var checkRes = tr(node.kids[6]);
        return makeNode('s-fun',
                    pos(node.pos), symbol(node.kids[1]),
                    header.tyParams,
                    header.args,
                    header.returnAnn,
                    tr(node.kids[4]),
                    tr(node.kids[5]),
                    checkRes[0], checkRes[1],
                    isBlock);
      },
      'data-expr': function(node) {
        // (data-expr DATA NAME params COLON variant ... sharing-part check END)
        var checkRes = tr(node.kids[node.kids.length - 2]);
        return makeNode('s-data',
                    pos(node.pos), symbol(node.kids[1]), tr(node.kids[2]), makeEmpty(),
                    makeListTr(node.kids, 4, node.kids.length - 3),
                    tr(node.kids[node.kids.length - 3]),
                    checkRes[0], checkRes[1]);
      },
      'assign-expr': function(node) {
        // (assign-expr id COLONEQUAL e)
        return makeNode('s-assign',
                    pos(node.pos), name(node.kids[0]), tr(node.kids[2]));
      },
      'when-expr': function(node) {
        // (when-expr WHEN test COLON body END)
        var isBlock = (node.kids[2].name === "BLOCK");
        return makeNode('s-when',
                    pos(node.pos), tr(node.kids[1]), tr(node.kids[3]), isBlock);
      },
      'check-expr': function(node) {
        if (node.kids.length === 3) {
          // (check-expr CHECKCOLON body END)
          return makeNode('s-check',
                      pos(node.pos), makeNone(), tr(node.kids[1]),
                      makeBoolean(node.kids[0].name === "CHECKCOLON"));
        } else {
          // (check-expr CHECK STRING COLON body END)
          return makeNode('s-check',
                      pos(node.pos), makeSome(string(node.kids[1])), tr(node.kids[3]),
                      makeBoolean(node.kids[0].name === "CHECK"));
        }
      },
      'check-test': function(node) {
        var kids = node.kids;
        if (kids.length === 1) {
          // (check-test e)
          return tr(kids[0]);
        } else if (kids.length === 2) {
          // (check-test left op)
          //             0    1
          return makeNode('s-check-test',
                      pos(node.pos), tr(kids[1]), makeNone(), tr(kids[0]), makeNone());
        } else if (kids.length === 3) {
          // (check-test left op right)
          //             0    1  2
          return makeNode('s-check-test',
                      pos(node.pos), tr(kids[1]), makeNone(), tr(kids[0]), makeSome(tr(kids[2])));
        }
        else {
          // (check-test left op PERCENT LPAREN refinement RPAREN right)
          //             0    1                 4                 6
          return makeNode('s-check-test',
                      pos(node.pos), tr(kids[1]), makeSome(tr(kids[4])), tr(kids[0]), makeSome(tr(kids[6])));
        }
      },
      'binop-expr': function(node) {
        if (node.kids.length === 1) {
          // (binop-expr e)
          return tr(node.kids[0]);
        } else {
          var expr = makeNode('s-op',
                              pos2(node.kids[0].pos, node.kids[2].pos),
                              pos(node.kids[1].pos),
                              tr(node.kids[1]),
                              tr(node.kids[0]),
                              tr(node.kids[2]));
          for(var i = 4; i < node.kids.length; i += 2) {
            expr = makeNode('s-op',
                            pos2(node.kids[0].pos, node.kids[i].pos),
                            pos(node.kids[i - 1].pos),
                            tr(node.kids[i - 1]),
                            expr,
                            tr(node.kids[i]));
          }
          return expr;
        }
      },
      'doc-string': function(node) {
        if (node.kids.length === 0) {
          // (doc-string)
          return makeString("");
        } else {
          // (doc-string DOC str)
          return string(node.kids[1]);
        }
      },
      'where-clause': function(node) {
        if (node.kids.length === 0) {
          // (where-clause)
          return [makeNone(), makeNone()];
        } else {
          // (where-clause WHERE block)
          return [makeSome(makePyretPos(fileName, node.kids[0].pos)),
                  makeSome(tr(node.kids[1]))];
        }
      },
      'check-op': function(node) {
        // (check-op str)
        var opname = String(node.kids[0].value).trim();
        if (opLookup[opname]) {
          return opLookup[opname](pos(node.pos));
        }
        else {
          throw "Unknown operator: " + opname;
        }
      },
      'check-op-postfix': function(node) {
        // (check-op-postfix str)
        var opname = String(node.kids[0].value).trim();
        if (opLookup[opname]) {
          return opLookup[opname](pos(node.pos));
        }
        else {
          throw "Unknown operator: " + opname;
        }
      },
      'expr': function(node) {
        // (expr e)
        return tr(node.kids[0]);
      },
      'template-expr': function(node) {
        return makeNode('s-template', pos(node.pos));
      },
      'binop-expr-paren': function(node) {
        if (node.kids[0].name === "paren-nospace-expr") {
          // (binop-expr-paren (paren-nospace-expr _ e _))
          return makeNode('s-paren',
                      pos(node.pos), tr(node.kids[0].kids[1]));
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
          return makeNode('a-blank');
        } else {
          // (return-ann THINARROW ann)
          return tr(node.kids[1]);
        }
      },
      
      'binding': function(node) {
        return tr(node.kids[0]);
      },

      'tuple-binding' : function(node) {
        var lastBinding = node.kids.length - 1;
        var optAsBinding;
        if (node.kids[lastBinding - 1].name === "AS") {
          optAsBinding = makeSome(tr(node.kids[lastBinding]));
          lastBinding -= 2;
        } else {
          optAsBinding = makeNone();
        }
        if (node.kids[lastBinding - 1].name === "SEMI") {
          lastBinding--;
        }
        return makeNode('s-tuple-bind',
                    pos(node.pos), makeListComma(node.kids, 1, lastBinding), optAsBinding);
      },

      'name-binding': function(node) {
        if (node.kids.length === 1) {
          // (binding name)
          return makeNode('s-bind',
                      pos(node.pos), makeBoolean(false), name(node.kids[0]),
                      makeNode('a-blank'));
        } else if (node.kids.length === 3) {
          // (binding name COLONCOLON ann)
          return makeNode('s-bind',
                      pos(node.pos), makeBoolean(false), name(node.kids[0]), tr(node.kids[2]));
        } else if (node.kids.length === 2) {
          // (binding SHADOW name)
          return makeNode('s-bind',
                      pos(node.pos), makeBoolean(true), name(node.kids[1]),
                      makeNode('a-blank'));
        } else {
          // (binding SHADOW name COLONCOLON ann)
          return makeNode('s-bind',
                      pos(node.pos), makeBoolean(true), name(node.kids[1]), tr(node.kids[3]));
        }
      },
      'toplevel-binding': function(node) {
        if (node.kids.length === 1) {
          // is actually a binding
          return tr(node.kids[0]);
        } else if (node.kids.length === 4) {
          // (toplevel-binding SHADOW NAME COLONCOLON noparen-arrow-ann)
          return makeNode('s-bind',
                      pos(node.pos), makeBoolean(true), name(node.kids[1]), tr(node.kids[3]));
        } else {
          // (toplevel-binding NAME COLONCOLON noparen-arrow-ann)
          return makeNode('s-bind',
                      pos(node.pos), makeBoolean(false), name(node.kids[0]), tr(node.kids[2]));
        }
      },
      'args': function(node) {
        if (node.kids.length === 2) {
          // (args LPAREN RPAREN)
          return makeList([]);
        } else {
          // (args LPAREN binding (COMMA binding)* RPAREN)
          return makeListComma(node.kids, 1, node.kids.length - 1);
        }
      },
      'variant-member': function(node) {
        if (node.kids.length === 1) {
          // (variant-member b)
          return makeNode('s-variant-member',
                      pos(node.pos), makeNode("s-normal"), tr(node.kids[0]));
        } else {
          return makeNode('s-variant-member',
                      pos(node.pos), makeNode("s-mutable"), tr(node.kids[1]));
        }
      },
      'variant-members': function(node) {
        if (node.kids.length === 2) {
          // (variant-members LPAREN RPAREN)
          return makeList([]);
        } else {
          // (variant-members LPAREN mem (COMMA mem)* RPAREN)
          return makeListComma(node.kids, 1, node.kids.length - 1);
        }
      },
      'key': function(node) {
        if (node.kids[0].name === "NAME") {
          // (key name)
          return symbol(node.kids[0]);
        } else {
          // (key str)
          return string(node.kids[0]);
        }
      },
      'obj-field': function(node) {
        if (node.kids.length === 4) {
          // (obj-field MUTABLE key COLON value)
          return makeNode('s-mutable-field',
                      pos(node.pos), tr(node.kids[1]), makeNode('a-blank'), tr(node.kids[3]));
        } else if (node.kids.length === 6) {
          // (obj-field MUTABLE key COLONCOLON ann COLON value)
          return makeNode('s-mutable-field',
                      pos(node.pos), tr(node.kids[1]), tr(node.kids[3]), tr(node.kids[5]));
        } else if (node.kids.length === 3) {
          // (obj-field key COLON value)
          return makeNode('s-data-field',
                      pos(node.pos), tr(node.kids[0]), tr(node.kids[2]));
        } else {
          // (obj-field METHOD key fun-header COLON doc body check END)
          var isBlock = (node.kids[3].name === "BLOCK");
          var header = tr(node.kids[2]);
          var checkRes = tr(node.kids[6])
          return makeNode('s-method-field',
                      pos(node.pos), tr(node.kids[1]), header.tyParams, header.args, header.returnAnn,
                      tr(node.kids[4]), tr(node.kids[5]), checkRes[0], checkRes[1], isBlock);
        }
      },
      'tuple-name-list' : function(node) {
        if (node.kids[node.kids.length - 1].name !== "binding") {
          // (obj-fields (list-tuple-field f1 SEMI) ... lastField SEMI)
          return makeListComma(node.kids, 0, node.kids.length - 1);
        } else {
          // (fields (list-tuple-field f1 SEMI) ... lastField)
          return makeListComma(node.kids);
        }
      },
      'tuple-fields' : function(node) {
        if (node.kids[node.kids.length - 1].name === "SEMI") {
          // (obj-fields (list-tuple-field f1 SEMI) ... lastField SEMI)
          return makeListComma(node.kids, 0, node.kids.length - 1);
        } else {
          // (fields (list-tuple-field f1 SEMI) ... lastField)
          return makeListComma(node.kids);
        }
      },
      'reactor-expr': function(node) {
        // (REACTOR COLON fields END)
        return makeNode('s-reactor',
                    pos(node.pos), tr(node.kids[2]));
      },
      'table-expr': function(node) {
        // (TABLE table-headers table-rows end)
        return makeNode('s-table',
                    pos(node.pos), tr(node.kids[1]), tr(node.kids[2]));
      },
      'load-table-expr': function(node) {
        // (LOAD-TABLE COLON table-headers load-table-specs END)
        return makeNode('s-load-table',
                    pos(node.pos), tr(node.kids[2]),
                    ((node.kids[3].name === "END")
                     ? makeList([]) : tr(node.kids[3])));
      },
      'table-headers': function(node) {
        // [list-table-header* table-header]
        return makeList(node.kids.map(tr));
      },
      'list-table-header': function(node) {
        // (table-header COMMA)
        return tr(node.kids[0]);
      },
      'table-header': function(node) {
        // NAME [:: ann]
        if (node.kids.length === 3) {
          return makeNode('s-field-name',
                      pos(node.pos), symbol(node.kids[0]), tr(node.kids[2]))
        } else {
          return makeNode('s-field-name',
                      pos(node.pos), symbol(node.kids[0]), makeNode('a-blank'))
        }
      },
      'table-rows': function(node) {
        // [table-row* table-row]
        return makeList(node.kids.map(tr));
      },
      'table-row': function(node) {
        // (ROW table-items)
        return makeNode('s-table-row',
                    pos(node.pos), tr(node.kids[1]));
      },
      'table-items': function(node) {
        // [list-table-item* binop-expr]
        return makeList(node.kids.map(tr));
      },
      'list-table-item': function(node) {
        // (binop-expr COMMA)
        return tr(node.kids[0]);
      },
      'table-extend-fields': function(node) {
        if (node.kids[node.kids.length - 1].name !== "table-extend-field") {
          return makeList(node.kids.slice(0, -1).map(tr));
        } else {
          // [list-table-extend-field* table-extend-field COMMA]
          return makeList(node.kids.map(tr));
        }
      },
      'list-table-extend-field': function(node) {
        // (table-extend-field COMMA)
        return tr(node.kids[0]);
      },
      'table-extend-field': function(node) {
        if (node.kids.length === 3) {
          // (key COLON binop-expr)
          return makeNode('s-table-extend-field',
                      pos(node.pos), tr(node.kids[0]), tr(node.kids[2]),
                      makeNode('a-blank'));
        } else if ((node.kids.length === 5)
                   && (node.kids[1].name === "COLONCOLON")){
          // (key COLONCOLON ann COLON binop-expr)
          return makeNode('s-table-extend-field',
                      pos(node.pos), tr(node.kids[0]), tr(node.kids[4]),
                      tr(node.kids[2]));
        } else if (node.kids.length === 5) {
          // (key COLON expr OF NAME)
          return makeNode('s-table-extend-reducer',
                      pos(node.pos), tr(node.kids[0]), tr(node.kids[2]),
                      name(node.kids[4]), makeNode('a-blank'));
        } else if (node.kids.length === 7) {
          // (key COLONCOLON ann COLON expr OF NAME)
          return makeNode('s-table-extend-reducer',
                      pos(node.pos), tr(node.kids[0]), tr(node.kids[4]),
                      name(node.kids[6]), tr(node.kids[2]));
        }
      },
      'load-table-specs': function(node) {
        if (node.kids[node.kids.length - 1].name !== "load-table-spec") {
          return makeList(node.kids.slice(0, -1).map(tr));
        } else {
          // [list-load-table-spec* load-table-spec COMMA]
          return makeList(node.kids.map(tr));
        }
      },
      'load-table-spec': function(node) {
        if (node.kids[0].name === "SANITIZE") {
          // (SANITIZE NAME USING expr)
          return makeNode('s-sanitize',
                      pos(node.pos), name(node.kids[1]), tr(node.kids[3]));
        } else {
          // (SOURCECOLON expr)
          return makeNode('s-table-src',
                      pos(node.pos), tr(node.kids[1]));
        }
      },
      'sql-expr': function(node) {
        var inspect = tr(node.kids[1]);
        var where = node.kids.length == 5
          ? makeNone()
          : makeSome(tr(node.kids[3]));
        var project = tr(node.kids[node.kids.length - 2]);
        return makeNode('s-sql',
                    pos(node.pos),
                    inspect, // from
                    where, // where
                    project); // project
      },
      'do-expr': function(node) {
        // (do FOR iter binds ... return COLON body END)
        return makeNode('s-do',
                    pos(node.pos),
                    tr(node.kids[1]),                         // iterator
                    makeList(node.kids.slice(3, -4).map(tr)), // bindings
                    tr(node.kids[node.kids.length - 3]),      // return-ann
                    tr(node.kids[node.kids.length - 1]));     // body
      },
      'for-then': function(node) {
        // (for-then FOR iter LPAREN binds ... RPAREN return COLON body)
        return makeNode('s-for',
                    pos(node.pos), tr(node.kids[1]), makeList(node.kids.slice(3, -4).map(tr)),
                    tr(node.kids[node.kids.length - 3]), tr(node.kids[node.kids.length - 1]));
      },
      'for-bind-elt': function(node) {
        // (for-bind-elt b COMMA)
        return tr(node.kids[0]);
      },
      'obj-fields': function(node) {
        if (node.kids[node.kids.length - 1].name !== "obj-field") {
          // (obj-fields objField (COMMA obj-field)* lastField COMMA)
          return makeListComma(node.kids, 0, node.kids.length - 1);
        } else {
          // (obj-fields obj-field (COMMA obj-field)*)
          return makeListComma(node.kids);
        }
      },
      'field': function(node) {
        if (node.kids.length === 3) {
          // (field key COLON value)
          return makeNode("s-data-field",
                      pos(node.pos), tr(node.kids[0]), tr(node.kids[2]));
        } else {
          // (field METHOD key fun-header (BLOCK|COLON) doc body check END)
          var isBlock = (node.kids[3].name === "BLOCK");
          var header = tr(node.kids[2]);
          var checkRes = tr(node.kids[6])
          return makeNode("s-method-field",
                      pos(node.pos), tr(node.kids[1]), header.tyParams, header.args, header.returnAnn,
                      tr(node.kids[4]), tr(node.kids[5]), checkRes[0], checkRes[1], isBlock);
        }
      },
      'fields': function(node) {
        if (node.kids[node.kids.length - 1].name !== "field") {
          // (fields field (COMMA f1)* COMMA)
          return makeListComma(node.kids, 0, node.kids.length - 1);
        } else {
          // (fields field (COMMA f1)*)
          return makeListComma(node.kids);
        }
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
      'app-args': function(node) {
        // (app-args LPAREN opt-comma-binops RPAREN)
        return tr(node.kids[1]);
      },
      'opt-comma-binops': function(node) {
        if (node.kids.length === 0) {
          return makeEmpty();
        } else {
          return tr(node.kids[0]);
        }
      },
      'comma-binops': function(node) {
        return makeListComma(node.kids);
      },
      'trailing-opt-comma-binops': function(node) {
        if (node.kids.length === 0) {
          return makeEmpty();
        } else {
          return tr(node.kids[0]);
        }
      },
      'cases-args': function(node) {
        if (node.kids.length === 2) {
          // (cases-args LPAREN RPAREN)
          return makeList([]);
        } else {
          // (cases-args LPAREN cases-binding (COMMA arg)* RPAREN)
          return makeListComma(node.kids, 1, node.kids.length - 1);
        }
      },
      'cases-binding': function(node) {
        if(node.kids.length === 2) {
          return makeNode('s-cases-bind',
                      pos(node.pos), makeNode('s-cases-bind-ref'), tr(node.kids[1]));
        }
        else {
          return makeNode('s-cases-bind',
                      pos(node.pos), makeNode('s-cases-bind-normal'), tr(node.kids[0]));
        }
      },
      'cases-branch': function(node) {
        if (node.kids.length === 4) {
          // (singleton-cases-branch PIPE NAME THICKARROW body)
          return makeNode('s-singleton-cases-branch',
                      pos(node.pos), pos(node.kids[1].pos), symbol(node.kids[1]), tr(node.kids[3]));
        } else {
          // (cases-branch PIPE NAME args THICKARROW body)
          return makeNode('s-cases-branch',
                      pos(node.pos), pos(node.kids[1].pos.combine(node.kids[2].pos)),
                      symbol(node.kids[1]), tr(node.kids[2]), tr(node.kids[4]));
        }
      },
      'if-pipe-branch': function(node) {
        // (if-pipe-branch BAR binop-expr THENCOLON block)
        return makeNode('s-if-pipe-branch',
                    pos(node.pos), tr(node.kids[1]), tr(node.kids[3]));
      },
      'else-if': function(node) {
        // (else-if ELSEIF test COLON body)
        return makeNode('s-if-branch',
                    pos(node.pos), tr(node.kids[1]), tr(node.kids[3]));
      },
      'else': function(node) {
        // (else ELSECOLON body)
        return makeNode('s-else',
                    pos(node.pos), tr(node.kids[1]));
      },
      'ty-params': function(node) {
        if (node.kids.length === 0) {
          // (ty-params)
          return makeList([]);
        } else {
          // (ty-params LANGLE comma-names RANGLE)
          return tr(node.kids[1]);
        }
      },
      'for-bind': function(node) {
        // (for-bind name FROM e)
        return makeNode('s-for-bind',
                    pos(node.pos), tr(node.kids[0]), tr(node.kids[2]));
      },
      'prim-expr': function(node) {
        // (prim-expr e)
        return tr(node.kids[0]);
      },
      'tuple-expr': function(node) {
        return makeNode('s-tuple',
                    pos(node.pos), tr(node.kids[1]))
      },
      'tuple-get': function(node) {
        return makeNode('s-tuple-get',
                    pos(node.pos), tr(node.kids[0]), number(node.kids[3]), pos(node.kids[3].pos))
      },
      'obj-expr': function(node) {
        if (node.kids.length === 2) {
          // (obj-expr LBRACE RBRACE)
          return makeNode('s-obj',
                      pos(node.pos), makeList([]));
        } else {
          // (obj-expr LBRACE obj-fields RBRACE)
          return makeNode('s-obj',
                      pos(node.pos), tr(node.kids[1]));
        }
      },
      'construct-expr': function(node) {
        // LBRACK construct-modifier binop-expr COLON trailing-opt-comma-binops RBRACK
        return makeNode('s-construct',
                    pos(node.pos), tr(node.kids[1]), tr(node.kids[2]), tr(node.kids[4]));
      },
      'construct-modifier': function(node) {
        if (node.kids.length === 0) {
          return makeNode('s-construct-normal');
        } else if (node.kids.length === 1) {
          if (node.kids[0].name === "LAZY") {
            return makeNode('s-construct-lazy');
          }
        }
      },
      'app-expr': function(node) {
        // (app-expr f args)
        return makeNode('s-app',
                    pos(node.pos), tr(node.kids[0]), tr(node.kids[1]));
      },
      'id-expr': function(node) {
        // (id-expr x)
        return makeNode('s-id',
                    pos(node.pos), name(node.kids[0]));
      },
      'dot-expr': function(node) {
        // (dot-expr obj PERIOD field)
        return makeNode('s-dot',
                    pos(node.pos), tr(node.kids[0]), symbol(node.kids[2]));
      },
      'get-bang-expr': function(node) {
        // (get-bang-expr obj BANG field)
        return makeNode('s-get-bang',
                    pos(node.pos), tr(node.kids[0]), symbol(node.kids[2]));
      },
      'bracket-expr': function(node) {
        // (bracket-expr obj LBRACK field RBRACK)
        return makeNode('s-bracket',
                    pos(node.pos), tr(node.kids[0]), tr(node.kids[2]));
      },
      'cases-expr': function(node) {
        var isBlock = (node.kids[5].name === "BLOCK");
        if (node.kids[node.kids.length - 4].name === "ELSE") {
          // (cases-expr CASES LPAREN type RPAREN val COLON branch ... PIPE ELSE THICKARROW elseblock END)
          return makeNode('s-cases-else',
                      pos(node.pos), tr(node.kids[2]), tr(node.kids[4]),
                      makeListTr(node.kids, 6, node.kids.length - 5), tr(node.kids[node.kids.length - 2]), isBlock);
        } else {
          // (cases-expr CASES LPAREN type RPAREN val COLON branch ... END)
          return makeNode('s-cases',
                      pos(node.pos), tr(node.kids[2]), tr(node.kids[4]),
                      makeListTr(node.kids, 6, node.kids.length - 1), isBlock);
        }
      },
      'if-pipe-expr': function(node) {
        var isBlock = (node.kids[1].name === "BLOCK");
        if (node.kids[node.kids.length - 3].name === "OTHERWISECOLON") {
          // (if-pipe-expr ASK (BLOCK|COLON) branch ... BAR OTHERWISECOLON else END)
          return makeNode('s-if-pipe-else',
                      pos(node.pos), makeListTr(node.kids, 2, node.kids.length - 4),
                      tr(node.kids[node.kids.length - 2]), isBlock);
        } else {
          // (if-pipe-expr ASK (BLOCK|COLON) branch ... END)
          return makeNode('s-if-pipe',
                      pos(node.pos), makeListTr(node.kids, 2, node.kids.length - 1), isBlock);
        }
      },
      'if-expr': function(node) {
        var isBlock = (node.kids[2].name === "BLOCK");
        if (node.kids[node.kids.length - 3].name === "ELSECOLON") {
          // (if-expr IF test (BLOCK|COLON) body branch ... ELSECOLON else END)
          return makeNode('s-if-else',
                      pos(node.pos),
                      makeList([makeNode('s-if-branch',
                                     pos2(node.kids[1].pos, node.kids[3].pos), tr(node.kids[1]), tr(node.kids[3]))],
                               0, 1,
                               makeListTr(node.kids, 4, node.kids.length - 3)),
                      tr(node.kids[node.kids.length - 2]), isBlock);
        } else {
          // (if-expr IF test (BLOCK|COLON) body branch ... END)
          return makeNode('s-if',
                      pos(node.pos),
                      makeList([makeNode('s-if-branch',
                                     pos2(node.kids[1].pos, node.kids[3].pos), tr(node.kids[1]), tr(node.kids[3]))],
                               0, 1,
                               makeListTr(node.kids, 4, node.kids.length - 1)), isBlock);
        }
      },
      'for-expr': function(node) {
        // (for-expr FOR iter LPAREN for-bind (COMMA for-bind)* RPAREN return (BLOCK|COLON) body END)
        var isBlock = (node.kids[node.kids.length - 3].name === "BLOCK");
        return makeNode('s-for',
                    pos(node.pos), tr(node.kids[1]), makeListComma(node.kids, 3, node.kids.length - 5),
                    tr(node.kids[node.kids.length - 4]), tr(node.kids[node.kids.length - 2]), isBlock);
      },
      'user-block-expr': function(node) {
        // (user-block-expr BLOCK body END)
        return makeNode('s-user-block',
                    pos(node.pos), tr(node.kids[1]));
      },
      'lambda-expr': function(node) {
        // (lambda-expr LAM fun-header COLON doc body check END)
        var isBlock = (node.kids[2].name === "BLOCK");
        var header = tr(node.kids[1]);
        var checkRes = tr(node.kids[5]);
        return makeNode('s-lam',
                    pos(node.pos), makeString(""), header.tyParams, header.args, header.returnAnn,
                    tr(node.kids[3]), tr(node.kids[4]), checkRes[0], checkRes[1], isBlock);
      },
      'method-expr': function(node) {
        // (method-expr METHOD fun-header COLON doc body check END)
        var isBlock = (node.kids[2].name === "BLOCK");
        var header = tr(node.kids[1]);
        var checkRes = tr(node.kids[5]);
        return makeNode('s-method',
                    pos(node.pos), makeString(""), header.tyParams, header.args, header.returnAnn,
                    tr(node.kids[3]), tr(node.kids[4]), checkRes[0], checkRes[1], isBlock);
      },
      'extend-expr': function(node) {
        // (extend-expr e PERIOD LBRACE fields RBRACE)
        return makeNode('s-extend',
                    pos(node.pos), tr(node.kids[0]), tr(node.kids[3]));
      },
      'update-expr': function(node) {
        // (update-expr e BANG LBRACE fields RBRACE)
        return makeNode('s-update',
                    pos(node.pos), tr(node.kids[0]), tr(node.kids[3]));
      },
      'paren-expr': function(node) {
        // (paren-expr LPAREN e RPAREN)
        return makeNode('s-paren',
                    pos(node.pos), tr(node.kids[1]));
      },
      'paren-nospace-expr': function(node) {
        // (paren-nospace-expr LPAREN e RPAREN)
        return makeNode('s-paren',
                    pos(node.pos), tr(node.kids[1]));
      },
      'inst-expr': function(node) {
        // (inst-expr e LANGLE ann (COMMA ann)* RANGLE)
        return makeNode('s-instantiate',
                    pos(node.pos), tr(node.kids[0]), makeListComma(node.kids, 2, node.kids.length - 1));
      },
      'bool-expr': function(node) {
        if (node.kids[0].name === "TRUE") {
          return makeNode('s-bool',
                      pos(node.pos), makeBoolean(true));
        } else {
          return makeNode('s-bool',
                      pos(node.pos), makeBoolean(false));
        }
      },
      'num-expr': function(node) {
        // (num-expr n)
        return makeNode('s-num',
                    pos(node.pos), number(node.kids[0]));
      },
      'frac-expr': function(node) {
        // (frac-expr n)
        var numden = node.kids[0].value.split("/");
        return makeNode('s-frac',
                    pos(node.pos), makeNumberFromString(numden[0]), makeNumberFromString(numden[1]));
      },
      'rfrac-expr': function(node) {
        // (rfrac-expr n)
        var numden = node.kids[0].value.substring(1).split("/");
        return makeNode('s-rfrac',
                    pos(node.pos), makeNumberFromString(numden[0]), makeNumberFromString(numden[1]));
      },
      'string-expr': function(node) {
        return makeNode('s-str',
                    pos(node.pos), string(node.kids[0]));
      },
      'ann-field': function(node) {
        // (ann-field n COLON ann) or (ann-field n COLONCOLON ann)
        return makeNode('a-field',
                    pos(node.pos), symbol(node.kids[0]), tr(node.kids[2]));
      },
      'name-ann': function(node) {
        if (node.kids[0].value === "Any") {
          return makeNode('a-any', pos(node.pos));
        } else {
          return makeNode('a-name',
                      pos(node.pos), name(node.kids[0]));
        }
      },
      'comma-ann-field': function(node) {
        return makeListComma(node.kids);
      },
      'trailing-opt-comma-ann-field': function(node) {
        if (node.kids.length === 0) {
          return makeEmpty();
        } else {
          return tr(node.kids[0]);
        }
      },
      'record-ann': function(node) {
        // (record-ann LBRACE ann-field (COMMA ann-field)* RBRACE)
        return makeNode('a-record',
                    pos(node.pos), tr(node.kids[1]));
      },
      'tuple-ann': function(node) {
        // (tuple LBRACE ann (SEMI ann)* [SEMI] RBRACE
        if (node.kids[node.kids.length - 2].name === "SEMI") {
          return makeNode('a-tuple',
                      pos(node.pos), makeListComma(node.kids, 1, node.kids.length - 2));
        } else {
          return makeNode('a-tuple',
                      pos(node.pos), makeListComma(node.kids, 0, node.kids.length - 1));
        }
      },
      'noparen-arrow-ann': function(node) {
        if (node.kids.length === 2) {
          // (noparen-arrow-ann THINARROW result)
          return makeNode('a-arrow',
                      pos(node.pos),
                      makeEmpty(), tr(node.kids[1]),
                      makeBoolean(false));
        } else {
          // (noparen-arrow-ann arrow-ann-args THINARROW result)
          var trArgs = tr(node.kids[0]);
          if (trArgs.named) {
            return makeNode('a-arrow-argnames',
                        pos(node.pos), trArgs.args, tr(node.kids[2]), makeBoolean(false));
          } else {
            return makeNode('a-arrow',
                        pos(node.pos), trArgs.args, tr(node.kids[2]), makeBoolean(false));
          }
        }
      },
      'arrow-ann-args': function(node) {
        if (node.kids.length === 1) {
          // (arrow-ann-args comma-anns)
          return { args: tr(node.kids[0]), named: false }
        } else {
          // (arrow-ann-args LPAREN comma-ann-field RPAREN
          return { args: tr(node.kids[1]), named: true }
        }
      },
      //TABLE-EXTEND expr [USING binding (COMMA binding)*] COLON obj-fields end
      //           0    1      3       4                -4    -3         -2  -1
      'table-extend': function(node) {
        var columns = new Array();
        for (var i = 3; i < node.kids.length - 3; i+=2)
          columns.push(tr(node.kids[i]));
        var table = tr(node.kids[1]);
        var extensions = tr(node.kids[node.kids.length - 2]);
        return makeNode('s-table-extend', pos(node.pos),
                    makeNode('s-column-binds', 
                         combinePyretPos(fileName, node.kids[1].pos, node.kids[node.kids.length - 4].pos),
                         makeList(columns),
                         table),
                    extensions);
      },
      'table-update': function(node) {
        var columns = new Array();
        for (var i = 3; i < node.kids.length - 3; i+=2)
          columns.push(tr(node.kids[i]));
        var table = tr(node.kids[1]);
        var extensions = tr(node.kids[node.kids.length - 2]);
        return makeNode('s-table-update', pos(node.pos),
                    makeNode('s-column-binds', 
                         combinePyretPos(fileName, node.kids[1].pos, node.kids[node.kids.length - 4].pos),
                         makeList(columns),
                         table),
                    extensions);
      },
      //TABLE-SELECT NAME (COMMA NAME)* FROM expr end
      'table-select': function(node) {
        var columns = new Array();
        for (var i = 1; i < node.kids.length - 3; i+=2)
          columns.push(name(node.kids[i]));
        var table = tr(node.kids[node.kids.length - 2]);
        return makeNode('s-table-select', 
                    pos(node.pos), makeList(columns), table);
      },
      'column-order': function(node) {
        var column = name(node.kids[0]);
        var direction = node.kids[1].name == "ASCENDING"  ? makeNode('ASCENDING')
          : node.kids[1].name == "DESCENDING" ? makeNode('DESCENDING')
          : undefined;
        return makeNode('s-column-sort',
                    pos(node.pos),
                    column,
                    direction);
      },
      'table-order': function(node) {
        // TABLE-ORDER NAME COLON column-orderings end
        return makeNode('s-table-order',
                    pos(node.pos),
                    tr(node.kids[1]),
                    makeListComma(node.kids, 3, node.kids.length - 1, tr));
      },
      'table-filter': function(node) {
        var columns = new Array();
        for (var i = 3; i < node.kids.length - 3; i+=2)
          columns.push(tr(node.kids[i]));
        var table = tr(node.kids[1]);
        var predicate = tr(node.kids[node.kids.length - 2]);
        return makeNode('s-table-filter', pos(node.pos),
                    makeNode('s-column-binds', 
                         combinePyretPos(fileName, node.kids[1].pos, node.kids[node.kids.length - 4].pos),
                         makeList(columns),
                         table),
                    predicate);
      },
      'table-extract': function(node) {
        return makeNode('s-table-extract', pos(node.pos),
                    name(node.kids[1]), tr(node.kids[3]));
      },
      'arrow-ann': function(node) {
        if (node.kids.length === 4) {
          // (arrow-ann LPAREN THINARROW result RPAREN)
          return makeNode('a-arrow',
                      pos(node.pos), makeEmpty(),
                      tr(node.kids[2]),
                      makeBoolean(true));
        } else {
          // (arrow-ann LPAREN arrow-ann-args THINARROW result RPAREN)
          // (noparen-arrow-ann arrow-ann-args THINARROW result)
          var trArgs = tr(node.kids[1]);
          if (trArgs.named) {
            return makeNode('a-arrow-argnames',
                        pos(node.pos), trArgs.args, tr(node.kids[3]), makeBoolean(true));
          } else {
            return makeNode('a-arrow',
                        pos(node.pos), trArgs.args, tr(node.kids[3]), makeBoolean(true));
          }
        }
      },
      'app-ann': function(node) {
        // (app-ann ann LANGLE comma-anns RANGLE)
        return makeNode('a-app',
                    pos(node.pos), tr(node.kids[0]), tr(node.kids[2]));
      },
      'comma-anns': function(node) {
        return makeListComma(node.kids);
      },
      'comma-names': function(node) {
        return makeListComma(node.kids, 0, node.kids.length, name);
      },
      'comma-binops': function(node) {
        return makeListComma(node.kids);
      },
      'pred-ann': function(node) {
        // (pred-ann ann PERCENT LPAREN exp RPAREN)
        return makeNode('a-pred',
                    pos(node.pos), tr(node.kids[0]), tr(node.kids[3]));
      },
      'dot-ann': function(node) {
        // (dot-ann n1 PERIOD n2)
        return makeNode('a-dot',
                    pos(node.pos), name(node.kids[0]), symbol(node.kids[2]));
      },
      'ann': function(node) {
        // (ann a)
        return tr(node.kids[0]);
      }
    };
    return tr(ast);
  }

  return {
    'translate': translate
  }
})
