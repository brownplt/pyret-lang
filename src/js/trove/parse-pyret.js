({
  requires: [
    { "import-type": "builtin", name: "srcloc" },
    { "import-type": "builtin", name: "ast" },
    { "import-type": "builtin", name: "lists" }
  ],
  nativeRequires: [
    "pyret-base/js/pyret-tokenizer",
    "pyret-base/js/pyret-parser"
  ],
  provides: {
    shorthands: {
      "Program": {
          tag: "name",
          origin: { "import-type": "uri", uri: "builtin://ast" },
          name: "Program"
        }
    },
    values: {
      "surface-parse": ["arrow", ["String", "String"], "Program"],
      "maybe-surface-parse": ["arrow", ["String", "String"], ["Option", "Program"]],
    }
  },
  theModule: function(RUNTIME, NAMESPACE, uri, srclocLib, astLib, listsLib, tokenizer, parser) {
    var srcloc = RUNTIME.getField(srclocLib, "values");
    var ast = RUNTIME.getField(astLib, "values");
    var lists = RUNTIME.getField(listsLib, "values");

    var link = RUNTIME.getField(lists, "link");
    var empty = RUNTIME.getField(lists, "empty");

    var makePyretPos = RUNTIME.ffi.makePyretPos;
    var combinePyretPos = RUNTIME.ffi.combinePyretPos;

    //var data = "#lang pyret\n\nif (f(x) and g(y) and h(z) and i(w) and j(u)): true else: false end";
    
    function isSignedNumberAsStmt(stmt) {
      var node = stmt;
      if (node.name !== "stmt") return false;       node = node.kids[0];
      if (node.name !== "check-test") return false; node = node.kids[0];
      if (node.name !== "binop-expr") return false; node = node.kids[0];
      if (node.name !== "expr") return false;       node = node.kids[0];
      if (node.name !== "prim-expr") return false;  node = node.kids[0];
      if (node.name !== "num-expr") return false;   node = node.kids[0];
      if (node.name !== "NUMBER") return false;
      return node.value[0] === '-' || node.value[0] === '+';
    }
    function detectAndComplainAboutOperatorWhitespace(stmts, fileName) {
      for (var i = 1; i < stmts.length; i++) {
        if (isSignedNumberAsStmt(stmts[i]) &&
            stmts[i].pos.startRow === stmts[i - 1].pos.endRow) {
          var pos = stmts[i].pos;
          var n = RUNTIME.makeNumber;
          RUNTIME.ffi.throwParseErrorBadOper(
            RUNTIME.getField(srcloc, "srcloc")
              .app(RUNTIME.makeString(fileName),
                   n(pos.startRow), n(pos.startCol), n(pos.startChar),
                   n(pos.startRow), n(pos.startCol + 1), n(pos.startChar + 1)));
        }
      }
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
        if (translators[node.name] === undefined)
          throw new Error("Cannot find " + node.name + " in translators");
        return translators[node.name](node);
      }

      function nameSpec(node, constructor) {
        return RUNTIME.getField(ast, constructor).app(
            pos(node.pos),
            tr(node.kids[0])
        );
      }
      function typeSpec(node, constructor) {
        return RUNTIME.getField(ast, constructor).app(
            pos(node.pos),
            tr(node.kids[1])
        );
      }
      function dataSpec(node, constructor) {
        var hidings;
        node.kids[1].name = 'name-spec';
        if(node.kids.length === 2) {
          hidings = makeListTr([]);
        }
        else {
          hidings = tr(node.kids[2]);
        }
        return RUNTIME.getField(ast, constructor).app(
            pos(node.pos),
            tr(node.kids[1]),
            hidings
            );
      }
      function moduleSpec(node, constructor) {
        return RUNTIME.getField(ast, constructor).app(
            pos(node.pos),
            tr(node.kids[1])
        );
      }

      var pos = function(p) { return makePyretPos(fileName, p); };
      var pos2 = function(p1, p2) { return combinePyretPos(fileName, p1, p2); };
      function makeListTr(arr, start, end, onto, f) {
        var ret = onto || empty;
        start = start || 0;
        end = end || arr.length;
        f = f || tr;
        for (var i = end - 1; i >= start; i--)
          ret = link.app(f(arr[i]), ret);
        return ret;
      }
      function makeListComma(arr, start, end, f) {
        var ret = empty;
        start = start || 0;
        end = end || arr.length;
        f = f || tr;
        for (var i = end - 1; i >= start; i -= 2)
          ret = link.app(f(arr[i]), ret);
        return ret;
      }
      function makeList(arr, start, end, onto) {
        var ret = onto || empty;
        start = start || 0;
        end = end || arr.length;
        for (var i = end - 1; i >= start; i--)
          ret = link.app(arr[i], ret);
        return ret;
      }
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
            .app(pos(node.pos), prelude.use, prelude.provides, prelude.provideTypes, prelude.allProvides, prelude.imports, body);
        },
        'prelude': function(node) {
          var use = RUNTIME.makeNone();
          var provides = undefined;
          var provideTypes = undefined;
          var allProvides = [];
          var imports = [];

          node.kids.forEach(function(kid, i) {
            if(kid.name === "use-stmt") {
              use = RUNTIME.makeSome(tr(kid));
            }
            else if(provideTypes === undefined && kid.kids[0].name === 'provide-types-stmt') {
              provideTypes = tr(kid);
            }
            else if(provides === undefined && kid.kids[0].name === 'provide-vals-stmt') {
              provides = tr(kid);
            }
            else if (kid.kids[0].name === 'provide-block') {
              allProvides.push(kid);
            }
            else if (kid.kids[0].name === "INCLUDE" || kid.kids[0].name === "IMPORT") {
              imports.push(kid);
            }
          });
          if(provides === undefined) {
            provides = RUNTIME.getField(ast, "s-provide-none").app(pos(node.pos));
          }
          if(provideTypes === undefined) {
            provideTypes = RUNTIME.getField(ast, "s-provide-types-none").app(pos(node.pos));
          }
          return {
            use: use,
            provides: provides,
            provideTypes: provideTypes,
            allProvides: makeListTr(allProvides),
            imports: makeListTr(imports)
          };
        },
        'use-stmt': function(node) {
          return RUNTIME.getField(ast, "s-use").app(pos(node.pos), name(node.kids[1]), tr(node.kids[2]));
        },
        'include-spec': function(node) {
          return tr(node.kids[0]);
        },
        'include-data-spec': function(node) {
          return dataSpec(node, 's-include-data');
        },
        'include-type-spec': function(node) {
          return typeSpec(node, 's-include-type');
        },
        'include-name-spec': function(node) {
          return nameSpec(node, 's-include-name');
        },
        'include-module-spec': function(node) {
          return moduleSpec(node, 's-include-module');
        },
        'hiding-spec': function(node) {
          return makeListComma(node.kids, 2, node.kids.length - 1, name);
        },
        'module-ref': function(node) {
          return makeListComma(node.kids, 0, node.kids.length, name);
        },
        'name-spec': function(node) {
          if(node.kids[0].name === "STAR" || node.kids[0].name === "TIMES") {
            if(node.kids.length === 1) {
              return RUNTIME.getField(ast, "s-star").app(
                pos(node.pos), makeListTr([]));
            }
            else {
              return RUNTIME.getField(ast, "s-star").app(
                pos(node.pos), tr(node.kids[1]));
            }
          }
          else if(node.kids.length === 1) {
            return RUNTIME.getField(ast, "s-module-ref").app(
              pos(node.pos), tr(node.kids[0]), RUNTIME.ffi.makeNone());
          }
          else {
            return RUNTIME.getField(ast, "s-module-ref").app(
              pos(node.pos),
              tr(node.kids[0]),
              RUNTIME.ffi.makeSome(name(node.kids[2])));
          }
        },
        'provide-spec': function(node) {
          return tr(node.kids[0]);
        },
        'provide-name-spec': function(node) {
          return nameSpec(node, 's-provide-name');
        },
        'provide-data-spec': function(node) {
          return dataSpec(node, 's-provide-data');
        },
        'provide-type-spec': function(node) {
          return typeSpec(node, 's-provide-type');
        },
        'provide-module-spec': function(node) {
          return moduleSpec(node, 's-provide-module');
        },
        'provide-stmt': function(node) {
          return tr(node.kids[0]);
        },
        'provide-block': function(node) {
          var skippedLast = 1;
          if(node.kids[node.kids.length - 2].name === "COMMA") skippedLast = 2;
          if(node.kids[0].name === "PROVIDECOLON") {
            return RUNTIME.getField(ast, "s-provide-block").app(
              pos(node.pos),
              makeListTr([]),
              makeListComma(node.kids, 1, node.kids.length - skippedLast));
          }
          else {
            return RUNTIME.getField(ast, "s-provide-block").app(
              pos(node.pos),
              tr(node.kids[2]),
              makeListComma(node.kids, 4, node.kids.length - skippedLast));
          }
        },
        'provide-vals-stmt': function(node) {
          if (node.kids.length === 2) {
            // (provide-stmt PROVIDE STAR)
            return RUNTIME.getField(ast, 's-provide-all')
              .app(pos(node.pos));
          } else {
            // (provide-stmt PROVIDE stmt END)
            return RUNTIME.getField(ast, 's-provide')
              .app(pos(node.pos), tr(node.kids[1]))
          }
        },
        'provide-types-stmt': function(node) {
          if (node.kids[1].name === "STAR" || node.kids[1].name === "TIMES") {
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
              // (import-stmt IMPORT import-source AS NAME)
              return RUNTIME.getField(ast, 's-import')
                .app(pos(node.pos), tr(node.kids[1]), name(node.kids[3]));
            } else {
              // (import-stmt IMPORT import-source AS NAME, TYPES)
              return RUNTIME.getField(ast, 's-import-types')
                .app(pos(node.pos), tr(node.kids[1]), name(node.kids[3]), name(node.kids[5]));
            }
          } else if (node.kids[0].name === "INCLUDE" && node.kids[1].name === "FROM") {
            var skippedLast = 1;
            if (node.kids[node.kids.length - 2].name === "COMMA") skippedLast++;
            return RUNTIME.getField(ast, 's-include-from').app(pos(node.pos), 
              tr(node.kids[2]),
              makeListComma(node.kids, 4, node.kids.length - skippedLast));
          } else if (node.kids[0].name === "INCLUDE" && node.kids[1].name !== "FROM") {
            // (import-stmt INCLUDE import-source)
            return RUNTIME.getField(ast, 's-include').app(pos(node.pos), tr(node.kids[1]));
          } else {
            // (import-stmt IMPORT comma-names FROM mod)
            return RUNTIME.getField(ast, 's-import-fields')
              .app(pos(node.pos), tr(node.kids[1]), tr(node.kids[3]));
          }
        },
        'import-source': function(node) {
          return tr(node.kids[0]);
        },
        // (import-special NAME LPAREN STRING (COMMA STRING)* RPAREN)
        'import-special': function(node) {
          return RUNTIME.getField(ast, 's-special-import')
            .app(pos(node.pos), symbol(node.kids[0]),
                 makeListComma(node.kids, 2, node.kids.length - 1, string))
        },
        'import-name': function(node) {
          // (import-name NAME)
          return RUNTIME.getField(ast, 's-const-import')
            .app(pos(node.pos), symbol(node.kids[0]))
        },
        'block': function(node) {
          // (block stmts ...)
          detectAndComplainAboutOperatorWhitespace(node.kids, fileName);
          return RUNTIME.getField(ast, 's-block')
            .app(pos(node.pos), makeListTr(node.kids));
        },
        'stmt': function(node) {
          // (stmt s)
          return tr(node.kids[0]);
        },
        'spy-stmt': function(node) {
          // (spy [label] COLON contents END)
          var label, contents;
          if (node.kids[1].name === "binop-expr") {
            label = RUNTIME.ffi.makeSome(tr(node.kids[1]));
          } else {
            label = RUNTIME.ffi.makeNone();
          }
          if (node.kids[node.kids.length - 2].name === "COLON") {
            contents = empty;
          } else {
            contents = tr(node.kids[node.kids.length - 2]);
          }
          return RUNTIME.getField(ast, 's-spy-block')
            .app(pos(node.pos), label, contents);
        },
        'spy-contents': function(node) {
          return makeListComma(node.kids);
        },
        'spy-field': function(node) {
          if (node.kids.length === 1) {
            return RUNTIME.getField(ast, 's-spy-expr')
              .app(pos(node.pos), symbol(node.kids[0].kids[0]), tr(node.kids[0]), RUNTIME.makeBoolean(true));
          } else {
            return RUNTIME.getField(ast, 's-spy-expr')
              .app(pos(node.pos), symbol(node.kids[0]), tr(node.kids[2]), RUNTIME.makeBoolean(false));
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
          return RUNTIME.getField(ast, 's-type')
              .app(pos(node.pos),
                   name(node.kids[1]),
                   tr(node.kids[2]),
                   tr(node.kids[4]));
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
        'rec-expr': function(node) {
          // (rec-expr REC bind EQUALS e)
          return RUNTIME.getField(ast, 's-rec')
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
              .app(pos(node.pos),
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
          return RUNTIME.getField(ast, 's-type-let-expr')
            .app(pos(node.pos),
                 makeListComma(node.kids, 1, node.kids.length - 3),
                 tr(node.kids[node.kids.length - 2]), isBlock);
        },
        'multi-let-expr': function(node) {
          // (multi-let-expr LET let-binding (COMMA let-binding)* COLON block END)
          // Note that we override the normal name dispatch here, because we don't want
          // to create the default let-expr or var-expr constructions
          var isBlock = (node.kids[node.kids.length - 3].name === "BLOCK");
          return RUNTIME.getField(ast, 's-let-expr')
            .app(pos(node.pos),
                 makeListComma(node.kids, 1, node.kids.length - 3, translators["let-binding"]),
                 tr(node.kids[node.kids.length - 2]), isBlock);
        },
        'letrec-expr': function(node) {
          // (letrec-expr LETREC let-expr (COMMA let-expr)* (BLOCK|COLON0 block END)
          // Note that we override the normal name dispatch here, because we don't want
          // to create the default let-expr constructions
          var isBlock = (node.kids[node.kids.length - 3].name === "BLOCK");
          return RUNTIME.getField(ast, 's-letrec')
            .app(pos(node.pos),
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
            return RUNTIME.getField(ast, 's-let-bind')
              .app(pos(node.pos), tr(node.kids[0]), tr(node.kids[2]));
          } else if (node.name === "var-expr") {
            // (var-expr VAR binding EQUALS binop-expr)
            return RUNTIME.getField(ast, 's-var-bind')
              .app(pos(node.pos), tr(node.kids[1]), tr(node.kids[3]));
          }
        },
        'letrec-binding': function(node) {
          // (let-expr binding EQUALS binop-expr)
          return RUNTIME.getField(ast, 's-letrec-bind')
            .app(pos(node.pos), tr(node.kids[0]), tr(node.kids[2]));
        },
        'contract-stmt': function(node) {
          // (contract-stmt NAME COLONCOLON ty-params ann)
          return RUNTIME.getField(ast, 's-contract')
            .app(pos(node.pos), name(node.kids[0]), tr(node.kids[2]), tr(node.kids[3]));
        },
        'fun-header': function(node) {
          // (fun-header ty-params args return-ann)
          if (node.kids[1].name === "bad-args") {
            return {
              lparenPos: pos(node.kids[1].kids[0].pos)
            };
          } else {
            return {
              tyParams: tr(node.kids[0]),
              args: tr(node.kids[1]),
              returnAnn: tr(node.kids[2])
            };
          }
        },
        'fun-expr': function(node) {
          // (fun-expr FUN fun-name fun-header COLON doc body check END)
          var isBlock = (node.kids[3].name === "BLOCK");
          var header = tr(node.kids[2]);
          if (header.lparenPos) {
            RUNTIME.ffi.throwParseErrorBadFunHeader(pos2(node.kids[0].pos, node.kids[3].pos), header.lparenPos);
          }
          var checkRes = tr(node.kids[6]);
          return RUNTIME.getField(ast, 's-fun')
            .app(pos(node.pos), symbol(node.kids[1]),
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
          return RUNTIME.getField(ast, 's-data')
            .app(pos(node.pos), symbol(node.kids[1]), tr(node.kids[2]), empty,
                 makeListTr(node.kids, 4, node.kids.length - 3),
                 tr(node.kids[node.kids.length - 3]),
                 checkRes[0], checkRes[1]);
        },
        'assign-expr': function(node) {
          // (assign-expr id COLONEQUAL e)
          return RUNTIME.getField(ast, 's-assign')
            .app(pos(node.pos), name(node.kids[0]), tr(node.kids[2]));
        },
        'when-expr': function(node) {
          // (when-expr WHEN test COLON body END)
          var isBlock = (node.kids[2].name === "BLOCK");
          return RUNTIME.getField(ast, 's-when')
            .app(pos(node.pos), tr(node.kids[1]), tr(node.kids[3]), isBlock);
        },
        'check-expr': function(node) {
          if (node.kids.length === 3) {
            // (check-expr CHECKCOLON body END)
            return RUNTIME.getField(ast, 's-check')
              .app(pos(node.pos), RUNTIME.ffi.makeNone(), tr(node.kids[1]),
                   RUNTIME.makeBoolean(node.kids[0].name === "CHECKCOLON"));
          } else {
            // (check-expr CHECK STRING COLON body END)
            return RUNTIME.getField(ast, 's-check')
              .app(pos(node.pos), RUNTIME.ffi.makeSome(string(node.kids[1])), tr(node.kids[3]),
                   RUNTIME.makeBoolean(node.kids[0].name === "CHECK"));
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
            return RUNTIME.getField(ast, 's-check-test')
              .app(pos(node.pos), tr(kids[1]), RUNTIME.ffi.makeNone(), tr(kids[0]), RUNTIME.ffi.makeNone(), RUNTIME.ffi.makeNone());
          } else {
            var refinement, right, because;
            if (kids[2].name === "PERCENT") {
              // (check-test left op PERCENT LPAREN refinement RPAREN right ...)
              //             0    1                 4                 6
              refinement = RUNTIME.ffi.makeSome(tr(kids[4]));
              right = RUNTIME.ffi.makeSome(tr(kids[6]));
            } else if (kids[2].name === "BECAUSE") {
              // (check-test left does-not-raise because ...)
              refinement = RUNTIME.ffi.makeNone();
              right = RUNTIME.ffi.makeNone();
            } else {
              // (check-test left op right ...)
              //             0    1  2
              refinement = RUNTIME.ffi.makeNone();
              right = RUNTIME.ffi.makeSome(tr(kids[2]));
            }
            if (kids[kids.length - 2].name === "BECAUSE") {
              // (check-test ... right BECAUSE cause)
              //                       len-2   len-1
              because = RUNTIME.ffi.makeSome(tr(kids[kids.length - 1]));
            } else {
              because = RUNTIME.ffi.makeNone();
            }
            return RUNTIME.getField(ast, 's-check-test')
              .app(pos(node.pos), tr(kids[1]), refinement, tr(kids[0]), right, because);
          }
        },
        'binop-expr': function(node) {
          if (node.kids.length === 1) {
            // (binop-expr e)
            return tr(node.kids[0]);
          } else {
            var mkOp = RUNTIME.getField(ast, 's-op').app;
            var expr = mkOp(pos2(node.kids[0].pos, node.kids[2].pos),
                            pos(node.kids[1].pos),
                            tr(node.kids[1]),
                            tr(node.kids[0]),
                            tr(node.kids[2]));
            for(var i = 4; i < node.kids.length; i += 2) {
              expr = mkOp(pos2(node.kids[0].pos, node.kids[i].pos),
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
            return RUNTIME.makeString("");
          } else {
            // (doc-string DOC str)
            return string(node.kids[1]);
          }
        },
        'where-clause': function(node) {
          if (node.kids.length === 0) {
            // (where-clause)
            return [RUNTIME.ffi.makeNone(), RUNTIME.ffi.makeNone()];
          } else {
            // (where-clause WHERE block)
            return [RUNTIME.ffi.makeSome(makePyretPos(fileName, node.kids[0].pos)),
                    RUNTIME.ffi.makeSome(tr(node.kids[1]))];
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
          return RUNTIME.getField(ast, 's-template').app(pos(node.pos));
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
          return tr(node.kids[0]);
        },

        'tuple-binding' : function(node) {
          var lastBinding = node.kids.length - 1;
          var optAsBinding;
          if (node.kids[lastBinding - 1].name === "AS") {
            optAsBinding = RUNTIME.ffi.makeSome(tr(node.kids[lastBinding]));
            lastBinding -= 2;
          } else {
            optAsBinding = RUNTIME.ffi.makeNone();
          }
          if (node.kids[lastBinding - 1].name === "SEMI") {
            lastBinding--;
          }
          return RUNTIME.getField(ast, 's-tuple-bind')
            .app(pos(node.pos), makeListComma(node.kids, 1, lastBinding), optAsBinding);
        },

        'name-binding': function(node) {
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
            // (args LPAREN binding (COMMA binding)* RPAREN)
            return makeListComma(node.kids, 1, node.kids.length - 1);
          }
        },
        'variant-member': function(node) {
          if (node.kids.length === 1) {
            // (variant-member b)
            return RUNTIME.getField(ast, 's-variant-member')
              .app(pos(node.pos), RUNTIME.getField(ast, "s-normal"), tr(node.kids[0]));
          } else {
            return RUNTIME.getField(ast, 's-variant-member')
              .app(pos(node.pos), RUNTIME.getField(ast, "s-mutable"), tr(node.kids[1]));
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
            return RUNTIME.getField(ast, 's-mutable-field')
              .app(pos(node.pos), tr(node.kids[1]), RUNTIME.getField(ast, 'a-blank'), tr(node.kids[3]));
          } else if (node.kids.length === 6) {
            // (obj-field MUTABLE key COLONCOLON ann COLON value)
            return RUNTIME.getField(ast, 's-mutable-field')
              .app(pos(node.pos), tr(node.kids[1]), tr(node.kids[3]), tr(node.kids[5]));
          } else if (node.kids.length === 3) {
            // (obj-field key COLON value)
            return RUNTIME.getField(ast, 's-data-field')
              .app(pos(node.pos), tr(node.kids[0]), tr(node.kids[2]));
          } else {
            // (obj-field METHOD key fun-header COLON doc body check END)
            var isBlock = (node.kids[3].name === "BLOCK");
            var header = tr(node.kids[2]);
            if (header.lparenPos) {
              RUNTIME.ffi.throwParseErrorBadFunHeader(pos2(node.kids[0].pos, node.kids[3].pos), header.lparenPos);
            }
            var checkRes = tr(node.kids[6])
            return RUNTIME.getField(ast, 's-method-field')
              .app(pos(node.pos), tr(node.kids[1]), header.tyParams, header.args, header.returnAnn,
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
          return RUNTIME.getField(ast, 's-reactor')
            .app(pos(node.pos), tr(node.kids[2]));
        },
        'table-expr': function(node) {
          // (TABLE table-headers table-rows end)
          return RUNTIME.getField(ast, 's-table')
            .app(pos(node.pos), tr(node.kids[1]), tr(node.kids[2]));
        },
        'load-table-expr': function(node) {
          // (LOAD-TABLE COLON table-headers load-table-specs END)
          return RUNTIME.getField(ast, 's-load-table')
            .app(pos(node.pos), tr(node.kids[2]),
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
            return RUNTIME.getField(ast, 's-field-name')
              .app(pos(node.pos), symbol(node.kids[0]), tr(node.kids[2]))
          } else {
            return RUNTIME.getField(ast, 's-field-name')
              .app(pos(node.pos), symbol(node.kids[0]), RUNTIME.getField(ast, 'a-blank'))
          }
        },
        'table-rows': function(node) {
          // [table-row* table-row]
          return makeList(node.kids.map(tr));
        },
        'table-row': function(node) {
          // (ROW table-items)
          return RUNTIME.getField(ast, 's-table-row')
            .app(pos(node.pos), tr(node.kids[1]));
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
            return RUNTIME.getField(ast, 's-table-extend-field')
              .app(pos(node.pos), tr(node.kids[0]), tr(node.kids[2]),
                   RUNTIME.getField(ast, 'a-blank'));
          } else if ((node.kids.length === 5)
                     && (node.kids[1].name === "COLONCOLON")){
            // (key COLONCOLON ann COLON binop-expr)
            return RUNTIME.getField(ast, 's-table-extend-field')
              .app(pos(node.pos), tr(node.kids[0]), tr(node.kids[4]),
                   tr(node.kids[2]));
          } else if (node.kids.length === 5) {
            // (key COLON expr OF NAME)
            return RUNTIME.getField(ast, 's-table-extend-reducer')
              .app(pos(node.pos), tr(node.kids[0]), tr(node.kids[2]),
                   name(node.kids[4]), RUNTIME.getField(ast, 'a-blank'));
          } else if (node.kids.length === 7) {
            // (key COLONCOLON ann COLON expr OF NAME)
            return RUNTIME.getField(ast, 's-table-extend-reducer')
              .app(pos(node.pos), tr(node.kids[0]), tr(node.kids[4]),
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
            return RUNTIME.getField(ast, 's-sanitize')
              .app(pos(node.pos), name(node.kids[1]), tr(node.kids[3]));
          } else {
            // (SOURCECOLON expr)
            return RUNTIME.getField(ast, 's-table-src')
              .app(pos(node.pos), tr(node.kids[1]));
          }
        },
        'sql-expr': function(node) {
          var inspect = tr(node.kids[1]);
          var where = node.kids.length == 5
                ? RUNTIME.ffi.makeNone()
                : RUNTIME.ffi.makeSome(tr(node.kids[3]));
          var project = tr(node.kids[node.kids.length - 2]);
          return RUNTIME.getField(ast, 's-sql')
            .app( pos(node.pos),
                  inspect, // from
                  where, // where
                  project); // project
        },
        'do-expr': function(node) {
          // (do FOR iter binds ... return COLON body END)
          return RUNTIME.getField(ast, 's-do')
            .app(pos(node.pos),
                 tr(node.kids[1]),                         // iterator
                 makeList(node.kids.slice(3, -4).map(tr)), // bindings
                 tr(node.kids[node.kids.length - 3]),      // return-ann
                 tr(node.kids[node.kids.length - 1]));     // body
        },
        'for-then': function(node) {
          // (for-then FOR iter LPAREN binds ... RPAREN return COLON body)
          return RUNTIME.getField(ast, 's-for')
            .app(pos(node.pos), tr(node.kids[1]), makeList(node.kids.slice(3, -4).map(tr)),
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
            return RUNTIME.getField(ast, "s-data-field")
              .app(pos(node.pos), tr(node.kids[0]), tr(node.kids[2]));
          } else {
            // (field METHOD key fun-header (BLOCK|COLON) doc body check END)
            var isBlock = (node.kids[3].name === "BLOCK");
            var header = tr(node.kids[2]);
            if (header.lparenPos) {
              RUNTIME.ffi.throwParseErrorBadFunHeader(pos2(node.kids[0].pos, node.kids[3].pos), header.lparenPos);
            }
            var checkRes = tr(node.kids[6])
            return RUNTIME.getField(ast, "s-method-field")
              .app(pos(node.pos), tr(node.kids[1]), header.tyParams, header.args, header.returnAnn,
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
            return empty;
          } else {
            return tr(node.kids[0]);
          }
        },
        'comma-binops': function(node) {
          return makeListComma(node.kids);
        },
        'trailing-opt-comma-binops': function(node) {
          if (node.kids.length === 0) {
            return empty;
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
            return RUNTIME.getField(ast, 's-cases-bind')
              .app(pos(node.pos), RUNTIME.getField(ast, 's-cases-bind-ref'), tr(node.kids[1]));
          }
          else {
            return RUNTIME.getField(ast, 's-cases-bind')
              .app(pos(node.pos), RUNTIME.getField(ast, 's-cases-bind-normal'), tr(node.kids[0]));
          }
        },
        'cases-branch': function(node) {
          if (node.kids.length === 4) {
            // (singleton-cases-branch PIPE NAME THICKARROW body)
            return RUNTIME.getField(ast, 's-singleton-cases-branch')
              .app(pos(node.pos), pos(node.kids[1].pos), symbol(node.kids[1]), tr(node.kids[3]));
          } else {
            // (cases-branch PIPE NAME args THICKARROW body)
            return RUNTIME.getField(ast, 's-cases-branch')
              .app(pos(node.pos), pos(node.kids[1].pos.combine(node.kids[2].pos)),
                   symbol(node.kids[1]), tr(node.kids[2]), tr(node.kids[4]));
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
            // (ty-params LANGLE comma-names RANGLE)
            return tr(node.kids[1]);
          }
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
        'tuple-expr': function(node) {
          return RUNTIME.getField(ast, 's-tuple')
              .app(pos(node.pos), tr(node.kids[1]))
        },
        'tuple-get': function(node) {
          return RUNTIME.getField(ast, 's-tuple-get')
              .app(pos(node.pos), tr(node.kids[0]), number(node.kids[3]), pos(node.kids[3].pos))
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
        'construct-expr': function(node) {
          // LBRACK construct-modifier binop-expr COLON trailing-opt-comma-binops RBRACK
          return RUNTIME.getField(ast, 's-construct')
            .app(pos(node.pos), tr(node.kids[1]), tr(node.kids[2]), tr(node.kids[4]));
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
          if (node.kids.length > 2) {
            RUNTIME.ffi.throwParseErrorBadApp(pos(node.kids[0].pos),
                                              pos2(node.kids[1].pos, node.kids[node.kids.length - 1].pos));
          } else {
            // (app-expr f args)
            return RUNTIME.getField(ast, 's-app')
              .app(pos(node.pos), tr(node.kids[0]), tr(node.kids[1]));
          }
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
          // (bracket-expr obj LBRACK field RBRACK)
          return RUNTIME.getField(ast, 's-bracket')
            .app(pos(node.pos), tr(node.kids[0]), tr(node.kids[2]));
        },
        'cases-expr': function(node) {
          var isBlock = (node.kids[5].name === "BLOCK");
          if (node.kids[node.kids.length - 4].name === "ELSE") {
            // (cases-expr CASES LPAREN type RPAREN val COLON branch ... PIPE ELSE THICKARROW elseblock END)
            return RUNTIME.getField(ast, 's-cases-else')
              .app(pos(node.pos), tr(node.kids[2]), tr(node.kids[4]),
                   makeListTr(node.kids, 6, node.kids.length - 5), tr(node.kids[node.kids.length - 2]), isBlock);
          } else {
            // (cases-expr CASES LPAREN type RPAREN val COLON branch ... END)
            return RUNTIME.getField(ast, 's-cases')
              .app(pos(node.pos), tr(node.kids[2]), tr(node.kids[4]),
                   makeListTr(node.kids, 6, node.kids.length - 1), isBlock);
          }
        },
        'if-pipe-expr': function(node) {
          var isBlock = (node.kids[1].name === "BLOCK");
          if (node.kids[node.kids.length - 3].name === "OTHERWISECOLON") {
            // (if-pipe-expr ASK (BLOCK|COLON) branch ... BAR OTHERWISECOLON else END)
            return RUNTIME.getField(ast, 's-if-pipe-else')
              .app(pos(node.pos), makeListTr(node.kids, 2, node.kids.length - 4),
                   tr(node.kids[node.kids.length - 2]), isBlock);
          } else {
            // (if-pipe-expr ASK (BLOCK|COLON) branch ... END)
            return RUNTIME.getField(ast, 's-if-pipe')
              .app(pos(node.pos), makeListTr(node.kids, 2, node.kids.length - 1), isBlock);
          }
        },
        'if-expr': function(node) {
          var isBlock = (node.kids[2].name === "BLOCK");
          if (node.kids[node.kids.length - 3].name === "ELSECOLON") {
            // (if-expr IF test (BLOCK|COLON) body branch ... ELSECOLON else END)
            return RUNTIME.getField(ast, 's-if-else')
              .app(pos(node.pos),
                   makeList([RUNTIME.getField(ast, 's-if-branch')
                             .app(pos2(node.kids[1].pos, node.kids[3].pos), tr(node.kids[1]), tr(node.kids[3]))],
                            0, 1,
                            makeListTr(node.kids, 4, node.kids.length - 3)),
                   tr(node.kids[node.kids.length - 2]), isBlock);
          } else {
            // (if-expr IF test (BLOCK|COLON) body branch ... END)
            return RUNTIME.getField(ast, 's-if')
              .app(pos(node.pos),
                   makeList([RUNTIME.getField(ast, 's-if-branch')
                             .app(pos2(node.kids[1].pos, node.kids[3].pos), tr(node.kids[1]), tr(node.kids[3]))],
                            0, 1,
                            makeListTr(node.kids, 4, node.kids.length - 1)), isBlock);
          }
        },
        'for-expr': function(node) {
          // (for-expr FOR iter LPAREN for-bind (COMMA for-bind)* RPAREN return (BLOCK|COLON) body END)
          var isBlock = (node.kids[node.kids.length - 3].name === "BLOCK");
          return RUNTIME.getField(ast, 's-for')
            .app(pos(node.pos), tr(node.kids[1]), makeListComma(node.kids, 3, node.kids.length - 5),
                 tr(node.kids[node.kids.length - 4]), tr(node.kids[node.kids.length - 2]), isBlock);
        },
        'user-block-expr': function(node) {
          // (user-block-expr BLOCK body END)
          return RUNTIME.getField(ast, 's-user-block')
            .app(pos(node.pos), tr(node.kids[1]));
        },
        'lambda-expr': function(node) {
          // (lambda-expr LAM fun-header COLON doc body check END)
          var isBlock = (node.kids[2].name === "BLOCK");
          var header = tr(node.kids[1]);
          if (header.lparenPos) {
            RUNTIME.ffi.throwParseErrorBadFunHeader(pos2(node.kids[0].pos, node.kids[2].pos), header.lparenPos);
          }
          var checkRes = tr(node.kids[5]);
          return RUNTIME.getField(ast, 's-lam')
            .app(pos(node.pos), RUNTIME.makeString(""), header.tyParams, header.args, header.returnAnn,
                 tr(node.kids[3]), tr(node.kids[4]), checkRes[0], checkRes[1], isBlock);
        },
        'method-expr': function(node) {
          // (method-expr METHOD fun-header COLON doc body check END)
          var isBlock = (node.kids[2].name === "BLOCK");
          var header = tr(node.kids[1]);
          if (header.lparenPos) {
            RUNTIME.ffi.throwParseErrorBadFunHeader(pos2(node.kids[0].pos, node.kids[2].pos), header.lparenPos);
          }
          var checkRes = tr(node.kids[5]);
          return RUNTIME.getField(ast, 's-method')
            .app(pos(node.pos), RUNTIME.makeString(""), header.tyParams, header.args, header.returnAnn,
                 tr(node.kids[3]), tr(node.kids[4]), checkRes[0], checkRes[1], isBlock);
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
          // (inst-expr e LANGLE ann (COMMA ann)* RANGLE)
          return RUNTIME.getField(ast, 's-instantiate')
            .app(pos(node.pos), tr(node.kids[0]), makeListComma(node.kids, 2, node.kids.length - 1));
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
        'rfrac-expr': function(node) {
          // (rfrac-expr n)
          var numden = node.kids[0].value.substring(1).split("/");
          return RUNTIME.getField(ast, 's-rfrac')
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
            return RUNTIME.getField(ast, 'a-any').app(pos(node.pos));
          } else {
            return RUNTIME.getField(ast, 'a-name')
              .app(pos(node.pos), name(node.kids[0]));
          }
        },
        'comma-ann-field': function(node) {
          return makeListComma(node.kids);
        },
        'trailing-opt-comma-ann-field': function(node) {
          if (node.kids.length === 0) {
            return empty;
          } else {
            return tr(node.kids[0]);
          }
        },
        'record-ann': function(node) {
          // (record-ann LBRACE ann-field (COMMA ann-field)* RBRACE)
          return RUNTIME.getField(ast, 'a-record')
            .app(pos(node.pos), tr(node.kids[1]));
        },
        'tuple-ann': function(node) {
          // (tuple LBRACE ann (SEMI ann)* [SEMI] RBRACE
          if (node.kids[node.kids.length - 2].name === "SEMI") {
            return RUNTIME.getField(ast, 'a-tuple')
              .app(pos(node.pos), makeListComma(node.kids, 1, node.kids.length - 2));
          } else {
            return RUNTIME.getField(ast, 'a-tuple')
              .app(pos(node.pos), makeListComma(node.kids, 0, node.kids.length - 1));
          }
        },
        'noparen-arrow-ann': function(node) {
          if (node.kids.length === 2) {
            // (noparen-arrow-ann THINARROW result)
            return RUNTIME.getField(ast, 'a-arrow')
              .app(pos(node.pos),
                   empty, tr(node.kids[1]),
                   RUNTIME.pyretFalse);
          } else {
            // (noparen-arrow-ann arrow-ann-args THINARROW result)
            var trArgs = tr(node.kids[0]);
            if (trArgs.named) {
              return RUNTIME.getField(ast, 'a-arrow-argnames')
                .app(pos(node.pos), trArgs.args, tr(node.kids[2]), RUNTIME.pyretFalse);
            } else {
              return RUNTIME.getField(ast, 'a-arrow')
                .app(pos(node.pos), trArgs.args, tr(node.kids[2]), RUNTIME.pyretFalse);
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
          return RUNTIME.getField(ast, 's-table-extend').app(pos(node.pos),
                                                             RUNTIME.getField(ast, 's-column-binds').app(
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
          return RUNTIME.getField(ast, 's-table-update').app(pos(node.pos),
                                                             RUNTIME.getField(ast, 's-column-binds').app(
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
          return RUNTIME.getField(ast, 's-table-select').app(
            pos(node.pos), makeList(columns), table);
        },
        'column-order': function(node) {
          var column = name(node.kids[0]);
          var direction = node.kids[1].name == "ASCENDING"  ? RUNTIME.getField(ast, 'ASCENDING')
                : node.kids[1].name == "DESCENDING" ? RUNTIME.getField(ast, 'DESCENDING')
                : undefined;
          return RUNTIME.getField(ast, 's-column-sort').app(pos(node.pos),
                                                            column,
                                                            direction);
        },
        'table-order': function(node) {
          // TABLE-ORDER NAME COLON column-orderings end
          return RUNTIME.getField(ast, 's-table-order').app(pos(node.pos),
                                                            tr(node.kids[1]),
                                                            makeListComma(node.kids, 3, node.kids.length - 1, tr));
        },
        'table-filter': function(node) {
          var columns = new Array();
          for (var i = 3; i < node.kids.length - 3; i+=2)
            columns.push(tr(node.kids[i]));
          var table = tr(node.kids[1]);
          var predicate = tr(node.kids[node.kids.length - 2]);
          return RUNTIME.getField(ast, 's-table-filter').app(pos(node.pos),
                                                             RUNTIME.getField(ast, 's-column-binds').app(
                                                               combinePyretPos(fileName, node.kids[1].pos, node.kids[node.kids.length - 4].pos),
                                                               makeList(columns),
                                                               table),
                                                             predicate);
        },
        'table-extract': function(node) {
          return RUNTIME.getField(ast, 's-table-extract').app(pos(node.pos),
                                                              name(node.kids[1]), tr(node.kids[3]));
        },
        'arrow-ann': function(node) {
          if (node.kids.length === 4) {
            // (arrow-ann LPAREN THINARROW result RPAREN)
            return RUNTIME.getField(ast, 'a-arrow')
              .app(pos(node.pos), empty,
                   tr(node.kids[2]),
                   RUNTIME.pyretTrue);
          } else {
            // (arrow-ann LPAREN arrow-ann-args THINARROW result RPAREN)
            // (noparen-arrow-ann arrow-ann-args THINARROW result)
            var trArgs = tr(node.kids[1]);
            if (trArgs.named) {
              return RUNTIME.getField(ast, 'a-arrow-argnames')
                .app(pos(node.pos), trArgs.args, tr(node.kids[3]), RUNTIME.pyretTrue);
            } else {
              return RUNTIME.getField(ast, 'a-arrow')
                .app(pos(node.pos), trArgs.args, tr(node.kids[3]), RUNTIME.pyretTrue);
            }
          }
        },
        'app-ann': function(node) {
          // (app-ann ann LANGLE comma-anns RANGLE)
          return RUNTIME.getField(ast, 'a-app')
            .app(pos(node.pos), tr(node.kids[0]), tr(node.kids[2]));
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
        }
      };
      return tr(node);
    }

    const opLookup = {
      "+":   RUNTIME.makeString("op+"),
      "-":   RUNTIME.makeString("op-"),
      "*":   RUNTIME.makeString("op*"),
      "/":   RUNTIME.makeString("op/"),
      "$":   RUNTIME.makeString("op^"),
      "^":   RUNTIME.makeString("op^"),
      "<=":  RUNTIME.makeString("op<="),
      "<":   RUNTIME.makeString("op<"),
      ">=":  RUNTIME.makeString("op>="),
      ">":   RUNTIME.makeString("op>"),
      "==":  RUNTIME.makeString("op=="),
      "=~":  RUNTIME.makeString("op=~"),
      "<=>": RUNTIME.makeString("op<=>"),
      "<>":  RUNTIME.makeString("op<>"),
      "and": RUNTIME.makeString("opand"),
      "or":  RUNTIME.makeString("opor"),

      "is":                function(l){return RUNTIME.getField(ast, "s-op-is").app(l);},
      "is-roughly":        function(l){return RUNTIME.getField(ast, "s-op-is-roughly").app(l);},
      "is==":              function(l){return RUNTIME.getField(ast, "s-op-is-op").app(l, "op==");},
      "is=~":              function(l){return RUNTIME.getField(ast, "s-op-is-op").app(l, "op=~");},
      "is<=>":             function(l){return RUNTIME.getField(ast, "s-op-is-op").app(l, "op<=>");},
      "is-not":            function(l){return RUNTIME.getField(ast, "s-op-is-not").app(l);},
      "is-not-roughly":    function(l){return RUNTIME.getField(ast, "s-op-is-not-roughly").app(l);},
      "is-not==":          function(l){return RUNTIME.getField(ast, "s-op-is-not-op").app(l, "op==");},
      "is-not=~":          function(l){return RUNTIME.getField(ast, "s-op-is-not-op").app(l, "op=~");},
      "is-not<=>":         function(l){return RUNTIME.getField(ast, "s-op-is-not-op").app(l, "op<=>");},
      "satisfies":         function(l){return RUNTIME.getField(ast, "s-op-satisfies").app(l);},
      "violates":          function(l){return RUNTIME.getField(ast, "s-op-satisfies-not").app(l);},
      "raises":            function(l){return RUNTIME.getField(ast, "s-op-raises").app(l);},
      "raises-other-than": function(l){return RUNTIME.getField(ast, "s-op-raises-other").app(l);},
      "does-not-raise":    function(l){return RUNTIME.getField(ast, "s-op-raises-not").app(l);},
      "raises-satisfies":  function(l){return RUNTIME.getField(ast, "s-op-raises-satisfies").app(l);},
      "raises-violates":   function(l){return RUNTIME.getField(ast, "s-op-raises-violates").app(l);},
    }

    function parseDataRaw(data, fileName) {
      var message = "";
      try {
        const toks = tokenizer.Tokenizer;
        const grammar = parser.PyretGrammar;
        toks.tokenizeFrom(data);
        // while (toks.hasNext())
        //   console.log(toks.next().toString(true));
        var parsed = grammar.parse(toks);
        //console.log("Result:");
        var countParses = grammar.countAllParses(parsed);
        if (countParses == 0) {
          var nextTok = toks.curTok;
          message = "There were " + countParses + " potential parses.\n" +
                      "Parse failed, next token is " + nextTok.toRepr(true) +
                      " at " + fileName + ", " + nextTok.pos.toString(true);
          if (toks.isEOF(nextTok))
            RUNTIME.ffi.throwParseErrorEOF(makePyretPos(fileName, nextTok.pos));
          else if (nextTok.name === "UNTERMINATED-STRING")
            RUNTIME.ffi.throwParseErrorUnterminatedString(makePyretPos(fileName, nextTok.pos));
          else if (nextTok.name === "BAD-NUMBER")
            RUNTIME.ffi.throwParseErrorBadNumber(makePyretPos(fileName, nextTok.pos));
          else if (nextTok.name === "BAD-OPER" || nextTok.name === "STAR")
            RUNTIME.ffi.throwParseErrorBadOper(makePyretPos(fileName, nextTok.pos));
          else if (nextTok.name === "COLONCOLON")
            RUNTIME.ffi.throwParseErrorColonColon(makePyretPos(fileName, nextTok.pos));
          else if (typeof opLookup[String(nextTok.value).trim()] === "function")
            RUNTIME.ffi.throwParseErrorBadCheckOper(opLookup[String(nextTok.value).trim()](makePyretPos(fileName, nextTok.pos)));
          else
            RUNTIME.ffi.throwParseErrorNextToken(makePyretPos(fileName, nextTok.pos), nextTok.value || nextTok.toString(true));
        }
        //console.log("There were " + countParses + " potential parses");
        if (countParses === 1) {
          var ast = grammar.constructUniqueParse(parsed);
          //          console.log(ast.toString());
          return RUNTIME.ffi.makeRight(translate(ast, fileName));
        } else {
          var asts = grammar.constructAllParses(parsed);
          throw "Non-unique parse";
          for (var i = 0; i < asts.length; i++) {
            //console.log("Parse " + i + ": " + asts[i].toString());
            //            console.log(("" + asts[i]) === ("" + asts2[i]));
          }
          return RUNTIME.ffi.makeRight(translate(ast, fileName));
        }
      } catch(e) {
        if (RUNTIME.isPyretException(e)) {
          return RUNTIME.ffi.makeLeft(RUNTIME.makeObject({
            exn: e.exn,
            message: RUNTIME.makeString(message)
          }));
        } else {
          throw e;
        }
      }
    }

    function parsePyret(data, fileName) {
      RUNTIME.ffi.checkArity(2, arguments, "surface-parse", false);
      RUNTIME.checkString(data);
      RUNTIME.checkString(fileName);
      var result = parseDataRaw(RUNTIME.unwrap(data), RUNTIME.unwrap(fileName));
      return RUNTIME.ffi.cases(RUNTIME.ffi.isEither, "is-Either", result, {
        left: function(err) {
          var exn = RUNTIME.getField(err, "exn");
          var message = RUNTIME.getField(err, "message");
          console.error(message);
          RUNTIME.raise(exn);
        },
        right: function(ast) {
          return ast;
        }
      });
    }

    function maybeParsePyret(data, fileName) {
      RUNTIME.ffi.checkArity(2, arguments, "maybe-surface-parse", false);
      RUNTIME.checkString(data);
      RUNTIME.checkString(fileName);
      return parseDataRaw(RUNTIME.unwrap(data), RUNTIME.unwrap(fileName));
    }

    return RUNTIME.makeModuleReturn({
          'surface-parse': RUNTIME.makeFunction(parsePyret, "surface-parse"),
          'maybe-surface-parse': RUNTIME.makeFunction(maybeParsePyret, "maybe-surface-parse"),
        }, {});
  }
})
