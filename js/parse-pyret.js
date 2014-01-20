define(["./pyret-tokenizer", "./pyret-parser", "./ast", "fs"], function(T, G, ast, fs) {
  return function(RUNTIME, NAMESPACE) {
    //var data = "#lang pyret\n\nif (f(x) and g(y) and h(z) and i(w) and j(u)): true else: false end";
    function translate(node, fileName) {
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
      function number(tok) { return RUNTIME.makeNumberFromString(tok.value); }
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
        },
        'variant-members': function(node) {
        },
        'key': function(node) {
        },
        'obj-field': function(node) {
        },
        'obj-fields': function(node) {
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
        },
        'mixins': function(node) {
        },
        'app-args': function(node) {
        },
        'cases-branch': function(node) {
        },
        'else-if': function(node) {
        },
        'else': function(node) {
        },
        'ty-params': function(node) {
        },
        'left-app-fun-expr': function(node) {
        },
        'for-bind': function(node) {
        },
        'prim-expr': function(node) {
        },
        'obj-expr': function(node) {
        },
        'list-expr': function(node) {
        },
        'app-expr': function(node) {
        },
        'id-expr': function(node) {
        },
        'dot-expr': function(node) {
        },
        'get-bang-expr': function(node) {
        },
        'bracket-expr': function(node) {
        },
        'colon-expr': function(node) {
        },
        'colon-bracket-expr': function(node) {
        },
        'cases-expr': function(node) {
        },
        'if-expr': function(node) {
        },
        'for-expr': function(node) {
        },
        'try-expr': function(node) {
        },
        'user-block-expr': function(node) {
        },
        'lambda-expr': function(node) {
        },
        'method-expr': function(node) {
        },
        'extend-expr': function(node) {
        },
        'update-expr': function(node) {
        },
        'left-app-expr': function(node) {
        },
        'paren-expr': function(node) {
        },
        'paren-nospace-expr': function(node) {
        },
        'inst-expr': function(node) {
        },
        'bool-expr': function(node) {
        },
        'num-expr': function(node) {
        },
        'string-expr': function(node) {
        },
        'ann-field': function(node) {
        },
        'name-ann': function(node) {
        },
        'record-ann': function(node) {
        },
        'arrow-ann': function(node) {
        },
        'app-ann': function(node) {
        },
        'pred-ann': function(node) {
        },
        'dot-ann': function(node) {
        },
        'ann': function(node) {
        },
        'list-ann-field': function(node) {
        },
        'arrow-ann-elt': function(node) {
        },
        'app-ann-elt': function(node) {
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



