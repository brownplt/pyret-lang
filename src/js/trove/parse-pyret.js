({
  requires: [
    { "import-type": "builtin", name: "srcloc" },
    { "import-type": "builtin", name: "ast" },
    { "import-type": "builtin", name: "lists" }
  ],
  nativeRequires: [
    "pyret-base/js/pyret-tokenizer",
    "pyret-base/js/pyret-parser",
    "pyret-base/js/translate-parse-tree"
  ],
  provides: {},
  theModule: function(RUNTIME, NAMESPACE, uri, srclocLib, astLib, listsLib, tokenizer, parser, translator) {
    var srcloc = RUNTIME.getField(srclocLib, "values");
    var ast = RUNTIME.getField(astLib, "values");
    var lists = RUNTIME.getField(listsLib, "values");

    var link = RUNTIME.getField(lists, "link");
    var empty = RUNTIME.getField(lists, "empty");

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
    function combinePyretPos(fileName, p1, p2) {
      var n = RUNTIME.makeNumber;
      return RUNTIME.getField(srcloc, "srcloc").app(
        RUNTIME.makeString(fileName),
        n(p1.startRow),
        n(p1.startCol),
        n(p1.startChar),
        n(p2.endRow),
        n(p2.endCol),
        n(p2.endChar)
      );
    }
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
    // A heuristic for better parse errors around some binop exprs.
    // See: https://github.com/brownplt/pyret-lang/commit/45a7efa5eae80846a04d218ccc0379a4c67c4031
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

    const binops = {
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
      "or":  RUNTIME.makeString("opor")
    };

    function makeCheckop1(opname) {
      return function(l) {
        return RUNTIME.getField(ast, opname).app(l);
      }
    }
    function makeCheckop2(opname, eqname) {
      return function(l) {
        return RUNTIME.getField(ast, opname).app(l, eqname);
      }
    }
    const checkops = {
      "is":                makeCheckop1("s-op-is"),
      "is":                makeCheckop1("s-op-is"),
      "is-roughly":        makeCheckop1("s-op-is-roughly"),
      "is==":              makeCheckop2("s-op-is-op", "op=="),
      "is=~":              makeCheckop2("s-op-is-op", "op=~"),
      "is<=>":             makeCheckop2("s-op-is-op", "op<=>"),
      "is-not":            makeCheckop1("s-op-is-not"),
      "is-not==":          makeCheckop2("s-op-is-not-op", "op=="),
      "is-not=~":          makeCheckop2("s-op-is-not-op", "op=~"),
      "is-not<=>":         makeCheckop2("s-op-is-not-op", "op<=>"),
      "satisfies":         makeCheckop1("s-op-satisfies"),
      "violates":          makeCheckop1("s-op-satisfies-not"),
      "raises":            makeCheckop1("s-op-raises"),
      "raises-other-than": makeCheckop1("s-op-raises-other"),
      "does-not-raise":    makeCheckop1("s-op-raises-not"),
      "raises-satisfies":  makeCheckop1("s-op-raises-satisfies"),
      "raises-violates":   makeCheckop1("s-op-raises-violates")
    }

    function translate(node, fileName) {
      let constructors = {
        "makeNode": function(con) {
          let args = Array.prototype.slice.call(arguments, 1);
          if (args.length === 0) {
            return RUNTIME.getField(ast, con);
          } else {
            return RUNTIME.getField(ast, con).app.apply(this, args);
          }
        },
        "binops": binops,
        "checkops": checkops,
        "makeLink": function(head, tail) {
          return RUNTIME.getField(lists, "link").app(head, tail);
        },
        "makeEmpty": function() {
          return RUNTIME.getField(lists, "empty");
        },
        "makeString": function(str) {
          return RUNTIME.makeString(str);
        },
        "makeNumberFromString": function(str) {
          return RUNTIME.makeNumberFromString(str);
        },
        "makeBoolean": function(bool) {
          return RUNTIME.makeBoolean(bool);
        },
        "makeNone": function() {
          return RUNTIME.ffi.makeNone();
        },
        "makeSome": function(val) {
          return RUNTIME.ffi.makeSome(val);
        },
        "getRecordFields": function(record) {
          return RUNTIME.getField(record, 'fields');
        },
        "makeSrcloc": makePyretPos,
        "combineSrcloc": combinePyretPos,
        "detectAndComplainAboutOperatorWhitespace": detectAndComplainAboutOperatorWhitespace
      };
      return translator.translate(node, fileName, constructors);
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
          else if (nextTok.name === "BAD-OPER")
            RUNTIME.ffi.throwParseErrorBadOper(makePyretPos(fileName, nextTok.pos));
          else if (nextTok.name === "COLONCOLON")
            RUNTIME.ffi.throwParseErrorColonColon(makePyretPos(fileName, nextTok.pos));
          else if (typeof checkops[String(nextTok.value).trim()] === "function")
            RUNTIME.ffi.throwParseErrorBadCheckOper(makePyretPos(fileName, nextTok.pos));
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
