define("pyret-base/js/pyret-tokenizer", ["jglr/jglr"], function(E) {
  const SrcLoc = E.SrcLoc
  const GenTokenizer = E.Tokenizer2;
  const IGNORED_WS = {name: "WS"};

  function Tokenizer(spec) {
    [spec.keywords, spec.comments, spec.symbols].forEach(function(specs) {
      for (var first in specs)
        specs[first].forEach(function(s) {
          if (s.parenIsForExp === true)
            s.parenIsForExp = "PARENSPACE";
          else if (!s.parenIsForExp)
            s.parenIsForExp = false;
        });
    });
    for (var first in spec.keywords) {
      spec.keywords[first].forEach(function(s) { s.noFollow = spec.keywordsNoFollow; })
    }
    GenTokenizer.call(this, spec);
    this.parenIsForExp = "PARENSPACE";
  }
  Tokenizer.prototype = Object.create(GenTokenizer.prototype);
  Tokenizer.prototype.tokenizeFrom = function(str) {
    GenTokenizer.prototype.tokenizeFrom.call(this, str);
    this.parenIsForExp = "PARENSPACE";
  }
  Tokenizer.prototype.makeToken = function makeToken(tok_name, s, pos, tok_spec) { 
    var t = new E.Token(tok_name, s);
    t.pos = pos;
    this.parenIsForExp = tok_spec && tok_spec.parenIsForExp;
    this.priorWhitespace = false;
    return t;
  };
  Tokenizer.prototype.makeWSToken = function makeWSToken(startLine, startCol, startPos) {
    this.parenIsForExp = true;
    this.priorWhitespace = true;
    this.addWhitespace(SrcLoc.make(startLine, startCol, startPos, this.line, this.col, this.pos));
    return IGNORED_WS;
    // var t = new E.Token("WS", this.str.slice(startPos, this.pos));
    // t.pos = SrcLoc.make(startLine, startCol, startPos, this.line, this.col, this.pos);
    // // Note: do not change parenIsForExp at all
    // return t;
  }

  const escapes = new RegExp("^(.*?)\\\\([\\\\\"\'nrt]|u[0-9A-Fa-f]{1,4}|x[0-9A-Fa-f]{1,2}|[0-7]{1,3}|[\r\n]{1,2})");
  function fixEscapes(s) {
    var ret = "";
    var match = escapes.exec(s);
    while (match !== null) {
      var esc = match[2];
      ret += match[1];
      s = s.slice(match[0].length);
      if (esc === "\n") {}
      else if (esc === "\r") {}
      else if (esc === "\n\r") {}
      else if (esc === "\r\n") {}
      else if (esc === "n") { ret += "\n"; }
      else if (esc === "r") { ret += "\r"; }
      else if (esc === "t") { ret += "\t"; }
      else if (esc === "\"") { ret += "\""; }
      else if (esc === "'") { ret += "'"; }
      else if (esc === "\\") { ret += "\\"; }
      else if (esc[0] === 'u') { ret += String.fromCharCode(parseInt(esc.slice(1), 16)); }
      else if (esc[0] === 'x') { ret += String.fromCharCode(parseInt(esc.slice(1), 16)); }
      else { ret += String.fromCharCode(parseInt(esc, 8)); }
      match = escapes.exec(s);
    }
    ret += s;
    return ret;
  }
  
  function makeTrie(kwds) {
    var ans = Object.create(null);
    for (var i = 0; i < kwds.length; i++) {
      var firsts = kwds[i].firsts || [kwds[i].val[0]];
      firsts.forEach(function(first) {
        var byFirst = ans[first];
        if (byFirst === undefined)
          byFirst = ans[first] = [];
        byFirst.push(kwds[i]);
      })
    }
    return ans;
  }
  function makeDict(str) {
    var ans = {};
    for (var i = 0; i < str.length; i++) {
      ans[str[i]] = true;
    }
    return ans;
  }

  var keywords = makeTrie([
    {name: "AND", val: "and", parenIsForExp: true},
    {name: "AS", val: "as"},
    {name: "ASCENDING", val: "ascending"},
    {name: "ASK", val: "ask", parenIsForExp: true},
    {name: "BY", val: "by"},
    {name: "CASES", val: "cases"},
    {name: "CHECK", val: "check"},
    {name: "DATA", val: "data"},
    {name: "DESCENDING", val: "descending"},
    {name: "DO", val: "do"},
    {name: "RAISESNOT", val: "does-not-raise", parenIsForExp: true},
    {name: "ELSE", val: "else"},
    {name: "ELSEIF", val: "else if"},
    {name: "END", val: "end"},
    {name: "EXAMPLES", val: "examples", parenIsForExp: true},
    {name: "TABLE-EXTEND", val: "extend"},
    {name: "TABLE-EXTRACT", val: "extract"},
    {name: "FALSE", val: "false"},
    {name: "FOR", val: "for"},
    {name: "FROM", val: "from"},
    {name: "FUN", val: "fun"},
    {name: "HIDING", val: "hiding"},
    {name: "IF", val: "if"},
    {name: "IMPORT", val: "import"},
    {name: "INCLUDE", val: "include"},
    {name: "IS", val: "is", parenIsForExp: true},
    {name: "ISEQUALEQUAL", val: "is==", parenIsForExp: true},
    {name: "ISEQUALTILDE", val: "is=~", parenIsForExp: true},
    {name: "ISNOT", val: "is-not", parenIsForExp: true},
    {name: "ISNOTEQUALEQUAL", val: "is-not==", parenIsForExp: true},
    {name: "ISNOTEQUALTILDE", val: "is-not=~", parenIsForExp: true},
    {name: "ISNOTSPACESHIP", val: "is-not<=>", parenIsForExp: true},
    {name: "ISROUGHLY", val: "is-roughly", parenIsForExp: true},
    {name: "ISNOTROUGHLY", val: "is-not-roughly", parenIsForExp: true},
    {name: "ISSPACESHIP", val: "is<=>", parenIsForExp: true},
    {name: "BECAUSE", val: "because", parenIsForExp: true},
    {name: "LAM", val: "lam"},
    {name: "LAZY", val: "lazy"},
    {name: "LET", val: "let"},
    {name: "LETREC", val: "letrec"},
    {name: "LOAD-TABLE", val: "load-table"},
    {name: "METHOD", val: "method"},
    {name: "MODULE", val: "module"},
    {name: "NEWTYPE", val: "newtype"},
    {name: "OF", val: "of"},
    {name: "OR", val: "or", parenIsForExp: true},
    {name: "PROVIDE", val: "provide"},
    {name: "PROVIDE-TYPES", val: "provide-types"},
    {name: "RAISES", val: "raises", parenIsForExp: true},
    {name: "RAISESOTHER", val: "raises-other-than", parenIsForExp: true},
    {name: "RAISESSATISFIES", val: "raises-satisfies", parenIsForExp: true},
    {name: "RAISESVIOLATES", val: "raises-violates", parenIsForExp: true},
    {name: "REACTOR", val: "reactor"},
    {name: "REC", val: "rec"},
    {name: "REF", val: "ref"},
    {name: "SANITIZE", val: "sanitize"},
    {name: "SATISFIES", val: "satisfies", parenIsForExp: true},
    {name: "TABLE-SELECT", val: "select"},
    {name: "SHADOW", val: "shadow"},
    {name: "TABLE-FILTER", val: "sieve"},
    {name: "SPY", val: "spy"},
    {name: "TABLE-ORDER", val: "order"},
    {name: "TABLE-UPDATE", val: "transform"},
    {name: "TRUE", val: "true"},
    {name: "TYPE", val: "type"},
    {name: "TYPE-LET", val: "type-let"},
    {name: "USING", val: "using"},
    {name: "USE", val: "use"},
    {name: "VAR", val: "var"},
    {name: "SATISFIESNOT", val: "violates", parenIsForExp: true},
    {name: "WHEN", val: "when", parenIsForExp: true}
  ]);
  const keywordsNoFollow = new Set("-_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890");

  const wsString = " \f\n\r\t\v\u00a0\u1680\u2000\u2001\u2002\u2003\u2004\u2005\u2005\u2006\u2007\u2008\u2009\u200a\u2028\u2029\u202f\u205f\u3000\ufeff";
  const whitespace = makeDict(wsString);

  const wsMustFollow = new Set(wsString); wsMustFollow.add("#"); wsMustFollow.add(undefined); // EOF

  //const identChars = new RegExp("[_a-zA-Z][_a-zA-Z0-9]*(?:-+[_a-zA-Z0-9]+)*", "g");
  const identChars = makeDict("_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789");


  // const unsigned_decimal_part = "[0-9]+(?:\\.[0-9]+)?(?:[eE][-+]?[0-9]+)?";
  // const unsigned_rational_part = "[0-9]+/[0-9]+"; 

  // const number = new RegExp("[-+]?" + unsigned_decimal_part, "g");

  const badNumber = new RegExp("~?[+-]?\\.[0-9]+(?:[eE][-+]?[0-9]+)?", "g");

  // const roughnum = new RegExp("~[-+]?"  + unsigned_decimal_part, "g");

  // const rational = new RegExp("[-+]?" + unsigned_rational_part, "g");

  // const roughrational = new RegExp("~[-+]?" + unsigned_rational_part, "g");

  const badOp = new RegExp("(?:\\^|\\+|-|\\*|/|<=|>=|<=>|>=|==|=~|<>|<|>|<-)", "g");

  const tquot_str =
    new RegExp("```(?:" +
               "\\\\[01234567]{1,3}" +
               "|\\\\x[0-9a-fA-F]{1,2}" +
               "|\\\\u[0-9a-fA-F]{1,4}" +
               "|\\\\[\\\\nrt\"\'`]" +
               "|`{1,2}(?!`)" +
               "|[^`\\\\])*```", "g"); // NOTE: Allow unescaped newlines
  const dquot_str =
    new RegExp("\"(?:" +
               "\\\\[01234567]{1,3}" +
               "|\\\\x[0-9a-fA-F]{1,2}" +
               "|\\\\u[0-9a-fA-F]{1,4}" +
               "|\\\\[\\\\nrt\"\']" +
               "|[^\\\\\"\n\r])*\"", "g");
  const squot_str =
    new RegExp("\'(?:" +
               "\\\\[01234567]{1,3}" +
               "|\\\\x[0-9a-fA-F]{1,2}" +
               "|\\\\u[0-9a-fA-F]{1,4}" +
               "|\\\\[\\\\nrt\"\']" +
               "|[^\\\\\'\n\r])*\'", "g");

  const unterminated_string = new RegExp("(?:[\"\']|```).*", "g");
  const octit = makeDict("01234567");
  const digit = makeDict("0123456789");
  const hexit = makeDict("0123456789abcdefABCDEF");
  
  var symbols = makeTrie([
    {name: "BLOCK", val: "block:", parenIsForExp: true},
    {name: "CHECKCOLON", val: "check:", parenIsForExp: true},
    {name: "DOC", val: "doc:", parenIsForExp: true},
    {name: "ELSECOLON", val: "else:", parenIsForExp: true},
    {name: "EXAMPLESCOLON", val: "examples:", parenIsForExp: true},
    {name: "OTHERWISECOLON", val: "otherwise:", parenIsForExp: true},
    {name: "PROVIDECOLON", val: "provide:", parenIsForExp: true},
    {name: "ROW", val: "row:"},
    {name: "SHARING", val: "sharing:", parenIsForExp: true},
    {name: "SOURCECOLON", val: "source:"},
    {name: "TABLE", val: "table:"},
    {name: "THENCOLON", val: "then:", parenIsForExp: true},
    {name: "WHERE", val: "where:", parenIsForExp: true},
    {name: "WITH", val: "with:", parenIsForExp: true},
    {name: "LBRACK", val: "[", parenIsForExp: true},
    {name: "RBRACK", val: "]"},
    {name: "LBRACE", val: "{", parenIsForExp: "PARENAFTERBRACE"},
    {name: "RBRACE", val: "}"},
    { name: "LPAREN", val: "(", parenIsForExp: true,
      process: function processLParen(tok_spec) {
        var tok_type = this.parenIsForExp || "PARENNOSPACE";
        if (this.priorWhitespace)
          tok_type = "PARENSPACE";
        var line = this.curLine, col = this.curCol, pos = this.pos;
        this.curCol++;
        this.pos++;
        return this.makeToken(tok_type, "(",
                              SrcLoc.make(line, col, pos, this.curLine, this.curCol, this.pos),
                              tok_spec);
      }},
    {name: "RPAREN", val: ")"},
    {name: "SEMI", val: ";"},
    {name: "BACKSLASH", val: "\\"},
    {name: "DOTDOTDOT", val: "..."},
    {name: "DOT", val: ".", noFollow: new Set("1234567890")},
    {name: "BANG", val: "!"},
    {name: "PERCENT", val: "%"},
    {name: "COMMA", val: ",", parenIsForExp: true},
    {name: "THINARROW", val: "->"},
    {name: "COLONEQUALS", val: ":=", parenIsForExp: true},
    {name: "COLON", val: ":", parenIsForExp: true},
    {name: "BAR", val: "|", parenIsForExp: true},
    {name: "EQUALS", val: "=", noFollow: new Set("~"), parenIsForExp: true},
    {name: "LANGLE", val: "<", noFollow: new Set(">=")},
    {name: "STAR", val: "*", noFollow: new Set(wsString), needsWs: true, parenIsForExp: true},
    {name: "RANGLE", val: ">", noFollow: new Set("=")},
    { name: "NUMBER", val: "", firsts: new Set("~-+1234567890"),
      process: function tokenizeNumber(tok_spec) {
        var match = undefined;
        var line = this.curLine, col = this.curCol, pos = this.pos;
        var tok_type = "";
        var rough = false;
        if (this.str[this.pos] === "~") {
          rough = true;
          this.pos++; this.curCol++;
        }
        if (this.str[this.pos] === "-" || this.str[this.pos] === "+") {
          this.pos++; this.curCol++;
        }
        if (this.str[this.pos] === ".") { // BAD-NUMBER
          this.pos = pos; this.curCol = col;
          return undefined;
        }
        if (digit[this.str[this.pos]]) {
          // Integer portion, or numerator
          this.pos++; this.curCol++;
          while (digit[this.str[this.pos]]) {
            this.pos++; this.curCol++;
          }
          if (this.str[this.pos] === "/") { // fraction
            this.pos++; this.curCol++;
            if (digit[this.str[this.pos]]) {
              this.pos++; this.curCol++;
              while (digit[this.str[this.pos]]) {
                this.pos++; this.curCol++;
              }
              this.parenIsForExp = false;
              this.priorWhitespace = false;
              return this.makeToken(rough ? "ROUGHRATIONAL" : "RATIONAL", this.str.slice(pos, this.pos),
                                    SrcLoc.make(line, col, pos, this.curLine, this.curCol, this.pos),
                                    tok_spec);
            } else {
              this.pos = pos; this.curCol = col;
              return undefined;
            }
          }
          if (this.str[this.pos] === ".") {
            this.pos++; this.curCol++;
            // decimal portion
            if (digit[this.str[this.pos]]) {
              this.pos++; this.curCol++;
              while (digit[this.str[this.pos]]) {
                this.pos++; this.curCol++;
              }
            } else {
              this.pos--; this.curCol--;
              this.parenIsForExp = false;
              this.priorWhitespace = false;
              return this.makeToken("NUMBER", this.str.slice(pos, this.pos),
                                    SrcLoc.make(line, col, pos, this.curLine, this.curCol, this.pos),
                                    tok_spec);
            }
          }
          if (this.str[this.pos] === "e" || this.str[this.pos] === "E") {
            var advance = this.pos + 1;
            if (this.str[advance] === "+" || this.str[advance] === "-") {
              advance++;
            }
            if (digit[this.str[advance]]) {
              advance++;
              while (digit[this.str[advance]]) {
                advance++;
              }
              this.curCol += (advance - this.pos);
              this.pos = advance;
              this.parenIsForExp = false;
              this.priorWhitespace = false;
              return this.makeToken("NUMBER", this.str.slice(pos, this.pos),
                                    SrcLoc.make(line, col, pos, this.curLine, this.curCol, this.pos),
                                    tok_spec);
            }
          }
          this.parenIsForExp = false;
          this.priorWhitespace = false;
          return this.makeToken("NUMBER", this.str.slice(pos, this.pos),
                                SrcLoc.make(line, col, pos, this.curLine, this.curCol, this.pos),
                                tok_spec);
        }
        // BAD-NUMBER
        this.pos = pos; this.curCol = col;
        return undefined;
      }

      /*function tokenizeNumber(tok_spec) {
        var match = undefined;
        var line = this.curLine, col = this.curCol, pos = this.pos;
        var tok_type = "";
        number.lastIndex = this.pos;
        roughnum.lastIndex = this.pos;
        rational.lastIndex = this.pos;
        roughrational.lastIndex = this.pos;
        var rough = (this.str[this.pos] === "~")
        if (rough) {
          if ((this.str[this.pos + 1] === ".") ||
              ((this.str[this.pos + 1] === "-" || this.str[this.pos + 1] === "+") && this.str[this.pos + 2] === ".")) {
            return undefined;
          } else if ((match = roughrational.exec(this.str)) && match.index === this.pos) {
            tok_type = "ROUGHRATIONAL";
          } else if ((match = rational.exec(this.str)) && match.index === this.pos) {
            tok_type = "RATIONAL";
          }
        } else if ((match = roughnum.exec(this.str)) && match.index === this.pos) {
          tok_type = "NUMBER";
        } else if ((match = number.exec(this.str)) && match.index === this.pos) {
          tok_type = "NUMBER";
        } else {
          return undefined;
        }
        this.pos += match[0].length;
        this.curCol += match[0].length;
        return this.makeToken(tok_type, match[0],
                              SrcLoc.make(line, col, pos, this.curLine, this.curCol, this.pos),
                              tok_spec);
                              }*/
    },
    { name: "BAD-NUMBER", val: "", firsts: new Set("~-+."),
      process: function tokenizeNumber(tok_spec) {
        var match = undefined;
        var line = this.curLine, col = this.curCol, pos = this.pos;
        var tok_type = "";
        badNumber.lastIndex = this.pos;
        if ((match = badNumber.exec(this.str))) {
          tok_type = "BAD-NUMBER";
        } else {
          return undefined;
        }
        this.pos += match[0].length;
        this.curCol += match[0].length;
        return this.makeToken(tok_type, match[0],
                              SrcLoc.make(line, col, pos, this.curLine, this.curCol, this.pos),
                              tok_spec);
      }},
    { name: "NAME", val: "", firsts: new Set("_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"),
      process: function tokenizeName(tok_spec) {
        var line = this.curLine, col = this.curCol, pos = this.pos;
        this.pos++; this.curCol++;
        while (this.pos < this.len) {
          if (identChars[this.str[this.pos]]) {
            this.pos++; this.curCol++;
          } else if (this.str[this.pos] === "-") {
            var front = this.pos + 1;
            while (this.str[front] === "-")
              front++;
            if (identChars[this.str[front]]) {
              this.curCol += (front - this.pos);
              this.pos = front;
            } else {
              break;
            }
          } else {
            break;
          }
        }
        return this.makeToken("NAME", this.str.slice(pos, this.pos),
                              SrcLoc.make(line, col, pos, this.curLine, this.curCol, this.pos),
                              tok_spec);
      }},
    { name: "STRING", val: "\"",
      process: function tokenizeDQString(tok_spec) {
        var line = this.curLine, col = this.curCol, pos = this.pos;
        dquot_str.lastIndex = this.pos;
        unterminated_string.lastIndex = this.pos;
        if ((match = dquot_str.exec(this.str))) {
          this.pos += match[0].length;
          this.curCol += match[0].length;
          return this.makeToken("STRING", fixEscapes(this.str.slice(pos, this.pos)),
                                SrcLoc.make(line, col, pos, this.curLine, this.curCol, this.pos),
                                tok_spec);
        } else if ((match = unterminated_string.exec(this.str))) {
          this.pos += match[0].length;
          this.curCol += match[0].length;
          return this.makeToken("UNTERMINATED-STRING", this.str.slice(pos, this.pos), // no escaping
                                SrcLoc.make(line, col, pos, this.curLine, this.curCol, this.pos),
                                tok_spec);
        }          
      }},
    { name: "STRING", val: "'",
      process: function tokenizeSQString(tok_spec) {
        var line = this.curLine, col = this.curCol, pos = this.pos;
        squot_str.lastIndex = this.pos;
        unterminated_string.lastIndex = this.pos;
        if ((match = squot_str.exec(this.str))) {
          this.pos += match[0].length;
          this.curCol += match[0].length;
          return this.makeToken("STRING", fixEscapes(this.str.slice(pos, this.pos)),
                                SrcLoc.make(line, col, pos, this.curLine, this.curCol, this.pos));
        } else if ((match = unterminated_string.exec(this.str))) {
          this.pos += match[0].length;
          this.curCol += match[0].length;
          return this.makeToken("UNTERMINATED-STRING", this.str.slice(pos, this.pos), // no escaping
                                SrcLoc.make(line, col, pos, this.curLine, this.curCol, this.pos),
                                tok_spec);
        }          
      }},
    { name: "STRING", val: "```",
      process: function tokenizeTQString(tok_spec) {
        var line = this.curLine, col = this.curCol, pos = this.pos;
        tquot_str.lastIndex = this.pos;
        unterminated_string.lastIndex = this.pos;
        if ((match = tquot_str.exec(this.str))) {
          this.pos += match[0].length;
          var lines = match[0].split("\n"); // From jsPerf, this is UNBELIEVABLY much faster than the prior implementation
          this.curLine += lines.length - 1;
          if (lines.length === 1)
            this.curCol += match[0].length;
          else
            this.curCol = lines[lines.length - 1].length;
          return this.makeToken("STRING", fixEscapes(this.str.slice(pos, this.pos)),
                                SrcLoc.make(line, col, pos, this.curLine, this.curCol, this.pos),
                                tok_spec);
        } else if ((match = unterminated_string.exec(this.str))) {
          this.pos += match[0].length;
          this.curCol += match[0].length;
          return this.makeToken("UNTERMINATED-STRING", this.str.slice(pos, this.pos), // no escaping
                                SrcLoc.make(line, col, pos, this.curLine, this.curCol, this.pos),
                                tok_spec);
        }          
      }},
    {name: "CARET", val: "^", mustFollow: wsMustFollow, needsWs: true, parenIsForExp: true},
    {name: "PLUS", val: "+", mustFollow: wsMustFollow, needsWs: true, parenIsForExp: true},
    {name: "DASH", val: "-", mustFollow: wsMustFollow, needsWs: true, parenIsForExp: true},
    {name: "TIMES", val: "*", mustFollow: wsMustFollow, needsWs: true, parenIsForExp: true},
    {name: "SLASH", val: "/", mustFollow: wsMustFollow, needsWs: true, parenIsForExp: true},
    {name: "SPACESHIP", val: "<=>", mustFollow: wsMustFollow, needsWs: true, parenIsForExp: true},
    {name: "LEQ", val: "<=", mustFollow: wsMustFollow, needsWs: true, parenIsForExp: true},
    {name: "GEQ", val: ">=", mustFollow: wsMustFollow, needsWs: true, parenIsForExp: true},
    {name: "EQUALEQUAL", val: "==", mustFollow: wsMustFollow, needsWs: true, parenIsForExp: true},
    {name: "EQUALTILDE", val: "=~", mustFollow: wsMustFollow, needsWs: true, parenIsForExp: true},
    {name: "NEQ", val: "<>", mustFollow: wsMustFollow, needsWs: true, parenIsForExp: true},
    {name: "LT", val: "<", mustFollow: wsMustFollow, needsWs: true, parenIsForExp: true},
    {name: "GT", val: ">", mustFollow: wsMustFollow, needsWs: true, parenIsForExp: true},
    {name: "THICKARROW", val: "=>", mustFollow: wsMustFollow, parenIsForExp: true},
    {name: "COLONCOLON", val: "::", mustFollow: wsMustFollow, parenIsForExp: true},
    { name: "BAD-OPER", val: "", firsts: new Set("^+-*/\<>="),
      process: function tokenizeBadOper(tok_spec) {
        var line = this.curLine, col = this.curCol, pos = this.pos;
        badOp.lastIndex = this.pos;
        var match;
        if ((match = badOp.exec(this.str))) {
          this.pos += match[0].length;
          this.curCol += match[0].length;
          this.parenIsForExp = false;
          this.priorWhitespace = false;
          return this.makeToken("BAD-OPER", this.str.slice(pos, this.pos),
                                SrcLoc.make(line, col, pos, this.curLine, this.curCol, this.pos),
                                tok_spec);
        } else {
          return undefined;
        }
      }}
  ]);
  
 
  const comments = makeTrie([
    { name: "BLOCKCOMMENT", val: "#|",
      process: function tokenizeBlockComment(tok_spec) {
        var nestingDepth = 1;
        var line = this.curLine, col = this.curCol, pos = this.pos;
        this.pos += 2; this.curCol += 2;
        while (nestingDepth > 0 && this.pos < this.len) {
          if (this.str.startsWith("#|", this.pos)) {
            nestingDepth++;
            this.pos += 2;
            this.curCol += 2;
          } else if (this.str.startsWith("|#", this.pos)) {
            nestingDepth--;
            this.pos += 2;
            this.curCol += 2;
          } else if (this.str[this.pos] === "\n") {
            this.curLine++;
            this.curCol = 0;
            this.pos++;
          } else {
            this.pos++;
            this.curCol++;
          }
        }
        if (nestingDepth === 0) {
          return this.makeWSToken(line, col, pos);
        } else {
          var ws_loc = SrcLoc.make(line, col, pos, this.curLine, this.curCol, this.pos);
          return this.makeToken("UNTERMINATED-BLOCK-COMMENT", this.str.slice(pos, this.pos), ws_loc, tok_spec);
        }
      }},
    { name: "COMMENT", val: "#",
      process: function tokenizeLineComment(tok_spec) {
        var line = this.curLine, col = this.curCol, pos = this.pos;
        while (this.pos < this.len && this.str[this.pos] !== "\n" && this.str[this.pos] !== "\r") {
          this.pos++;
          this.curCol++;
        }
        return this.makeToken("COMMENT", ""/*this.str.slice(pos, this.pos)*/, line, col, pos);
      }}
  ]);
  
  const spec = {
    keywords,
    keywordsNoFollow,
    symbols,
    whitespace,
    comments,
    ignore: new Set(["WS", "COMMENT"])
  };


  return {
    'Tokenizer': new Tokenizer(spec)
  }; 
});
