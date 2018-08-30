define("tokenize", ["jglr/rnglr"], function(E) {
  const SrcLoc = E.SrcLoc
  /*! https://mths.be/startswith v0.2.0 by @mathias */
  if (!String.prototype.startsWith) {
    (function() {
      'use strict'; // needed to support `apply`/`call` with `undefined`/`null`
      var defineProperty = (function() {
        // IE 8 only supports `Object.defineProperty` on DOM elements
        try {
	  var object = {};
	  var $defineProperty = Object.defineProperty;
	  var result = $defineProperty(object, object, object) && $defineProperty;
        } catch(error) {}
        return result;
      }());
      var toString = {}.toString;
      var startsWith = function(search) {
        if (this == null) {
	  throw TypeError();
        }
        var string = String(this);
        if (search && toString.call(search) == '[object RegExp]') {
	  throw TypeError();
        }
        var stringLength = string.length;
        var searchString = String(search);
        var searchLength = searchString.length;
        var position = arguments.length > 1 ? arguments[1] : undefined;
        // `ToInteger`
        var pos = position ? Number(position) : 0;
        if (pos != pos) { // better `isNaN`
	  pos = 0;
        }
        var start = Math.min(Math.max(pos, 0), stringLength);
        // Avoid the `indexOf` call if no match is possible
        if (searchLength + start > stringLength) {
	  return false;
        }
        var index = -1;
        while (++index < searchLength) {
	  if (string.charCodeAt(start + index) != searchString.charCodeAt(index)) {
	    return false;
	  }
        }
        return true;
      };
      if (defineProperty) {
        defineProperty(String.prototype, 'startsWith', {
	  'value': startsWith,
	  'configurable': true,
	  'writable': true
        });
      } else {
        String.prototype.startsWith = startsWith;
      }
    }());
  }

  function makeToken(tok_type, s, pos) { 
    var t = new E.Token(tok_type, s);
    t.pos = pos;
    return t;
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
      else { ret += String.fromCharCode(parseInt(esc.slice(2), 8)); }
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

  var keywords = makeTrie([
    {name: "AND", val: "and"},
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
    {name: "ISSPACESHIP", val: "is<=>", parenIsForExp: true},
    {name: "LAM", val: "lam"},
    {name: "LAZY", val: "lazy"},
    {name: "LET", val: "let"},
    {name: "LETREC", val: "letrec"},
    {name: "LOAD-TABLE", val: "load-table"},
    {name: "METHOD", val: "method"},
    {name: "NEWTYPE", val: "newtype"},
    {name: "OF", val: "of"},
    {name: "OR", val: "or"},
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
    {name: "VAR", val: "var"},
    {name: "SATISFIESNOT", val: "violates", parenIsForExp: true},
    {name: "WHEN", val: "when", parenIsForExp: true}
  ]);
  const keywordsNoFollow = new Set("-_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890");

  const whitespace = new Set(" \f\n\r\t\v\u00a0\u1680\u2000\u2001\u2002\u2003\u2004\u2005\u2005\u2006\u2007\u2008\u2009\u200a\u2028\u2029\u202f\u205f\u3000\ufeff");

  const wsMustFollow = new Set(whitespace); wsMustFollow.add("#"); wsMustFollow.add(undefined); // EOF

  const identChars = new RegExp("[_a-zA-Z][_a-zA-Z0-9]*(?:-+[_a-zA-Z0-9]+)*", "g");

  const unsigned_decimal_part = "[0-9]+(?:\\.[0-9]+)?(?:[eE][-+]?[0-9]+)?";
  const unsigned_rational_part = "[0-9]+/[0-9]+"; 

  const number = new RegExp("[-+]?" + unsigned_decimal_part, "g");

  const badNumber = new RegExp("~?[+-]?\\.[0-9]+(?:[eE][-+]?[0-9]+)?", "g");

  const roughnum = new RegExp("~[-+]?"  + unsigned_decimal_part, "g");

  const rational = new RegExp("[-+]?" + unsigned_rational_part, "g");

  const roughrational = new RegExp("~[-+]?" + unsigned_rational_part, "g");

  const badOp = new RegExp("(?:\\^|\\+|-|\\*|/|<=|>=|<=>|>=|==|=~|<>|<|>|<-)", "g");

  const tquot_str =
    new RegExp("```(?:" +
               "\\\\[01234567]{1,3}" +
               "|\\\\x[0-9a-fA-F]{1,2}" +
               "|\\\\u[0-9a-fA-f]{1,4}" +
               "|\\\\[\\\\nrt\"\'`]" +
               "|`{1,2}(?!`)" +
               "|[^`\\\\])*```", "g"); // NOTE: Allow unescaped newlines
  const dquot_str =
    new RegExp("\"(?:" +
               "\\\\[01234567]{1,3}" +
               "|\\\\x[0-9a-fA-F]{1,2}" +
               "|\\\\u[0-9a-fA-f]{1,4}" +
               "|\\\\[\\\\nrt\"\']" +
               "|[^\\\\\"\n\r])*\"", "g");
  const squot_str =
    new RegExp("\'(?:" +
               "\\\\[01234567]{1,3}" +
               "|\\\\x[0-9a-fA-F]{1,2}" +
               "|\\\\u[0-9a-fA-f]{1,4}" +
               "|\\\\[\\\\nrt\"\']" +
               "|[^\\\\\'\n\r])*\'", "g");

  const unterminated_string = new RegExp("^(?:[\"\']|```).*", "g");
  
  var symbols = makeTrie([
    {name: "BLOCK", val: "block:", parenIsForExp: true},
    {name: "CHECKCOLON", val: "check:", parenIsForExp: true},
    {name: "DOC", val: "doc:", parenIsForExp: true},
    {name: "ELSECOLON", val: "else:", parenIsForExp: true},
    {name: "EXAMPLESCOLON", val: "examples:", parenIsForExp: true},
    {name: "OTHERWISECOLON", val: "otherwise:", parenIsForExp: true},
    {name: "ROW", val: "row:"},
    {name: "SHARING", val: "sharing:", parenIsForExp: true},
    {name: "SOURCECOLON", val: "source:"},
    {name: "TABLE", val: "table:"},
    {name: "THENCOLON", val: "then:", parenIsForExp: true},
    {name: "WHERE", val: "where:", parenIsForExp: true},
    {name: "WITH", val: "with:", parenIsForExp: true},
    {name: "LBRACK", val: "["},
    {name: "RBRACK", val: "]"},
    {name: "LBRACE", val: "{", parenIsForExp: "PARENAFTERBRACE"},
    {name: "RBRACE", val: "}"},
    { name: "LPAREN", val: "(", parenIsForExp: true,
      process: function processLParen() {
        var tok_type = this.parenIsForExp || "PARENNOSPACE";
        if (this.priorWhitespace)
          tok_type = "PARENSPACE"
        var line = this.curLine, col = this.curCol, pos = this.pos;
        this.curCol++;
        this.pos++;
        this.parenIsForExp = "PARENSPACE";
        return makeToken(tok_type, "(",
                         SrcLoc.make(line, col, pos, this.curLine, this.curCol, this.curPos));
      }},
    {name: "RPAREN", val: ")"},
    {name: "SEMI", val: ";"},
    {name: "BACKSLASH", val: "\\"},
    {name: "DOTDOTDOT", val: "..."},
    {name: "DOT", val: ".", noFollow: new Set("1234567890")},
    {name: "BANG", val: "!"},
    {name: "PERCENT", val: "%"},
    {name: "COMMA", val: ","},
    {name: "THINARROW", val: "->"},
    {name: "COLONEQUALS", val: ":=", parenIsForExp: true},
    {name: "COLON", val: ":", parenIsForExp: true},
    {name: "BAR", val: "|", parenIsForExp: true},
    {name: "EQUALS", val: "=", noFollow: new Set("~"), parenIsForExp: true},
    {name: "LANGLE", val: "<", noFollow: new Set(">=")},
    {name: "RANGLE", val: ">", noFollow: new Set("=")},
    { name: "NUMBER", val: "", firsts: new Set("~-+1234567890"),
      process: function tokenizeNumber() {
        var match = undefined;
        var line = this.curLine, col = this.curCol, pos = this.pos;
        var tok_type = "";
        number.lastIndex = this.pos;
        roughnum.lastIndex = this.pos;
        rational.lastIndex = this.pos;
        roughrational.lastIndex = this.pos;
        if (this.str[this.pos] === "~" &&
            (this.str[this.pos + 1] === ".") ||
            ((this.str[this.pos + 1] === "-" || this.str[this.pos + 1] === "+") && this.str[this.pos + 2] === ".")) {
          return undefined;
        } else if ((match = roughrational.exec(this.str)) && match.index === this.pos) {
          tok_type = "ROUGHRATIONAL";
        } else if ((match = rational.exec(this.str)) && match.index === this.pos) {
          tok_type = "RATIONAL";
        } else if ((match = roughnum.exec(this.str)) && match.index === this.pos) {
          tok_type = "NUMBER";
        } else if ((match = number.exec(this.str)) && match.index === this.pos) {
          tok_type = "NUMBER";
        } else {
          return undefined;
        }
        this.pos += match[0].length;
        this.curCol += match[0].length;
        this.parenIsForExp = false;
        this.priorWhitespace = false;
        return makeToken(tok_type, match[0],
                         SrcLoc.make(line, col, pos, this.curLine, this.curCol, this.curPos));
      }},
    { name: "BAD-NUMBER", val: "", firsts: new Set("~-+."),
      process: function tokenizeNumber() {
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
        this.parenIsForExp = false;
        this.priorWhitespace = false;
        return makeToken(tok_type, match[0],
                         SrcLoc.make(line, col, pos, this.curLine, this.curCol, this.curPos));
      }},
    { name: "NAME", val: "", firsts: new Set("_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"),
      process: function tokenizeName() {
        var line = this.curLine, col = this.curCol, pos = this.pos;
        identChars.lastIndex = this.pos;
        var match = identChars.exec(this.str);
        this.pos += match[0].length;
        this.curCol += match[0].length;
        this.parenIsForExp = false;
        this.priorWhitespace = false;
        return makeToken("NAME", match[0],
                         SrcLoc.make(line, col, pos, this.curLine, this.curCol, this.curPos));
      }},
    { name: "STRING", val: "\"",
      process: function tokenizeDQString() {
        var line = this.curLine, col = this.curCol, pos = this.pos;
        dquot_str.lastIndex = this.pos;
        unterminated_string.lastIndex = this.pos;
        if ((match = dquot_str.exec(this.str))) {
          this.pos += match[0].length;
          this.curCol += match[0].length;
          this.parenIsForExp = false;
          this.priorWhitespace = false;
          return makeToken("STRING", fixEscapes(this.str.slice(pos, this.pos)),
                           SrcLoc.make(line, col, pos, this.curLine, this.curCol, this.curPos));
        } else if ((match = unterminated_string.exec(this.str))) {
          this.pos += match[0].length;
          this.curCol += match[0].length;
          this.parenIsForExp = false;
          this.priorWhitespace = false;
          return makeToken("UNTERMINATED-STRING", this.str.slice(pos, this.pos), // no escaping
                           SrcLoc.make(line, col, pos, this.curLine, this.curCol, this.curPos));
        }          
      }},
    { name: "STRING", val: "'",
      process: function tokenizeSQString() {
        var line = this.curLine, col = this.curCol, pos = this.pos;
        squot_str.lastIndex = this.pos;
        unterminated_string.lastIndex = this.pos;
        if ((match = squot_str.exec(this.str))) {
          this.pos += match[0].length;
          this.curCol += match[0].length;
          this.parenIsForExp = false;
          this.priorWhitespace = false;
          return makeToken("STRING", fixEscapes(this.str.slice(pos, this.pos)),
                           SrcLoc.make(line, col, pos, this.curLine, this.curCol, this.curPos));
        } else if ((match = unterminated_string.exec(this.str))) {
          this.pos += match[0].length;
          this.curCol += match[0].length;
          this.parenIsForExp = false;
          this.priorWhitespace = false;
          return makeToken("UNTERMINATED-STRING", this.str.slice(pos, this.pos), // no escaping
                           SrcLoc.make(line, col, pos, this.curLine, this.curCol, this.curPos));
        }          
      }},
    { name: "STRING", val: "```",
      process: function tokenizeTQString() {
        var line = this.curLine, col = this.curCol, pos = this.pos;
        tquot_str.lastIndex = this.pos;
        unterminated_string.lastIndex = this.pos;
        if ((match = tquot_str.exec(this.str))) {
          this.pos += match[0].length;
          this.curCol += match[0].length;
          this.parenIsForExp = false;
          this.priorWhitespace = false;
          return makeToken("STRING", fixEscapes(this.str.slice(pos, this.pos)),
                           SrcLoc.make(line, col, pos, this.curLine, this.curCol, this.curPos));
        } else if ((match = unterminated_string.exec(this.str))) {
          this.pos += match[0].length;
          this.curCol += match[0].length;
          this.parenIsForExp = false;
          this.priorWhitespace = false;
          return makeToken("UNTERMINATED-STRING", this.str.slice(pos, this.pos), // no escaping
                           SrcLoc.make(line, col, pos, this.curLine, this.curCol, this.curPos));
        }          
      }},
    {name: "CARET", val: "^", mustFollow: wsMustFollow, needsWs: true},
    {name: "PLUS", val: "+", mustFollow: wsMustFollow, needsWs: true},
    {name: "DASH", val: "-", mustFollow: wsMustFollow, needsWs: true},
    {name: "STAR", val: "*", mustFollow: wsMustFollow, needsWs: true},
    {name: "SLASH", val: "/", mustFollow: wsMustFollow, needsWs: true},
    {name: "SPACESHIP", val: "<=>", mustFollow: wsMustFollow, needsWs: true},
    {name: "LEQ", val: "<=", mustFollow: wsMustFollow, needsWs: true},
    {name: "GEQ", val: ">=", mustFollow: wsMustFollow, needsWs: true},
    {name: "EQUALEQUAL", val: "==", mustFollow: wsMustFollow, needsWs: true},
    {name: "EQUALTILDE", val: "=~", mustFollow: wsMustFollow, needsWs: true},
    {name: "NEQ", val: "<>", mustFollow: wsMustFollow, needsWs: true},
    {name: "LT", val: "<", mustFollow: wsMustFollow, needsWs: true},
    {name: "GT", val: ">", mustFollow: wsMustFollow, needsWs: true},
    {name: "THICKARROW", val: "=>", mustFollow: wsMustFollow, parenIsForSpace: true},
    {name: "COLONCOLON", val: "::", mustFollow: wsMustFollow, parenIsForSpace: true},
    { name: "BAD-OPER", val: "", firsts: new Set("^+-*/\<>="),
      process: function tokenizeBadOper() {
        var line = this.curLine, col = this.curCol, pos = this.pos;
        badOp.lastIndex = this.pos;
        var match;
        if ((match = badOp.exec(this.str))) {
          this.pos += match[0].length;
          this.curCol += match[0].length;
          this.parenIsForExp = false;
          this.priorWhitespace = false;
          return makeToken("BAD-OPER", this.str.slice(pos, this.pos),
                           SrcLoc.make(line, col, pos, this.curLine, this.curCol, this.curPos));
        } else {
          return undefined;
        }
      }}
  ]);
  
 
  const comments = makeTrie([
    { name: "BLOCKCOMMENT", val: "#|",
      process: function tokenizeBlockComment() {
        var nestingDepth = 1;
        var line = this.curLine, col = this.curCol, pos = this.pos;
        this.pos += 2;
        while (nestingDepth > 0 && this.pos < this.len) {
          if (this.str.startsWith("#|", this.pos)) {
            nestingDepth++;
            this.pos += 2;
          } else if (this.str.startsWith("|#", this.pos)) {
            nestingDepth--;
            this.pos += 2;
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
          return makeToken("COMMENT", this.str.slice(pos, this.pos),
                           SrcLoc.make(line, col, pos, this.curLine, this.curCol, this.pos));
        } else {
          return makeToken("UNTERMINATED-BLOCK-COMMENT", this.str.slice(pos, this.pos),
                           SrcLoc.make(line, col, pos, this.curLine, this.curCol, this.pos));
        }
      }},
    { name: "COMMENT", val: "#",
      process: function tokenizeLineComment() {
        var line = this.curLine, col = this.curCol, pos = this.pos;
        while (this.pos < this.len && this.str[this.pos] !== "\n" && this.str[this.pos] !== "\r") {
          this.pos++;
          this.curCol++;
        }
        return makeToken("COMMENT", this.str.slice(pos, this.pos),
                         SrcLoc.make(line, col, pos, this.curLine, this.curCol, this.pos));    
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

  function merge(dest, src, defaultNoFollow) {
    for (var first in src) {
      var cur = dest[first];
      if (cur === undefined)
        cur = dest[first] = [];
      var ext = src[first];
      for (var i = 0; i < ext.length; i++) {
        var tok = ext[i];
        cur.push({name: tok.name, val: tok.val,
                  noFollow: tok.noFollow || defaultNoFollow,
                  mustFollow: tok.mustFollow,
                  needsWs: tok.needsWs,
                  process: tok.process,
                  parenIsForExp: tok.parenIsForExp ? "PARENSPACE" : false});
      }
    }
  }
  
  function Tokenizer(str, spec) {
    if (typeof str.normalize === "function")
      this.str = str.normalize("NFC");
    else
      this.str = str;
    
    this.len = str.length; // XXX Not necessarily unicode-aware
    this.curCol = 0;
    this.curLine = 1;
    this.pos = 0;
    this.priorWhitespace = true;
    this.ignore = spec.ignore;
    this.parenIsForExp = "PARENSPACE";

    this.whitespace = spec.whitespace;
    this.keywords = Object.create(null);
    merge(this.keywords, spec.symbols, undefined);
    merge(this.keywords, spec.comments, undefined);
    merge(this.keywords, spec.keywords, spec.keywordsNoFollow);
    for (var first in this.keywords) {
      this.keywords[first].sort(function(a, b) {
        // sort in descending length, then by which needs priorWhitespace, then alphabetically
        if (b.val.length === a.val.length) {
          if (a.needsWs && !b.needsWs) return -1;
          if (b.needsWs && !a.needsWs) return 1;
          return (a < b) ? -1 : 0;
        }
        return b.val.length - a.val.length;
      });
    }
  }
  Tokenizer.prototype.isEmpty = function() { return this.length == 0; }
  Tokenizer.prototype.hasNext = function() { return this.pos <= this.len; }
  Tokenizer.prototype.unshiftToken = function(prev_tok) { this.curTok = prev_tok; }
  Tokenizer.prototype.next = function() {
    this.curTok = undefined;
    while (this.curTok === undefined && this.hasNext()) {
      if (this.pos == this.len) { 
        this.pos++;
        this.curCol++;
        E.EOF.pos = SrcLoc.make(this.curLine, this.curCol, this.pos, this.curLine, this.curCol, this.pos);
        this.curTok = E.EOF;
        return E.EOF; 
      }
      var c = this.str[this.pos];  // XXX not unicode aware
      var candidates = this.keywords[c];
      var line = this.curLine, col = this.curCol, pos = this.pos;
      if (candidates) {
        for (var i = 0; i < candidates.length; i++) {
          var candidate = candidates[i];
          if (this.str.startsWith(candidate.val, this.pos)) {
            if (candidate.process) {
              this.curTok = candidate.process.call(this);
              if (this.curTok) break;
            } else {
              var noFollow = candidate.noFollow;
              var mustFollow = candidate.mustFollow;
              var match;
              if (candidate.needsWs && !this.priorWhitespace) {
                match = false;
              } else if (!noFollow && !mustFollow) {
                match = true;
              } else if (noFollow && !noFollow.has(this.str[this.pos + candidate.val.length])) {
                match = true;
              } else if (mustFollow && mustFollow.has(this.str[this.pos + candidate.val.length])) {
                match = true;
              }
              if (match) {
                this.curCol += candidate.val.length;
                this.pos += candidate.val.length;
                var t = makeToken(candidate.name, candidate.val,
                                  SrcLoc.make(line, col, pos, this.curLine, this.curCol, this.pos));
                this.parenIsForExp = candidate.parenIsForExp;
                this.priorWhitespace = false;
                this.curTok = t;
                break;
              }
            }
          }
        }
      }
      // Handle whitespace
      if (this.curTok === undefined && this.whitespace.has(c)) {
        this.priorWhitespace = true;
        while (this.whitespace.has(c)) {
          this.pos++;
          this.curCol++;
          if (c === "\n") {
            this.curLine++;
            this.curCol = 0;
          }
          c = this.str[this.pos];  // XXX not unicode aware
        }
        this.curTok = makeToken("WS", this.str.slice(pos, this.pos),
                             SrcLoc.make(line, col, pos, this.curLine, this.curCol, this.pos));
      }
      if (this.curTok === undefined) {
        // FAILOVER
        var t = makeToken("UNKNOWN", this.str[this.pos],
                          SrcLoc.make(line, col, pos, line, col + 1, pos + 1));
        this.curCol++;
        this.pos++;
        this.curTok = t;
      }

      if (this.ignore.has(this.curTok.name)) {
        this.curTok = undefined;
      }
    }
    return this.curTok;
  }

  return {
    'Tokenizer': Tokenizer,
    'Atom': E.Atom,
    'Nonterm': E.Nonterm,
    'Token': E.Token,
    'Rule': E.Rule,
    'Grammar': E.Grammar,
    'SetOfSets': E.SetOfSets,
    'EOF': E.EOF,
    'EPSILON': E.EPSILON,
    'SrcLoc': E.SrcLoc,
    'spec': spec
  };
});
