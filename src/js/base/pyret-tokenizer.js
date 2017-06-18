define(["jglr/jglr"], function(E) {
  const Grammar = E.Grammar
  const Nonterm = E.Nonterm
  const Token = E.Token
  const SrcLoc = E.SrcLoc
  const GenTokenizer = E.Tokenizer;
  const STICKY_REGEXP = E.STICKY_REGEXP;

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

  function Tokenizer(ignore_ws, Tokens) {
    GenTokenizer.call(this, ignore_ws, Tokens);
    this.parenIsForExp = true; // initialize this at the beginning of file to true
  }
  Tokenizer.prototype = Object.create(GenTokenizer.prototype);
  Tokenizer.prototype.tokenizeFrom = function(str) {
    GenTokenizer.prototype.tokenizeFrom.call(this, str);
    this.parenIsForExp = "PARENSPACE";
  }
  Tokenizer.prototype.makeToken = function (tok_type, s, pos) {
    switch(tok_type) {
    case "STRING": s = fixEscapes(s); break;
    case "LONG_STRING": tok_type = "STRING"; break;
    case "PARENSPACE": case "PARENNOSPACE": case "PARENAFTERBRACE":
    case "PLUS": case "DASH": case "STAR": case "SLASH":
    case "LT": case "GT": case "CARET":
      // Trim off whitespace
      pos = SrcLoc.make(pos.endRow, pos.endCol - 1, pos.endChar - 1, pos.endRow, pos.endCol, pos.endChar);
      break;
    case "EQUALEQUAL": case "NEQ": case "LEQ": case "GEQ": // they're longer tokens
      // Trim off whitespace
      pos = SrcLoc.make(pos.endRow, pos.endCol - 2, pos.endChar - 2, pos.endRow, pos.endCol, pos.endChar);
      break;
    default:
      break;
    }
    return GenTokenizer.prototype.makeToken(tok_type, s, pos);
  }
  Tokenizer.prototype.postProcessMatch = function(tok, match, str) {
    var tok_type = tok.name;
    if (tok_type === "PAREN?") {
      for (var j = 0; j < this.Tokens.length; j++) {
        if (STICKY_REGEXP !== '') {
          var oldIndex = this.Tokens[j].val.lastIndex;
          this.Tokens[j].val.lastIndex = 0;
        }
        var op = this.Tokens[j].val.exec(match[0]);
        if (STICKY_REGEXP !== '') {
          this.Tokens[j].val.lastIndex = oldIndex;
        }
        if (op !== null) {
          tok_type = this.Tokens[j].name;
          if (tok_type == "LPAREN?")
            tok_type = this.parenIsForExp || "PARENNOSPACE";
          break;
        }
      }
    } else if (tok_type === "LPAREN?") {
      tok_type = this.parenIsForExp || "PARENNOSPACE";
    } else if (tok_type === "BLOCKCOMMENT") {
      return this.tokenizeBlockComment(match, str, 1, 2);
    }
    this.parenIsForExp = tok.parenIsForExp || "PARENNOSPACE";
    return tok_type;
  }
  Tokenizer.prototype.tokenizeBlockComment = function(match, str, nestingDepth, commentLen) {
    var strLen = str.length;
    while (nestingDepth > 0 && commentLen < strLen) {
      if (str.substr(commentLen, 2) === "#|") {
        nestingDepth++;
        commentLen += 2;
      } else if (str.substr(commentLen, 2) === "|#") {
        nestingDepth--;
        commentLen += 2;
      } else {
        commentLen++;
      }
    }
    match[0] = str.substr(0, commentLen);
    return nestingDepth == 0 ? "COMMENT" : "UNTERMINATED-BLOCK-COMMENT";
  }

  const ws_after = "(?=\\s|$|#)"; // allow actual space, end-of-input, or comments

  function kw(str) { return "^(?:" + str + ")(?![-_a-zA-Z0-9])"; }
  function colonKw(str) { return "^(?:" + str + ")"; }
  function anyOf(strs) { return "(?:" + strs.join("|") + ")(?![-_a-zA-Z0-9])"; }
  function op(str) { return "^\\s+" + str + ws_after; }

  const name = new RegExp("^[_a-zA-Z][_a-zA-Z0-9]*(?:-+[_a-zA-Z0-9]+)*", STICKY_REGEXP);

  const unsigned_decimal_part = "[0-9]+(?:\\.[0-9]+)?(?:[eE][-+]?[0-9]+)?";
  const unsigned_rational_part = "[0-9]+/[0-9]+"; 

  const number = new RegExp("^[-+]?" + unsigned_decimal_part, STICKY_REGEXP);

  const badNumber = new RegExp("^~?[+-]?\\.[0-9]+(?:[eE][-+]?[0-9]+)?", STICKY_REGEXP);

  const roughnum = new RegExp("^~[-+]?"  + "(?:" + unsigned_rational_part + "|" + unsigned_decimal_part + ")", STICKY_REGEXP);

  const rational = new RegExp("^[-+]?" + unsigned_rational_part, STICKY_REGEXP);

  const parenparen = new RegExp("^\\((?=\\()", STICKY_REGEXP); // NOTE: Don't include the following paren
  const spaceparen = new RegExp("^\\s+\\(", STICKY_REGEXP);
  const ws = new RegExp("^\\s+", STICKY_REGEXP);
  const comment = new RegExp("^(#((?!\\|).*)?(?:\\n|\\r|\\r\\n|\\n\\r|$))", STICKY_REGEXP)
  const blockcommentstart = new RegExp("^(#\\|)", STICKY_REGEXP);
  const bar = new RegExp("^\\|", STICKY_REGEXP);
  const langle = new RegExp("^<(?![>=])", STICKY_REGEXP);
  const rangle = new RegExp("^>(?!=)", STICKY_REGEXP);
  const lbrack = new RegExp("^\\[", STICKY_REGEXP);
  const rbrack = new RegExp("^\\]", STICKY_REGEXP);
  const lbrace = new RegExp("^\\{", STICKY_REGEXP);
  const rbrace = new RegExp("^\\}", STICKY_REGEXP);
  const lparen = new RegExp("^\\(", STICKY_REGEXP);
  const rparen = new RegExp("^\\)", STICKY_REGEXP);
  const period = new RegExp("^\\.", STICKY_REGEXP);
  const dotdotdot = new RegExp("^\\.\\.\\.", STICKY_REGEXP);
  const bang = new RegExp("^!", STICKY_REGEXP);
  const percent = new RegExp("^%", STICKY_REGEXP);
  const comma = new RegExp("^,", STICKY_REGEXP);
  const thinarrow = new RegExp("^->", STICKY_REGEXP);
  const thickarrow = new RegExp("^=>" + ws_after, STICKY_REGEXP);
  const coloncolon = new RegExp("^::" + ws_after, STICKY_REGEXP);
  const colon = new RegExp("^:", STICKY_REGEXP);
  const equals = new RegExp("^=(?!~)", STICKY_REGEXP);
  const colonequals = new RegExp("^:=", STICKY_REGEXP);
  const semi = new RegExp("^;", STICKY_REGEXP);
  const backslash = new RegExp("^\\\\", STICKY_REGEXP);

  const oppcaret = new RegExp(op("\\^"), STICKY_REGEXP);
  const opplus = new RegExp(op("\\+"), STICKY_REGEXP);
  const opminus = new RegExp(op("-"), STICKY_REGEXP);
  const optimes = new RegExp(op("\\*"), STICKY_REGEXP);
  const opdiv = new RegExp(op("/"), STICKY_REGEXP);
  const opleq = new RegExp(op("<="), STICKY_REGEXP);
  const opgeq = new RegExp(op(">="), STICKY_REGEXP);
  const opidentical = new RegExp(op("<=>"), STICKY_REGEXP);
  const opeq = new RegExp(op("=="), STICKY_REGEXP);
  const opeqnow = new RegExp(op("=~"), STICKY_REGEXP);
  const opneq = new RegExp(op("<>"), STICKY_REGEXP);
  const oplt = new RegExp(op("<"), STICKY_REGEXP);
  const opgt = new RegExp(op(">"), STICKY_REGEXP);
  

  const opsNoSpace = new RegExp("^(?:\\^|\\+|-|\\*|/|<=|>=|<=>|>=|==|=~|<>|<|>|<-)", STICKY_REGEXP);

  // English ops don't require whitespace. That way it is possible to catch them in ID position
  const opand = new RegExp(kw("and"), STICKY_REGEXP);
  const opor = new RegExp(kw("or"), STICKY_REGEXP);
  const opiseq = new RegExp(kw("is=="), STICKY_REGEXP);
  const opiseqnow = new RegExp(kw("is=~"), STICKY_REGEXP);
  const opisidentical = new RegExp(kw("is<=>"), STICKY_REGEXP);
  const opis = new RegExp(kw("is"), STICKY_REGEXP);
  const opisroughly = new RegExp(kw("is-roughly"), STICKY_REGEXP);
  const opisnoteq = new RegExp(kw("is-not=="), STICKY_REGEXP);
  const opisnoteqnow = new RegExp(kw("is-not=~"), STICKY_REGEXP);
  const opisnotidentical = new RegExp(kw("is-not<=>"), STICKY_REGEXP);
  const opisnot = new RegExp(kw("is-not"), STICKY_REGEXP);
  const opsatisfies = new RegExp(kw("satisfies"), STICKY_REGEXP);
  const opsatisfiesnot = new RegExp(kw("violates"), STICKY_REGEXP);
  const opraises = new RegExp(kw("raises"), STICKY_REGEXP);
  const opraisesother = new RegExp(kw("raises-other-than"), STICKY_REGEXP);
  const opraisesnot = new RegExp(kw("does-not-raise"), STICKY_REGEXP);
  const opraisessatisfies = new RegExp(kw("raises-satisfies"), STICKY_REGEXP);
  const opraisesviolates = new RegExp(kw("raises-violates"), STICKY_REGEXP);

  const tquot_str =
    new RegExp("^```(?:" +
               "\\\\[01234567]{1,3}" +
               "|\\\\x[0-9a-fA-F]{1,2}" +
               "|\\\\u[0-9a-fA-f]{1,4}" +
               "|\\\\[\\\\nrt\"\'`]" +
               "|`{1,2}(?!`)" +
               "|[^`\\\\])*```", STICKY_REGEXP); // NOTE: Allow unescaped newlines
  const dquot_str =
    new RegExp("^\"(?:" +
               "\\\\[01234567]{1,3}" +
               "|\\\\x[0-9a-fA-F]{1,2}" +
               "|\\\\u[0-9a-fA-f]{1,4}" +
               "|\\\\[\\\\nrt\"\']" +
               "|[^\\\\\"\n\r])*\"", STICKY_REGEXP);
  const squot_str =
    new RegExp("^\'(?:" +
               "\\\\[01234567]{1,3}" +
               "|\\\\x[0-9a-fA-F]{1,2}" +
               "|\\\\u[0-9a-fA-f]{1,4}" +
               "|\\\\[\\\\nrt\"\']" +
               "|[^\\\\\'\n\r])*\'", STICKY_REGEXP);

  const unterminated_string = new RegExp("^(?:[\"\']|```).*", STICKY_REGEXP);

  const anychar = new RegExp("^[^]", STICKY_REGEXP);
  const Tokens = [
    {name: "PAREN?", val: parenparen, parenIsForExp: true},
    {name: "PARENSPACE", val: spaceparen, parenIsForExp: true},
    {name: "LPAREN?", val: lparen, parenIsForExp: true},

    {name: "IMPORT", val: new RegExp(kw("import"), STICKY_REGEXP)},
    {name: "INCLUDE", val: new RegExp(kw("include"), STICKY_REGEXP)},
    {name: "PROVIDE-TYPES", val: new RegExp(kw("provide-types"), STICKY_REGEXP)},
    {name: "PROVIDE", val: new RegExp(kw("provide"), STICKY_REGEXP)},
    {name: "AS", val: new RegExp(kw("as"), STICKY_REGEXP)},
    {name: "ASCENDING", val: new RegExp(kw("ascending"), STICKY_REGEXP)},
    {name: "DESCENDING", val: new RegExp(kw("descending"), STICKY_REGEXP)},
    {name: "NEWTYPE", val: new RegExp(kw("newtype"), STICKY_REGEXP)},
    {name: "TYPE-LET", val: new RegExp(kw("type-let"), STICKY_REGEXP)},
    {name: "TYPE", val: new RegExp(kw("type"), STICKY_REGEXP)},
    {name: "VAR", val: new RegExp(kw("var"), STICKY_REGEXP)},
    {name: "REC", val: new RegExp(kw("rec"), STICKY_REGEXP)},
    {name: "LETREC", val: new RegExp(kw("letrec"), STICKY_REGEXP)},
    {name: "LET", val: new RegExp(kw("let"), STICKY_REGEXP)},
    {name: "FUN", val: new RegExp(kw("fun"), STICKY_REGEXP)},
    {name: "LAM", val: new RegExp(kw("lam"), STICKY_REGEXP)},
    {name: "TRUE", val: new RegExp(kw("true"), STICKY_REGEXP)},
    {name: "FALSE", val: new RegExp(kw("false"), STICKY_REGEXP)},
    {name: "METHOD", val: new RegExp(kw("method"), STICKY_REGEXP)},
    {name: "DOC", val: new RegExp(colonKw("doc:"), STICKY_REGEXP), parenIsForExp: true},
    {name: "WHERE", val: new RegExp(colonKw("where:"), STICKY_REGEXP), parenIsForExp: true},
    {name: "CHECKCOLON", val: new RegExp(colonKw("check:"), STICKY_REGEXP), parenIsForExp: true},
    {name: "EXAMPLESCOLON", val: new RegExp(colonKw("examples:"), STICKY_REGEXP), parenIsForExp: true},
    {name: "CHECK", val: new RegExp(kw("check"), STICKY_REGEXP)},
    {name: "TABLE", val: new RegExp(colonKw("table:"), STICKY_REGEXP)},
    {name: "ROW", val: new RegExp(colonKw("row:"), STICKY_REGEXP)},
    {name: "USING", val: new RegExp(colonKw("using"), STICKY_REGEXP)},
    {name: "TABLE-EXTEND", val: new RegExp(kw("extend"), STICKY_REGEXP)},
    {name: "TABLE-UPDATE", val: new RegExp(kw("transform"), STICKY_REGEXP)},
    {name: "TABLE-SELECT", val: new RegExp(kw("select"), STICKY_REGEXP)},
    {name: "TABLE-FILTER", val: new RegExp(kw("sieve"), STICKY_REGEXP)},
    {name: "TABLE-ORDER",  val: new RegExp(kw("order"), STICKY_REGEXP)},
    {name: "TABLE-EXTRACT",  val: new RegExp(kw("extract"), STICKY_REGEXP)},
    {name: "LOAD-TABLE", val: new RegExp(kw("load-table"), STICKY_REGEXP)},
    {name: "SANITIZE", val: new RegExp(kw("sanitize"), STICKY_REGEXP)},
    {name: "SOURCECOLON", val: new RegExp(kw("source:"), STICKY_REGEXP)},
    {name: "REACTOR", val: new RegExp(kw("reactor"), STICKY_REGEXP)},
    {name: "CASES", val: new RegExp(kw("cases"), STICKY_REGEXP)},
    {name: "WHEN", val: new RegExp(kw("when"), STICKY_REGEXP), parenIsForExp: true},
    {name: "ASK", val: new RegExp(kw("ask"), STICKY_REGEXP), parenIsForExp: true},
    {name: "OTHERWISECOLON", val: new RegExp(colonKw("otherwise:"), STICKY_REGEXP), parenIsForExp: true},
    {name: "IF", val: new RegExp(kw("if"), STICKY_REGEXP)},
    {name: "OF", val: new RegExp(kw("of"), STICKY_REGEXP)},
    {name: "THENCOLON", val: new RegExp(colonKw("then:"), STICKY_REGEXP), parenIsForExp: true},
    {name: "ELSECOLON", val: new RegExp(colonKw("else:"), STICKY_REGEXP), parenIsForExp: true},
    {name: "ELSEIF", val: new RegExp(kw("else if"), STICKY_REGEXP)},
    {name: "ELSE", val: new RegExp(kw("else"), STICKY_REGEXP)},
    {name: "DATA", val: new RegExp(kw("data"), STICKY_REGEXP)},
    {name: "WITH", val: new RegExp(colonKw("with:"), STICKY_REGEXP), parenIsForExp: true},
    {name: "SHARING", val: new RegExp(colonKw("sharing:"), STICKY_REGEXP), parenIsForExp: true},
    {name: "SHADOW", val: new RegExp(kw("shadow"), STICKY_REGEXP)},
    {name: "REF", val: new RegExp(kw("ref"), STICKY_REGEXP)},
    {name: "BLOCK", val: new RegExp(colonKw("block:"), STICKY_REGEXP), parenIsForExp: true},
    {name: "FOR", val: new RegExp(kw("for"), STICKY_REGEXP)},
    {name: "FROM", val: new RegExp(kw("from"), STICKY_REGEXP)},
    {name: "DO", val: new RegExp(kw("do"), STICKY_REGEXP)},
    {name: "END", val: new RegExp(kw("end"), STICKY_REGEXP)},
    {name: "LAZY", val: new RegExp(kw("lazy"), STICKY_REGEXP)},
    {name: "BY", val: new RegExp(kw("by"), STICKY_REGEXP)},

    {name: "BAD-NUMBER", val: badNumber},
    {name: "DOTDOTDOT", val: dotdotdot},
    {name: "DOT", val: period},
    {name: "BANG", val: bang},
    {name: "PERCENT", val: percent},
    {name: "COMMA", val: comma, parenIsForExp: true},
    {name: "THINARROW", val: thinarrow},
    {name: "THICKARROW", val: thickarrow, parenIsForExp: true},
    {name: "COLONEQUALS", val: colonequals, parenIsForExp: true},
    {name: "COLONCOLON", val: coloncolon, parenIsForExp: true},
    {name: "COLON", val: colon, parenIsForExp: true},
    {name: "BAR", val: bar, parenIsForExp: true},

    {name: "RATIONAL", val: rational},
    {name: "NUMBER", val: number},
    {name: "NUMBER", val: roughnum},
    {name: "LONG_STRING", val: tquot_str},
    {name: "STRING", val: dquot_str},
    {name: "STRING", val: squot_str},

    {name: "CARET", val: oppcaret, parenIsForExp: true},
    {name: "PLUS", val: opplus, parenIsForExp: true},
    {name: "DASH", val: opminus, parenIsForExp: true},
    {name: "STAR", val: optimes, parenIsForExp: true},
    {name: "SLASH", val: opdiv, parenIsForExp: true},
    {name: "SPACESHIP", val: opidentical, parenIsForExp: true},
    {name: "LEQ", val: opleq, parenIsForExp: true},
    {name: "GEQ", val: opgeq, parenIsForExp: true},
    {name: "EQUALEQUAL", val: opeq, parenIsForExp: true},
    {name: "EQUALTILDE", val: opeqnow, parenIsForExp: true},
    {name: "NEQ", val: opneq, parenIsForExp: true},
    {name: "LT", val: oplt, parenIsForExp: true},
    {name: "GT", val: opgt, parenIsForExp: true},
    {name: "AND", val: opand, parenIsForExp: true},
    {name: "OR", val: opor, parenIsForExp: true},
    {name: "ISNOTEQUALEQUAL", val: opisnoteq, parenIsForExp: true},
    {name: "ISNOTEQUALTILDE", val: opisnoteqnow, parenIsForExp: true},
    {name: "ISNOTSPACESHIP", val: opisnotidentical, parenIsForExp: true},
    {name: "ISNOT", val: opisnot, parenIsForExp: true},
    {name: "ISEQUALEQUAL", val: opiseq, parenIsForExp: true},
    {name: "ISEQUALTILDE", val: opiseqnow, parenIsForExp: true},
    {name: "ISSPACESHIP", val: opisidentical, parenIsForExp: true},
    {name: "ISROUGHLY", val: opisroughly, parenIsForExp: true},
    {name: "IS", val: opis, parenIsForExp: true},
    {name: "SATISFIESNOT", val: opsatisfiesnot, parenIsForExp: true},
    {name: "SATISFIES", val: opsatisfies, parenIsForExp: true},
    {name: "RAISESOTHER", val: opraisesother, parenIsForExp: true},
    {name: "RAISESNOT", val: opraisesnot, parenIsForExp: true},
    {name: "RAISESSATISFIES", val: opraisessatisfies, parenIsForExp: true},
    {name: "RAISESVIOLATES", val: opraisesviolates, parenIsForExp: true},
    {name: "RAISES", val: opraises, parenIsForExp: true},

    {name: "LBRACK", val: lbrack},
    {name: "RBRACK", val: rbrack},
    {name: "LBRACE", val: lbrace, parenIsForExp: "PARENAFTERBRACE"},
    {name: "RBRACE", val: rbrace},
    {name: "RPAREN", val: rparen},
    {name: "LANGLE", val: langle},
    {name: "RANGLE", val: rangle},

    {name: "EQUALS", val: equals, parenIsForExp: true},

    {name: "BAD-OPER", val: opsNoSpace},

    {name: "BLOCKCOMMENT", val: blockcommentstart},
    {name: "COMMENT", val: comment},
    {name: "WS", val: ws, parenIsForExp: true},

    {name: "SEMI", val: semi},
    {name: "BACKSLASH", val: backslash},

    {name: "NAME", val: name},

    {name: "UNTERMINATED-STRING", val: unterminated_string},
    {name: "UNKNOWN", val: anychar},
  ];
  Tokens.forEach(function(tok) {
    if (!tok.hasOwnProperty("parenIsForExp")) tok.parenIsForExp = false;
    else if (tok.parenIsForExp == true) tok.parenIsForExp = "PARENSPACE";
  });

  return {
    'Tokenizer': new Tokenizer(true, Tokens)
  };
});
