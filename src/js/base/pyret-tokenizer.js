define(["../../../lib/jglr/jglr"], function(E) {
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
    this.parenIsForExp = true;
  }
  Tokenizer.prototype.makeToken = function (tok_type, s, pos) {
    switch(tok_type) {
    case "STRING": s = fixEscapes(s); break;
    case "LONG_STRING": tok_type = "STRING"; break;
    case "PARENSPACE": case "PARENNOSPACE":
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
  Tokenizer.prototype.postProcessMatch = function(tok, match) {
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
            tok_type = this.parenIsForExp ? "PARENSPACE" : "PARENNOSPACE";
          break;
        }
      }
    } else if (tok_type == "LPAREN?") {
      tok_type = this.parenIsForExp ? "PARENSPACE" : "PARENNOSPACE";
    }
    this.parenIsForExp = !!tok.parenIsForExp;
    return tok_type;
  }


  const ws_after = "(?=\\s|$|#)"; // allow actual space, end-of-input, or comments

  function kw(str) { return "^(?:" + str + ")(?![-_a-zA-Z0-9])"; }
  function colonKw(str) { return "^(?:" + str + ")"; }
  function anyOf(strs) { return "(?:" + strs.join("|") + ")(?![-_a-zA-Z0-9])"; }
  function op(str) { return "^\\s+" + str + ws_after; }

  const name = new RegExp("^[_a-zA-Z][_a-zA-Z0-9]*(?:-+[_a-zA-Z0-9]+)*", STICKY_REGEXP);
  const number = new RegExp("^-?[0-9]+(?:\\.[0-9]+)?", STICKY_REGEXP);
  const rational = new RegExp("^-?[0-9]+/[0-9]+", STICKY_REGEXP);
  const parenparen = new RegExp("^\\((?=\\()", STICKY_REGEXP); // NOTE: Don't include the following paren
  const spaceparen = new RegExp("^\\s+\\(", STICKY_REGEXP);
  const ws = new RegExp("^\\s+", STICKY_REGEXP);
  const comment = new RegExp("^#.*(?:\\n|\\r|\\r\\n|\\n\\r|$)", STICKY_REGEXP)
  const bar = new RegExp("^\\|", STICKY_REGEXP);
  const langle = new RegExp("^<", STICKY_REGEXP);
  const rangle = new RegExp("^>", STICKY_REGEXP);
  const lbrack = new RegExp("^\\[", STICKY_REGEXP);
  const rbrack = new RegExp("^\\]", STICKY_REGEXP);
  const lbrace = new RegExp("^\\{", STICKY_REGEXP);
  const rbrace = new RegExp("^\\}", STICKY_REGEXP);
  const lparen = new RegExp("^\\(", STICKY_REGEXP);
  const rparen = new RegExp("^\\)", STICKY_REGEXP);
  const period = new RegExp("^\\.", STICKY_REGEXP);
  const bang = new RegExp("^!", STICKY_REGEXP);
  const percent = new RegExp("^%", STICKY_REGEXP);
  const comma = new RegExp("^,", STICKY_REGEXP);
  const thinarrow = new RegExp("^->", STICKY_REGEXP);
  const thickarrow = new RegExp("^=>" + ws_after, STICKY_REGEXP);
  const coloncolon = new RegExp("^::" + ws_after, STICKY_REGEXP);
  const colon = new RegExp("^:", STICKY_REGEXP);
  const equals = new RegExp("^=", STICKY_REGEXP);
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

  // English ops don't require whitespace. That way it is possible to catch them in ID position
  const opand = new RegExp(kw("and"), STICKY_REGEXP);
  const opor = new RegExp(kw("or"), STICKY_REGEXP);
  const opiseq = new RegExp(kw("is=="), STICKY_REGEXP);
  const opiseqnow = new RegExp(kw("is=~"), STICKY_REGEXP);
  const opisidentical = new RegExp(kw("is<=>"), STICKY_REGEXP);
  const opis = new RegExp(kw("is"), STICKY_REGEXP);
  const opisnoteq = new RegExp(kw("is-not=="), STICKY_REGEXP);
  const opisnoteqnow = new RegExp(kw("is-not=~"), STICKY_REGEXP);
  const opisnotidentical = new RegExp(kw("is-not<=>"), STICKY_REGEXP);
  const opisnot = new RegExp(kw("is-not"), STICKY_REGEXP);
  const opsatisfies = new RegExp(kw("satisfies"), STICKY_REGEXP);
  const opsatisfiesnot = new RegExp(kw("violates"), STICKY_REGEXP);
  const opraises = new RegExp(kw("raises"), STICKY_REGEXP);
  const opraisesother = new RegExp(kw("raises-other-than"), STICKY_REGEXP);
  const opraisesnot = new RegExp(kw("does-not-raise"), STICKY_REGEXP);

  const slashable = "[\\\\nrt\"\']"
  const tquot_str =
    new RegExp("^```(?:" +
               "\\\\[01234567]{1,3}" +
               "|\\\\x[0-9a-fA-F]{1,2}" +
               "|\\\\u[0-9a-fA-f]{1,4}" +
               "|\\\\[\\\\nrt\"\']" +
               "|[^`])*```", STICKY_REGEXP); // NOTE: Allow unescaped newlines
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

  const unterminated_string = new RegExp("^[\"\'].*", STICKY_REGEXP);

  const anychar = new RegExp("^[^]", STICKY_REGEXP);
  const Tokens = [
    {name: "PAREN?", val: parenparen, parenIsForExp: true},
    {name: "PARENSPACE", val: spaceparen, parenIsForExp: true},
    {name: "LPAREN?", val: lparen, parenIsForExp: true},


    {name: "IMPORT", val: new RegExp(kw("import"), STICKY_REGEXP)},
    {name: "PROVIDE-TYPES", val: new RegExp(kw("provide-types"), STICKY_REGEXP)},
    {name: "PROVIDE", val: new RegExp(kw("provide"), STICKY_REGEXP)},
    {name: "AS", val: new RegExp(kw("as"), STICKY_REGEXP)},
    {name: "NEWTYPE", val: new RegExp(kw("newtype"), STICKY_REGEXP)},
    {name: "TYPE-LET", val: new RegExp(kw("type-let"), STICKY_REGEXP)},
    {name: "TYPE", val: new RegExp(kw("type"), STICKY_REGEXP)},
    {name: "VAR", val: new RegExp(kw("var"), STICKY_REGEXP)},
    {name: "LETREC", val: new RegExp(kw("letrec"), STICKY_REGEXP)},
    {name: "LET", val: new RegExp(kw("let"), STICKY_REGEXP)},
    {name: "FUN", val: new RegExp(kw("fun"), STICKY_REGEXP)},
    {name: "LAM", val: new RegExp(kw("lam"), STICKY_REGEXP)},
    {name: "TRUE", val: new RegExp(kw("true"), STICKY_REGEXP)},
    {name: "FALSE", val: new RegExp(kw("false"), STICKY_REGEXP)},
    {name: "METHOD", val: new RegExp(kw("method"), STICKY_REGEXP)},
    {name: "DOC", val: new RegExp(colonKw("doc:"), STICKY_REGEXP)},
    {name: "WHERE", val: new RegExp(colonKw("where:"), STICKY_REGEXP)},
    {name: "CHECKCOLON", val: new RegExp(colonKw("check:"), STICKY_REGEXP)},
    {name: "CHECK", val: new RegExp(kw("check"), STICKY_REGEXP)},
    {name: "TRY", val: new RegExp(colonKw("try:"), STICKY_REGEXP)},
    {name: "EXCEPT", val: new RegExp(kw("except"), STICKY_REGEXP)},
    {name: "CASES", val: new RegExp(kw("cases"), STICKY_REGEXP)},
    {name: "WHEN", val: new RegExp(kw("when"), STICKY_REGEXP)},
    {name: "ASKCOLON", val: new RegExp(colonKw("ask:"), STICKY_REGEXP)},
    {name: "OTHERWISECOLON", val: new RegExp(colonKw("otherwise:"), STICKY_REGEXP)},
    {name: "IF", val: new RegExp(kw("if"), STICKY_REGEXP)},
    {name: "THENCOLON", val: new RegExp(colonKw("then:"), STICKY_REGEXP)},
    {name: "ELSECOLON", val: new RegExp(colonKw("else:"), STICKY_REGEXP)},
    {name: "ELSEIF", val: new RegExp(kw("else if"), STICKY_REGEXP)},
    {name: "ELSE", val: new RegExp(kw("else"), STICKY_REGEXP)},
    {name: "DATA", val: new RegExp(kw("data"), STICKY_REGEXP)},
    {name: "WITH", val: new RegExp(colonKw("with:"), STICKY_REGEXP)},
    {name: "SHARING", val: new RegExp(colonKw("sharing:"), STICKY_REGEXP)},
    {name: "SHADOW", val: new RegExp(kw("shadow"), STICKY_REGEXP)},
    {name: "REF", val: new RegExp(kw("ref"), STICKY_REGEXP)},
    {name: "DATATYPE", val: new RegExp(kw("datatype"), STICKY_REGEXP)},
    {name: "WITHCONSTRUCTOR", val: new RegExp(kw("with constructor"), STICKY_REGEXP)},
    {name: "GRAPH", val: new RegExp(colonKw("graph:"), STICKY_REGEXP)},
    {name: "MGRAPH", val: new RegExp(colonKw("ref-graph:"), STICKY_REGEXP)},
    {name: "BLOCK", val: new RegExp(colonKw("block:"), STICKY_REGEXP)},
    {name: "FOR", val: new RegExp(kw("for"), STICKY_REGEXP)},
    {name: "FROM", val: new RegExp(kw("from"), STICKY_REGEXP)},
    {name: "END", val: new RegExp(kw("end"), STICKY_REGEXP)},
    {name: "LAZY", val: new RegExp(kw("lazy"), STICKY_REGEXP)},

    {name: "DOT", val: period},
    {name: "BANG", val: bang},
    {name: "PERCENT", val: percent},
    {name: "COMMA", val: comma, parenIsForExp: true},
    {name: "THINARROW", val: thinarrow},
    {name: "THICKARROW", val: thickarrow, parenIsForExp: true},
    {name: "COLONEQUALS", val: colonequals, parenIsForExp: true},
    {name: "COLONCOLON", val: coloncolon},
    {name: "COLON", val: colon, parenIsForExp: true},
    {name: "BAR", val: bar},

    {name: "RATIONAL", val: rational},
    {name: "NUMBER", val: number},
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
    {name: "IS", val: opis, parenIsForExp: true},
    {name: "SATISFIESNOT", val: opsatisfiesnot, parenIsForExp: true},
    {name: "SATISFIES", val: opsatisfies, parenIsForExp: true},
    {name: "RAISESOTHER", val: opraisesother, parenIsForExp: true},
    {name: "RAISESNOT", val: opraisesnot, parenIsForExp: true},
    {name: "RAISES", val: opraises, parenIsForExp: true},

    {name: "LBRACK", val: lbrack},
    {name: "RBRACK", val: rbrack},
    {name: "LBRACE", val: lbrace},
    {name: "RBRACE", val: rbrace},
    {name: "RPAREN", val: rparen},
    {name: "LANGLE", val: langle},
    {name: "RANGLE", val: rangle},

    {name: "EQUALS", val: equals, parenIsForExp: true},

    {name: "COMMENT", val: comment},
    {name: "WS", val: ws, parenIsForExp: true},

    {name: "SEMI", val: semi},
    {name: "BACKSLASH", val: backslash},

    {name: "NAME", val: name},

    {name: "UNTERMINATED-STRING", val: unterminated_string},
    {name: "UNKNOWN", val: anychar},
  ];


  return {
    'Tokenizer': new Tokenizer(true, Tokens)
  };
});
