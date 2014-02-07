define(["./tokenizer", "./rnglr"], function(T, E) {
  const Grammar = E.Grammar
  const Nonterm = E.Nonterm
  const Token = E.Token
  const SrcLoc = E.SrcLoc
  const GenTokenizer = T.Tokenizer;
  const STICKY_REGEXP = T.STICKY_REGEXP;

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
  Tokenizer.prototype.makeToken = function (tok_type, s, pos) { 
    if (tok_type === "STRING") s = fixEscapes(s);
    return GenTokenizer.prototype.makeToken(tok_type, s, pos);
  }
  Tokenizer.prototype.postProcessMatch = function(tok, match) {
    var tok_type = tok.name;
    if (tok_type === "PAREN?") {
      for (var j = 0; j < this.Tokens.length; j++) {
        this.Tokens[j].val.lastIndex = 0;
        var op = this.Tokens[j].val.exec(match[0]);
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



  function kw(str) { return "^(?:" + str + ")(?![-_a-zA-Z0-9])"; }
  function anyOf(strs) { return "(?:" + strs.join("|") + ")(?![-_a-zA-Z0-9])"; }
  const operator_regex_str = anyOf(["\\+", "-", "\\*", "/", "<=", ">=", "==", "<>", 
                                    "<", ">", "and", "or", "not", "is", "raises"]);
  const name = new RegExp("^[_a-zA-Z][-_a-zA-Z0-9]*", STICKY_REGEXP);
  const number = new RegExp("^-?[0-9]+(?:\\.[0-9]+)?", STICKY_REGEXP);
  const parenparen = new RegExp("^\\((?=\\()", STICKY_REGEXP); // NOTE: Don't include the following paren
  const opparen = new RegExp("^" + operator_regex_str + "(?=\\()", STICKY_REGEXP); // NOTE: likewise
  const spaceparen = new RegExp("^\\s+\\(", STICKY_REGEXP);
  const ws = new RegExp("^\\s+", STICKY_REGEXP);
  const comment = new RegExp("^#.*(?:\\n|\\r|\\r\\n|\\n\\r)", STICKY_REGEXP)
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
  const comma = new RegExp("^,", STICKY_REGEXP);
  const thinarrow = new RegExp("^->", STICKY_REGEXP);
  const thickarrow = new RegExp("^=>", STICKY_REGEXP);
  const coloncolon = new RegExp("^::", STICKY_REGEXP);
  const colon = new RegExp("^:", STICKY_REGEXP);
  const caret = new RegExp("^\\^", STICKY_REGEXP);
  const equals = new RegExp("^=", STICKY_REGEXP);
  const colonequals = new RegExp("^:=", STICKY_REGEXP);
  const semi = new RegExp("^;", STICKY_REGEXP);
  const backslash = new RegExp("^\\\\", STICKY_REGEXP);
  const opplus = new RegExp("^\\s+\\+\\s+", STICKY_REGEXP);
  const opminus = new RegExp("^\\s+-\\s+", STICKY_REGEXP);
  const optimes = new RegExp("^\\s+\\*\\s+", STICKY_REGEXP);
  const opdiv = new RegExp("^\\s+/\\s+", STICKY_REGEXP);
  const opleq = new RegExp("^\\s+<=\\s+", STICKY_REGEXP);
  const opgeq = new RegExp("^\\s+>=\\s+", STICKY_REGEXP);
  const opeq = new RegExp("^\\s+==\\s+", STICKY_REGEXP);
  const opneq = new RegExp("^\\s+<>\\s+", STICKY_REGEXP);
  const oplt = new RegExp("^\\s*<\\s*", STICKY_REGEXP);
  const opgt = new RegExp("^\\s*>\\s*", STICKY_REGEXP);
  const opand = new RegExp("^and(?![-_a-zA-Z0-9])", STICKY_REGEXP);
  const opor = new RegExp("^or(?![-_a-zA-Z0-9])", STICKY_REGEXP);
  const opnot = new RegExp("^not(?![-_a-zA-Z0-9])", STICKY_REGEXP);
  const opis = new RegExp("^is(?![-_a-zA-Z0-9])", STICKY_REGEXP);
  const opraises = new RegExp("^raises(?![-_a-zA-Z0-9])", STICKY_REGEXP);

  const slashable = "[\\\\nrt\"\']"
  const dquot_str = 
    new RegExp("^\"(?:" +
               "\\\\[01234567]{1,3}" +
               "|\\\\x[0-9a-fA-F]{1,2}" + 
               "|\\\\u[0-9a-fA-f]{1,4}" + 
               "|\\\\[\r\n]{1,2}" + 
               "|\\\\[\\\\nrt\"\']" + 
               "|[^\"])*\"", STICKY_REGEXP);
  const squot_str = 
    new RegExp("^\'(?:" +
               "\\\\[01234567]{1,3}" +
               "|\\\\x[0-9a-fA-F]{1,2}" + 
               "|\\\\u[0-9a-fA-f]{1,4}" + 
               "|\\\\[\r\n]{1,2}" + 
               "|\\\\[\\\\nrt\"\']" + 
               "|[^\'])*\'", STICKY_REGEXP);

  const anychar = new RegExp("^[^]", STICKY_REGEXP);
  const Tokens = [
    {name: "PAREN?", val: parenparen, parenIsForExp: true},
    {name: "PAREN?", val: opparen, parenIsForExp: true},
    {name: "PARENSPACE", val: spaceparen, parenIsForExp: true},
    {name: "LPAREN?", val: lparen, parenIsForExp: true},


    {name: "IMPORT", val: new RegExp(kw("import"), STICKY_REGEXP)},
    {name: "PROVIDE", val: new RegExp(kw("provide"), STICKY_REGEXP)},
    {name: "AS", val: new RegExp(kw("as"), STICKY_REGEXP)},
    {name: "VAR", val: new RegExp(kw("var"), STICKY_REGEXP)},
    {name: "FUN", val: new RegExp(kw("fun"), STICKY_REGEXP)},
    {name: "TRUE", val: new RegExp(kw("true"), STICKY_REGEXP)},
    {name: "FALSE", val: new RegExp(kw("false"), STICKY_REGEXP)},
    {name: "METHOD", val: new RegExp(kw("method"), STICKY_REGEXP)},
    {name: "DOC", val: new RegExp(kw("doc:"), STICKY_REGEXP)},
    {name: "WHERE", val: new RegExp(kw("where:"), STICKY_REGEXP)},
    {name: "CHECK", val: new RegExp(kw("check:"), STICKY_REGEXP)},
    {name: "TRY", val: new RegExp(kw("try:"), STICKY_REGEXP)},
    {name: "EXCEPT", val: new RegExp(kw("except"), STICKY_REGEXP)},
    {name: "CASES", val: new RegExp(kw("cases"), STICKY_REGEXP)},
    {name: "WHEN", val: new RegExp(kw("when"), STICKY_REGEXP)},
    {name: "IFCOLON", val: new RegExp(kw("if:"), STICKY_REGEXP)},
    {name: "IF", val: new RegExp(kw("if"), STICKY_REGEXP)},
    {name: "THENCOLON", val: new RegExp(kw("then:"), STICKY_REGEXP)},
    {name: "ELSECOLON", val: new RegExp(kw("else:"), STICKY_REGEXP)},
    {name: "ELSEIF", val: new RegExp(kw("else if"), STICKY_REGEXP)},
    {name: "ELSE", val: new RegExp(kw("else"), STICKY_REGEXP)},
    {name: "DATA", val: new RegExp(kw("data"), STICKY_REGEXP)},
    {name: "WITH", val: new RegExp(kw("with:"), STICKY_REGEXP)},
    {name: "SHARING", val: new RegExp(kw("sharing:"), STICKY_REGEXP)},
    {name: "SHADOW", val: new RegExp(kw("shadow"), STICKY_REGEXP)},
    {name: "MUTABLE", val: new RegExp(kw("mutable"), STICKY_REGEXP)},
    {name: "CYCLIC", val: new RegExp(kw("cyclic"), STICKY_REGEXP)},
    {name: "DATATYPE", val: new RegExp(kw("datatype"), STICKY_REGEXP)},
    {name: "WITHCONSTRUCTOR", val: new RegExp(kw("with constructor"), STICKY_REGEXP)},
    {name: "GRAPH", val: new RegExp(kw("graph:"), STICKY_REGEXP)},
    {name: "BLOCK", val: new RegExp(kw("block:"), STICKY_REGEXP)},
    {name: "FOR", val: new RegExp(kw("for"), STICKY_REGEXP)},
    {name: "FROM", val: new RegExp(kw("from"), STICKY_REGEXP)},
    {name: "END", val: new RegExp(kw("end"), STICKY_REGEXP)},
    
    {name: "DOT", val: period},
    {name: "BANG", val: bang},
    {name: "COMMA", val: comma},
    {name: "THINARROW", val: thinarrow},
    {name: "THICKARROW", val: thickarrow},
    {name: "COLONEQUALS", val: colonequals},
    {name: "COLONCOLON", val: coloncolon},
    {name: "COLON", val: colon},
    {name: "CARET", val: caret},
    {name: "BAR", val: bar},

    {name: "NUMBER", val: number},
    {name: "STRING", val: dquot_str}, 
    {name: "STRING", val: squot_str},

    {name: "PLUS", val: opplus, parenIsForExp: true},
    {name: "DASH", val: opminus, parenIsForExp: true},
    {name: "STAR", val: optimes, parenIsForExp: true},
    {name: "SLASH", val: opdiv, parenIsForExp: true},
    {name: "LEQ", val: opleq, parenIsForExp: true},
    {name: "GEQ", val: opgeq, parenIsForExp: true},
    {name: "EQUALEQUAL", val: opeq, parenIsForExp: true},
    {name: "NEQ", val: opneq, parenIsForExp: true},
    {name: "LT", val: oplt, parenIsForExp: true},
    {name: "GT", val: opgt, parenIsForExp: true},
    {name: "AND", val: opand},
    {name: "OR", val: opor},
    {name: "NOT", val: opnot},
    {name: "IS", val: opis},
    {name: "RAISES", val: opraises},
    {name: "LBRACK", val: lbrack}, 
    {name: "RBRACK", val: rbrack}, 
    {name: "LBRACE", val: lbrace}, 
    {name: "RBRACE", val: rbrace}, 
    {name: "RPAREN", val: rparen}, 
    {name: "LANGLE", val: langle, parenIsForExp: true},
    {name: "RANGLE", val: rangle, parenIsForExp: true},

    {name: "EQUALS", val: equals},

    {name: "COMMENT", val: comment}, 
    {name: "WS", val: ws, parenIsForExp: true},

    {name: "SEMI", val: semi},
    {name: "BACKSLASH", val: backslash},

    {name: "NAME", val: name},

    {name: "UNKNOWN", val: anychar},
  ];



  function ListCons(hd, tl, shouldInline) {
    return function(kids) {
      var useful_kids = [];
      for (var i = 0; i < kids.length; i++) {
        if (kids[i].name === hd) {
          if (kids[i].shouldInline === true)
            useful_kids = useful_kids.concat(kids[i].kids);
          else
            useful_kids.push(kids[i]);
        } else if (kids[i].name === tl) useful_kids = useful_kids.concat(kids[i].kids); 
      }
      var start = (kids.length > 0 ? kids[kids.length - 1].startColumn : undefined);
      return { name: tl, kids: useful_kids, toString: E.Rule.defaultASTToString, startColumn: start,
               shouldInline: shouldInline };
    }
  }

  function Inline(kids) {
    var ret = E.Rule.defaultAction.call(this, kids);
    ret.shouldInline = true;
    return ret;
  }


  return {
    'Tokenizer': new Tokenizer(true, Tokens)
  };
});
