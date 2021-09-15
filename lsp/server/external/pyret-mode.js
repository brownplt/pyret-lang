CodeMirror.defineMode("pyret", function(config, parserConfig) {
  var ERRORCLASS = 'error';
  function wordRegexp(words) {
    return new RegExp("^((" + words.join(")|(") + "))(?![a-zA-Z0-9-_])");
  }
  function toToken(type) {
    return function(tstr) {
      return {type: type, string: tstr};
    };
  }

  const pyret_indent_regex = new RegExp("^[a-zA-Z_][a-zA-Z0-9$_\\-]*");
  const pyret_closing_keywords = ["end"];
  const pyret_closing_builtins = [];
  const pyret_closing_tokens =
        pyret_closing_keywords.map(toToken("keyword")).concat(
          pyret_closing_builtins.map(toToken("builtin")));
  const pyret_opening_keywords_colon = ["reactor", "try", "ref-graph", "block", "table", "load-table"];
  const pyret_opening_keywords_nocolon = ["fun", "when", "for", "if", "let", "type-let", "ask", "spy",
                                          "cases", "data", "shared", "check",
                                          "except", "letrec", "lam", "method",
                                          "examples", "do", "select", "extend", "transform", "extract",
                                          "sieve", "order", "provide"];
  const pyret_opening_keywords = pyret_opening_keywords_colon.concat(pyret_opening_keywords_nocolon);
  const pyret_opening_tokens = pyret_opening_keywords.map(toToken("keyword"));
  const pyret_openers_closed_by_end = {"FUN": true, "WHEN": true, "DO": true,
    "FOR": true, "IF": true, "BLOCK": true, "LET": true, "TABLE": true,
    "LOADTABLE": true, "SELECT": true, "EXTEND": true, "SIEVE": true, "TRANSFORM": true, "EXTRACT": true,
    "ORDER": true, "REACTOR": true, "SPY": true};
  const pyret_keywords =
    wordRegexp(["else if"].concat(pyret_opening_keywords_nocolon, pyret_closing_keywords,
               ["spy", "var", "rec", "import", "include", "type", "newtype",
                "from", "lazy", "shadow", "ref", "of",
                "and", "or", "as", "else", "cases", "is==", "is=~", "is<=>", "is", "satisfies", "raises",
                "violates", "by", "ascending", "descending", "sanitize", "using", "because"]));
  const pyret_booleans = wordRegexp(["true", "false"]);
  const pyret_keywords_hyphen =
    wordRegexp(["provide-types", "type-let", "does-not-raise", "raises-violates",
                "raises-satisfies", "raises-other-than", "is-roughly", "is-not==", "is-not=~", "is-not<=>", "is-not"]);
  const pyret_keywords_colon =
    wordRegexp(pyret_opening_keywords_colon.concat(["doc", "otherwise", "then", "with", "sharing", "where", "do", "row", "source"]));
  const pyret_single_punctuation =
    new RegExp("^([" + [":", ".", "<", ">", ",", "^", "!",
                        ";", "|", "=", "+", "*", "/", "\\\\", // NOTE: No minus
                        "(", ")", "{", "}", "\\[", "\\]"].join('') + "])");
  const pyret_double_punctuation =
    new RegExp("^((" + ["<=>", "::", "=~", "==", ">=", "<=", "=>", "->", ":=", "<>"].join(")|(") + "))");
  const initial_operators = { "-": true, "+": true, "*": true, "/": true, "<": true, "<=": true,
                              ">": true, ">=": true, "==": true, "<>": true, ".": true, "^": true,
                              "<=>": true, "=~": true,
                              "is": true, "is==": true, "is=~": true, "is<=>": true, "because": true,
                              "is-roughly": true, "is-not": true, "is-not==": true, "is-not=~": true, "is-not<=>": true,
                              "satisfies": true, "violates": true, "raises": true, "raises-other-than": true,
                              "does-not-raise": true, "raises-satisfies": true, "raises-violates": true
                            };

  const pyret_delimiter_type = {NONE : 0,         // Not a delimiter token
                                OPENING : 1,      // Opening token (e.g. "fun", "{")
                                CLOSING : 2,      // Closing token (e.g. "end", "}")
                                SUBKEYWORD : 3,   // Subkeyword (e.g. "else if")
                                OPEN_CONTD : 4,   // Extension of opening keyword (e.g. ":")
                                CLOSE_CONTD : 5,  // Extension of closing keyword (UNUSED)
                                SUB_CONTD : 6,    // Extension of subkeyword (i.e. colon after "else if")
                                FOLD_OPEN_CONTD : 7}; // Extension of opening keyword (acts like OPEN_CONTD *when folding*)

  // Contexts in which function-names can be unprefixed
  // (i.e. no "fun" or "method")
  const pyret_unprefixed_contexts = [];

  // Subkeywords each token can have
  const pyret_subkeywords = {
    "if": ["block", "else if", "else"], "when": ["block"],
    "fun": ["block", "where"], "method": ["block", "where"], "lam": ["block"],
    "for": ["block", "do"], "let": ["block"], "letrec": ["block"], "type-let": ["block"],
    "cases": ["block"], "ask": ["block", "then", "otherwise"],
    "data": ["sharing", "where"], "table": ["row"], "load-table": ["sanitize", "source"]
  };

  // Subkeywords which cannot be followed by any other keywords
  const pyret_last_subkeywords = {
    "if": "else"
  };

  // Tokens with closing tokens other than "end" or ";"
  const pyret_special_delimiters = [{start: "(", end: ")"},
                                    {start: "[", end: "]"},
                                    {start: "{", end: "}"},
                                    {start: "provide", end: "*"}];

  function ret(state, tokType, content, style) {
    state.lastToken = tokType; state.lastContent = content;
    //console.log("Token:", state, tokType, content, style);
    return style;
  }


  function tokenBase(stream, state) {
    if (stream.eatSpace())
      return "IGNORED-SPACE";

    var ch = stream.peek();


    // Handle Comments
    if (ch === '#') {
      if (stream.match("#|", true)) {
        state.tokenizer = tokenizeBlockComment;
        state.commentNestingDepth = 1;
        return ret(state, "COMMENT-START", state.lastContent, 'comment');
      } else {
        stream.skipToEnd();
        return ret(state, "COMMENT", state.lastContent, 'comment');
      }
    }

    // Handle Number Literals
    const unsigned_decimal_part = "[0-9]+(?:\\.[0-9]+)?(?:[eE][-+]?[0-9]+)?";
    const unsigned_rational_part = "[0-9]+/[0-9]+"; 
    const number = new RegExp("^[-+]?" + unsigned_decimal_part);
    const badNumber = new RegExp("^~?[+-]?\\.[0-9]+(?:[eE][-+]?[0-9]+)?");
    const roughnum = new RegExp("^~[-+]?"  + "(?:" + unsigned_rational_part + "|" + unsigned_decimal_part + ")");
    const rational = new RegExp("^[-+]?" + unsigned_rational_part);
    if (stream.match(roughnum))
      return ret(state, 'number', stream.current(), 'roughnum');
    else if (stream.match(rational))
      return ret(state, 'number', stream.current(), 'number');
    else if (stream.match(number))
      return ret(state, 'number', stream.current(), 'number');
    else if (stream.match(badNumber))
      return ret(state, 'number', stream.current(), 'bad-number');

    // if (ch === '"') {
    //   state.tokenizer = tokenStringDouble;
    //   state.lastToken = '"';
    //   stream.eat('"');
    //   return state.tokenizer(stream, state);
    // }
    // if (ch === "'") {
    //   state.tokenizer = tokenStringSingle;
    //   state.lastToken = "'";
    //   stream.eat("'");
    //   return state.tokenizer(stream, state);
    // }
    const dquot_str =
      new RegExp("^\"(?:" +
                 "\\\\[01234567]{1,3}" +
                 "|\\\\x[0-9a-fA-F]{1,2}" +
                 "|\\\\u[0-9a-fA-f]{1,4}" +
                 "|\\\\[\\\\nrt\"\']" +
                 "|[^\\\\\"\n\r])*\"");
    const squot_str =
      new RegExp("^\'(?:" +
                 "\\\\[01234567]{1,3}" +
                 "|\\\\x[0-9a-fA-F]{1,2}" +
                 "|\\\\u[0-9a-fA-f]{1,4}" +
                 "|\\\\[\\\\nrt\"\']" +
                 "|[^\\\\\'\n\r])*\'");
    const unterminated_string = new RegExp("^[\"\'].*");

    var match;
    state.maybeShorthandLambda = false;
    if ((match = stream.match(dquot_str, true))) {
      return ret(state, 'string', match[0], 'string');
    } else if ((match = stream.match(squot_str, true))) {
      return ret(state, 'string', match[0], 'string');
    } else if (stream.match(/^```/, true)) {
      state.tokenizer = tokenStringTriple;
      state.inString = stream.column();
      state.lastToken = '```';
      return state.tokenizer(stream, state);
    } else if ((match = stream.match(unterminated_string, true))) {
      return ret(state, 'string', match[0], 'unterminated-string');
    } else if ((match = stream.match(/^\.\.\./, true))) {
      return ret(state, match[0], match[0], 'builtin');
    }
    // Level 1
    if ((match = stream.match(/^({(?=\())/, true))) {
      state.maybeShorthandLambda = true;
      return ret(state, '{', '{', 'builtin');
    }
    if ((match = stream.match(pyret_double_punctuation, true)) ||
        (match = stream.match(pyret_single_punctuation, true))) {
      if (state.dataNoPipeColon && (match[0] == ":" || match[0] == "|"))
        state.dataNoPipeColon = false;
      return ret(state, match[0], match[0], 'builtin');
    }
    if ((match = stream.match(pyret_keywords_hyphen, true))) {
      return ret(state, match[0], match[0], 'keyword');
    }
    if ((match = stream.match(pyret_keywords, true))) {
      if (match[0] == "data")
        state.dataNoPipeColon = true;
      return ret(state, match[0], match[0], 'keyword');
    }
    if ((match = stream.match(pyret_booleans, true))) {
      return ret(state, match[0], match[0], 'boolean');
    }
    if ((match = stream.match(pyret_keywords_colon, true))) {
      if (stream.peek() === ":")
        return ret(state, match[0], match[0], 'keyword');
      else
        return ret(state, 'name', match[0], 'variable');
    }
    // Level 2
    if ((match = stream.match(pyret_indent_regex))) {
      if (state.lastToken === "|" || state.lastToken === "::" || state.lastToken === "data"
          || state.dataNoPipeColon) {
        state.dataNoPipeColon = false;
        return ret(state, 'name', match[0], 'type');
      }
      else if (stream.match(/\s*\(/, false))
        return ret(state, 'name', match[0], 'function-name');
      return ret(state, 'name', match[0], 'variable');
    }
    if (stream.eat("-"))
      return ret(state, '-', '-', 'builtin');
    stream.next();
    return null;
  }

  function mkTokenString(singleOrDouble) {
    return function(stream, state) {
      var insideRE = singleOrDouble === "'" ? new RegExp("[^'\\]") : new RegExp('[^"\\]');
      var endRE = singleOrDouble === "'" ? new RegExp("'") : new RegExp('"');
      while (!stream.eol()) {
        stream.eatWhile(insideRE);
        if (stream.eat('\\')) {
          stream.next();
          if (stream.eol())
            return ret(state, 'string', stream.current(), 'string');
        } else if (stream.eat(singleOrDouble)) {
          state.tokenizer = tokenBase;
          return ret(state, 'string', stream.current(), 'string');
        } else
          stream.eat(endRE);
      }
      return ret(state, 'string', stream.current(), 'string');
    };
  }

  function tokenizeBlockComment(stream, state) {
    if (stream.match('#|', true)) {
      state.commentNestingDepth++;
      return ret(state, "COMMENT-START", state.lastContent, 'comment');
    } else if (stream.match('|#', true)) {
      state.commentNestingDepth--;
      if (state.commentNestingDepth === 0) state.tokenizer = tokenBase;
      return ret(state, "COMMENT-END", state.lastContent, 'comment');
    } else {
      stream.next(); stream.eatWhile(/[^#|]/);
      return ret(state, "COMMENT", state.lastContent, 'comment');
    }
  }

  var tokenStringDouble = mkTokenString('"');
  var tokenStringSingle = mkTokenString("'");

  function tokenStringTriple(stream, state) {
    while (!stream.eol()) {
      stream.match(/[^`\\]*/, true); //eatWhile(/[^`\\]|`{1,2}([^`\\]|(?:\\))/);
      if (stream.eat('\\')) {
        stream.next();
        if (stream.eol()) {
          return ret(state, 'string', stream.current(), 'string');
        }
      } else if (stream.match('```', true)) {
        state.tokenizer = tokenBase;
        state.inString = false;
        return ret(state, 'string', stream.current(), 'string');
      } else
        stream.next();
    }
    return ret(state, 'string', stream.current(), 'string');
  }

  // Parsing

  function Indent(funs, cases, data, shared, trys, except, graph, parens, objects, vars, fields, initial, comments) {
    this.fn = funs || 0;
    this.c = cases || 0;
    this.d = data || 0;
    this.s = shared || 0;
    this.t = trys || 0;
    this.e = except || 0;
    this.g = graph || 0;
    this.p = parens || 0;
    this.o = objects || 0;
    this.v = vars || 0;
    this.f = fields || 0;
    this.i = initial || 0;
    this.comments = comments || 0;
  }
  Indent.prototype.toString = function() {
    return ("Fun " + this.fn + ", Cases " + this.c + ", Data " + this.d + ", Shared " + this.s
            + ", Try " + this.t + ", Except " + this.e + ", Graph " + this.g + ", Parens " + this.p
            + ", Object " + this.o + ", Vars " + this.v + ", Fields " + this.f + ", Initial " + this.i
            + ", Comment depth " + this.comments);
  };
  Indent.prototype.copy = function() {
    return new Indent(this.fn, this.c, this.d, this.s, this.t, this.e, this.g,
                      this.p, this.o, this.v, this.f, this.i, this.comments);
  };
  Indent.prototype.zeroOut = function() {
    this.fn = this.c = this.d = this.s = this.t = this.e = this.g = this.p = this.o = this.v = this.f = this.i = this.comments = 0;
  };
  Indent.prototype.addSelf = function(that) {
    this.fn += that.fn; this.c += that.c; this.d += that.d; this.s += that.s; this.t += that.t; this.e += that.e;
    this.g += that.g; this.p += that.p; this.o += that.o; this.v += that.v; this.f += that.f; this.i += that.i;
    this.comments += that.comments;
    return this;
  };
  Indent.prototype.add = function(that) { return this.copy().addSelf(that); };
  Indent.prototype.subSelf = function(that) {
    this.fn -= that.fn; this.c -= that.c; this.d -= that.d; this.s -= that.s; this.t -= that.t; that.e -= that.e;
    this.g -= that.g; this.p -= that.p; this.o -= that.o; this.v -= that.v; this.f -= that.f; this.i -= that.i;
    this.comments -= that.comments;
    return this;
  };
  Indent.prototype.sub = function(that) { return this.copy().subSelf(that); };

  function LineState(tokens,
                     nestingsAtLineStart, nestingsAtLineEnd,
                     deferedOpened, curOpened, deferedClosed, curClosed, delimType) {
    this.tokens = tokens;
    this.nestingsAtLineStart = nestingsAtLineStart;
    this.nestingsAtLineEnd = nestingsAtLineEnd;
    this.deferedOpened = deferedOpened;
    this.curOpened = curOpened;
    this.deferedClosed = deferedClosed;
    this.curClosed = curClosed;
    this.delimType = delimType;
  }
  LineState.prototype.copy = function() {
    return new LineState(this.tokens.concat([]),
                         this.nestingsAtLineStart.copy(), this.nestingsAtLineEnd.copy(),
                         this.deferedOpened.copy(), this.curOpened.copy(),
                         this.deferedClosed.copy(), this.curClosed.copy(), this.delimType);
  };
  LineState.prototype.print = function() {
    console.log("LineState is:");
    console.log("  NestingsAtLineStart = " + this.nestingsAtLineStart);
    console.log("  NestingsAtLineEnd = " + this.nestingsAtLineEnd);
    console.log("  DeferedOpened = " + this.deferedOpened);
    console.log("  DeferedClosed = " + this.deferedClosed);
    console.log("  CurOpened = " + this.curOpened);
    console.log("  CurClosed = " + this.curClosed);
    console.log("  Tokens = " + this.tokens);
  };

  function peek(arr) { return arr[arr.length - 1]; }
  function hasTop(arr, wanted) {
    if (wanted instanceof Array) {
      for (var i = 0; i < wanted.length; i++) {
        if (arr[arr.length - 1 - i] !== wanted[i]) {
          return false;
        }
      }
      return true;
    } else {
      return arr[arr.length - 1] === wanted;
    }
  }
  // Unused, but temporarily leaving in until
  // we are positive that unprefixed function
  // definitions will never appear again
  function isUnprefixedContext(ctx) {
    if (ctx.length === 0)
      return false;
    // FIXME
    for (var i = 0; i < pyret_unprefixed_contexts.length; i++) {
      if (pyret_unprefixed_contexts[i] === ctx[ctx.length - 1])
        return true;
    }
    return false;
    //return Array.prototype.includes.bind(pyret_unprefixed_contexts).call(ctx[ctx.length - 1]);
    //return pyret_unprefixed_contexts.includes(ctx[ctx.length - 1]);
  }
  function parse(firstTokenInLine, state, stream, style) {
    ls = state.lineState;
    // Sometimes we want to pick a delimiter type based on the
    // previous token's type
    var inOpening = ls.delimType === pyret_delimiter_type.OPENING
        || ls.delimType === pyret_delimiter_type.OPEN_CONTD;
    var inSubkw = ls.delimType === pyret_delimiter_type.SUBKEYWORD
        || ls.delimType === pyret_delimiter_type.SUB_CONTD;
    ls.delimType = pyret_delimiter_type.NONE;
    if (firstTokenInLine) {
      ls.nestingsAtLineStart = ls.nestingsAtLineEnd.copy();
    }
    // Special case: period-separated names in for <func>(...) expressions
    // Philip: disabling for now, since this is only useful for staying visible when folding...
    //         ...not to mention that it's been broken for who-knows-how-long and no one has
    //         noticed since it's not even really used
    /*if ((state.lastToken === "name" || state.lastToken === ".") && hasTop(ls.tokens, ["WANTCOLONORBLOCK", "FOR"])) {
      if (inOpening)
        ls.delimType = pyret_delimiter_type.OPEN_CONTD;
    }*/
    // Special case: don't hide function-names when folding
    if ((state.lastToken === "name") && (style === 'function-name')
        && (hasTop(ls.tokens, ["WANTOPENPAREN", "WANTCLOSEPAREN", "WANTCOLONORBLOCK", "FUN"]))) {
      if (inOpening) // Slightly redundant, but let's be safe
        ls.delimType = pyret_delimiter_type.FOLD_OPEN_CONTD;
    }
    // Uncomment if pyret_unprefixed_contexts is ever used again
    /*if (state.lastToken === "name" && style === 'function-name' && isUnprefixedContext(ls.tokens)) {
      ls.delimType = pyret_delimiter_type.OPENING;
    }*/
    if (ls.nestingsAtLineStart.comments > 0 || ls.curOpened.comments > 0 || ls.deferedOpened.comments > 0) {
      if (state.lastToken === "COMMENT-END") {
        if (ls.curOpened.comments > 0) ls.curOpened.comments--;
        else if (ls.deferedOpened.comments > 0) ls.deferedOpened.comments--;
        else if (firstTokenInLine) ls.curClosed.comments++;
        else ls.deferedClosed.comments++;
      } else if (state.lastToken === "COMMENT-START") {
        ls.deferedOpened.comments++;
      }
    } else if (state.lastToken === "COMMENT-START") {
      ls.deferedOpened.comments++;
    } else if (state.lastToken === "COMMENT") {
      // nothing to do
    } else if (hasTop(ls.tokens, "NEEDSOMETHING")) {
      ls.tokens.pop();
      if (hasTop(ls.tokens, "VAR") && ls.deferedOpened.v > 0) {
        ls.deferedOpened.v--;
        ls.tokens.pop();
      }
      parse(firstTokenInLine, state, stream, style); // keep going; haven't processed token yet
    } else if (firstTokenInLine &&
               ((initial_operators[state.lastToken] && (state.lastToken == "." || stream.match(/^\s+/)))
                || (state.lastToken === "is" && stream.match(/^%/))
                || (state.lastToken === "is-not" && stream.match(/^%/)))) {
      ls.curOpened.i++;
      ls.deferedClosed.i++;
    } else if (state.lastToken === ":") {
      if (inOpening)
        ls.delimType = pyret_delimiter_type.OPEN_CONTD;
      else if (inSubkw)
        ls.delimType = pyret_delimiter_type.SUB_CONTD;
      if (hasTop(ls.tokens, "WANTCOLON")
          || hasTop(ls.tokens, "WANTCOLONOREQUAL")
          || hasTop(ls.tokens, "WANTCOLONORBLOCK"))
        ls.tokens.pop();
      else if (hasTop(ls.tokens, "OBJECT")
               || hasTop(ls.tokens, "REACTOR")
               || hasTop(ls.tokens, "SHARED")
               || hasTop(ls.tokens, "BRACEDEXPR")
               || hasTop(ls.tokens, "BRACEDEXPR_NOLAMBDA")) {
        if (hasTop(ls.tokens, "BRACEDEXPR") || hasTop(ls.tokens, "BRACEDEXPR_NOLAMBDA")) {
          ls.tokens.pop();
          ls.tokens.push("OBJECT");
        }
        ls.deferedOpened.f++;
        ls.tokens.push("FIELD", "NEEDSOMETHING");
      }
    } else if (state.lastToken === ";") {
      if (hasTop(ls.tokens, "BRACEDEXPR") || hasTop(ls.tokens, "BRACEDEXPR_NOLAMBDA")) {
        ls.tokens.pop();
        ls.tokens.push("TUPLE");
      }
    } else if (state.lastToken === "::") {
      if (hasTop(ls.tokens, "OBJECT") || hasTop(ls.tokens, "SHARED")) {
        ls.deferedOpened.f++;
        ls.tokens.push("FIELD", "NEEDSOMETHING");
      }
    } else if (state.lastToken === ",") {
      if (hasTop(ls.tokens, "FIELD")) {
        ls.tokens.pop();
        if (ls.curOpened.f > 0) ls.curOpened.f--;
        else if (ls.deferedOpened.f > 0) ls.deferedOpened.f--;
        else ls.deferedClosed.f++;
      }
    } else if (state.lastToken === "=") {
      if (hasTop(ls.tokens, "WANTCOLONOREQUAL"))
        ls.tokens.pop();
      else {
        while (hasTop(ls.tokens, "VAR")) {
          ls.tokens.pop();
          ls.curClosed.v++;
        }
        ls.deferedOpened.v++;
        ls.tokens.push("VAR", "NEEDSOMETHING");
      }
    } else if (state.lastToken === "var" || state.lastToken === "rec") {
      ls.deferedOpened.v++;
      ls.tokens.push("VAR", "NEEDSOMETHING", "WANTCOLONOREQUAL");
    } else if (state.lastToken === "fun" || state.lastToken === "method" || state.lastToken === "lam") {
      ls.delimType = pyret_delimiter_type.OPENING;
      ls.deferedOpened.fn++;
      ls.tokens.push("FUN", "WANTCOLONORBLOCK", "WANTCLOSEPAREN", "WANTOPENPAREN");
    } else if (state.lastToken === "method") {
      if (hasTop(ls.tokens, "BRACEDEXPR") || hasTop(ls.tokens, "BRACEDEXPR_NOLAMBDA")) {
        ls.tokens.pop();
        ls.tokens.push("OBJECT");
      }
      ls.delimType = pyret_delimiter_type.OPENING;
      ls.deferedOpened.fn++;
      ls.tokens.push("FUN", "WANTCOLONORBLOCK", "WANTCLOSEPAREN", "WANTOPENPAREN");
    } else if (state.lastToken === "let" || state.lastToken === "letrec" || state.lastToken === "type-let") {
      ls.delimType = pyret_delimiter_type.OPENING;
      ls.deferedOpened.fn++;
      ls.tokens.push("LET", "WANTCOLONORBLOCK");
    } else if (state.lastToken === "when") {
      ls.delimType = pyret_delimiter_type.OPENING;
      ls.deferedOpened.fn++; // when indents like functions
      ls.tokens.push("WHEN", "WANTCOLONORBLOCK");
    } else if (state.lastToken === "do") {
      if (hasTop(ls.tokens, "DO")) {
        if (ls.curOpened.fn > 0) ls.curOpened.fn--;
        else if (ls.deferedOpened.fn > 0) ls.deferedOpened.fn--;
        else ls.curClosed.fn++;
        ls.deferedOpened.fn++;
        ls.tokens.push("WHEN", "WANTCOLON");
        ls.delimType = pyret_delimiter_type.SUBKEYWORD;
      }
    } else if (state.lastToken === "for") {
      ls.delimType = pyret_delimiter_type.OPENING;
      ls.deferedOpened.fn++; // for-loops indent like functions
      ls.tokens.push("FOR", "WANTCOLONORBLOCK");
    } else if (state.lastToken === "cases") {
      ls.delimType = pyret_delimiter_type.OPENING;
      ls.deferedOpened.c++;
      ls.tokens.push("CASES", "WANTCOLONORBLOCK", "WANTCLOSEPAREN", "WANTOPENPAREN");
    } else if (state.lastToken === "data") {
      ls.delimType = pyret_delimiter_type.OPENING;
      ls.deferedOpened.d++;
      ls.tokens.push("DATA", "WANTCOLON", "NEEDSOMETHING");
    } else if (state.lastToken === "ask") {
      ls.delimType = pyret_delimiter_type.OPENING;
      ls.deferedOpened.c++;
      ls.tokens.push("IFCOND", "WANTCOLONORBLOCK");
    } else if (state.lastToken === "spy") {
      ls.delimType = pyret_delimiter_type.OPENING;
      ls.deferedOpened.fn++;
      ls.tokens.push("SPY", "WANTCOLON");
    } else if (state.lastToken === "if") {
      ls.delimType = pyret_delimiter_type.OPENING;
      ls.deferedOpened.fn++;
      ls.tokens.push("IF", "WANTCOLONORBLOCK", "NEEDSOMETHING");
    } else if (state.lastToken === "else if") {
      if (hasTop(ls.tokens, "IF")) {
        if (ls.curOpened.fn > 0) ls.curOpened.fn--;
        else if (ls.deferedOpened.fn > 0) ls.deferedOpened.fn--;
        else ls.curClosed.fn++;
        ls.deferedOpened.fn++;
        ls.tokens.push("WANTCOLON", "NEEDSOMETHING");
        ls.delimType = pyret_delimiter_type.SUBKEYWORD;
      }
    } else if (state.lastToken === "else") {
      if (hasTop(ls.tokens, "IF")) {
        if (ls.curOpened.fn > 0) ls.curOpened.fn--;
        else if (ls.deferedOpened.fn > 0) ls.deferedOpened.fn--;
        else ls.curClosed.fn++;
        ls.deferedOpened.fn++;
        ls.tokens.push("WANTCOLON");
        ls.delimType = pyret_delimiter_type.SUBKEYWORD;
      }
    } else if (state.lastToken === "row") {
      if (hasTop(ls.tokens, "TABLEROW")) {
        if (ls.curOpened.fn > 0) ls.curOpened.fn--;
        else if (ls.deferedOpened.fn > 0) ls.deferedOpened.fn--;
        else ls.curClosed.fn++;
        ls.deferedOpened.fn++;
        ls.tokens.push("WANTCOLON");
        ls.delimType = pyret_delimiter_type.SUBKEYWORD;
      } else if (hasTop(ls.tokens, "TABLE")) {
        ls.deferedOpened.fn++;
        ls.tokens.push("TABLEROW", "WANTCOLON");
        ls.delimType = pyret_delimiter_type.SUBKEYWORD;
      }
    } else if (state.lastToken === "source") {
      if (hasTop(ls.tokens, "LOADTABLESPEC")) {
        if (ls.curOpened.fn > 0) ls.curOpened.fn--;
        else if (ls.deferedOpened.fn > 0) ls.deferedOpened.fn--;
        else ls.curClosed.fn++;
        ls.deferedOpened.fn++;
        ls.tokens.push("NEEDSOMETHING", "WANTCOLON");
        ls.delimType = pyret_delimiter_type.SUBKEYWORD;
      } else if (hasTop(ls.tokens, "LOADTABLE")) {
        ls.deferedOpened.fn++;
        ls.tokens.push("LOADTABLESPEC", "NEEDSOMETHING", "WANTCOLON");
        ls.delimType = pyret_delimiter_type.SUBKEYWORD;
      }
    } else if (state.lastToken === "sanitize") {
      if (hasTop(ls.tokens, "LOADTABLESPEC")) {
        if (ls.curOpened.fn > 0) ls.curOpened.fn--;
        else if (ls.deferedOpened.fn > 0) ls.deferedOpened.fn--;
        else ls.curClosed.fn++;
        ls.deferedOpened.fn++;
        ls.tokens.push("NEEDSOMETHING");
        ls.delimType = pyret_delimiter_type.SUBKEYWORD;
      } else if (hasTop(ls.tokens, "LOADTABLE")) {
        ls.deferedOpened.fn++;
        ls.tokens.push("LOADTABLESPEC", "NEEDSOMETHING");
        ls.delimType = pyret_delimiter_type.SUBKEYWORD;
      }
    } else if (state.lastToken === "|") {
      if (hasTop(ls.tokens, ["OBJECT", "DATA"]) || hasTop(ls.tokens, ["FIELD", "OBJECT", "DATA"])) {
        //ls.curClosed.o++;
        if (hasTop(ls.tokens, "FIELD")) {
          ls.tokens.pop();
          if (ls.curOpened.f > 0) ls.curOpened.f--;
          else if (ls.deferedOpened.f > 0) ls.deferedOpened.f--;
          else ls.curClosed.f++;
        }
        if (hasTop(ls.tokens, "OBJECT"))
          ls.tokens.pop();
      } else if (hasTop(ls.tokens, "DATA"))
        ls.tokens.push("NEEDSOMETHING");
    } else if (state.lastToken === "with") {
      if (hasTop(ls.tokens, ["WANTOPENPAREN", "WANTCLOSEPAREN", "DATA"])) {
        ls.tokens.pop(); ls.tokens.pop();
        ls.tokens.push("OBJECT", "WANTCOLON");
      } else if (hasTop(ls.tokens, ["DATA"])) {
        ls.tokens.push("OBJECT", "WANTCOLON");
      }
    } else if (state.lastToken === "provide") {
      ls.tokens.push("PROVIDE");
      ls.delimType = pyret_delimiter_type.OPENING;
      ls.deferedOpened.s++;
    } else if (state.lastToken === "sharing") {
      ls.curClosed.d++; ls.deferedOpened.s++;
      ls.delimType = pyret_delimiter_type.SUBKEYWORD;
      if (hasTop(ls.tokens, ["FIELD", "OBJECT", "DATA"])) {
        ls.tokens.pop(); ls.tokens.pop(); ls.tokens.pop();
        ls.curClosed.o++;
        ls.tokens.push("SHARED", "WANTCOLON");
      } else if (hasTop(ls.tokens, ["OBJECT", "DATA"])) {
        ls.tokens.pop(); ls.tokens.pop();
        //ls.curClosed.o++;
        ls.tokens.push("SHARED", "WANTCOLON");
      } else if (hasTop(ls.tokens, "DATA")) {
        ls.tokens.pop();
        ls.tokens.push("SHARED", "WANTCOLON");
      }
    } else if (state.lastToken === "where" || (state.lastToken === "examples" && ls.tokens.length > 0)) {
      ls.delimType = (state.lastToken === "where") ? pyret_delimiter_type.SUBKEYWORD
                                                   : pyret_delimiter_type.OPENING;
      if (hasTop(ls.tokens, ["FIELD", "OBJECT", "DATA"])) {
        ls.tokens.pop(); ls.tokens.pop();
        ls.curClosed.o++;
        ls.curClosed.d++; ls.deferedOpened.s++;
      } else if (hasTop(ls.tokens, ["OBJECT", "DATA"])) {
        ls.tokens.pop();
        // ls.curClosed.o++;
        ls.curClosed.d++; ls.deferedOpened.s++;
      } else if (hasTop(ls.tokens, "DATA")) {
        ls.curClosed.d++; ls.deferedOpened.s++;
      } else if (hasTop(ls.tokens, "FUN")) {
        ls.curClosed.f++; ls.deferedOpened.s++;
      } else if (hasTop(ls.tokens, "SHARED")) {
        ls.curClosed.s++; ls.deferedOpened.s++;
      }
      ls.tokens.pop();
      ls.tokens.push("CHECK", "WANTCOLON");
    } else if (state.lastToken === "check" || (state.lastToken === "examples" && ls.tokens.length === 0)) {
      ls.delimType = pyret_delimiter_type.OPENING;
      ls.deferedOpened.s++;
      ls.tokens.push("CHECK", "WANTCOLON");
    } else if (state.lastToken === "try") {
      ls.delimType = pyret_delimiter_type.OPENING;
      ls.deferedOpened.t++;
      ls.tokens.push("TRY", "WANTCOLON");
    } else if (state.lastToken === "except") {
      ls.delimType = pyret_delimiter_type.OPENING;
      if (ls.curOpened.t > 0) ls.curOpened.t--;
      else if (ls.deferedOpened.t > 0) ls.deferedOpened.t--;
      else ls.curClosed.t++;
      if (hasTop(ls.tokens, "TRY")) {
        ls.tokens.pop();
        ls.tokens.push("WANTCOLON", "WANTCLOSEPAREN", "WANTOPENPAREN");
      }
    } else if (state.lastToken === "then" || state.lastToken === "otherwise") {
      ls.delimType = pyret_delimiter_type.SUBKEYWORD;
    } else if (state.lastToken === "block") {
      if (hasTop(ls.tokens, "WANTCOLONORBLOCK")) {
        ls.delimType = pyret_delimiter_type.SUBKEYWORD;
      } else {
        ls.deferedOpened.fn++;
        ls.tokens.push("BLOCK", "WANTCOLON");
        ls.delimType = pyret_delimiter_type.OPENING;
      }
    } else if (state.lastToken === "reactor") {
      ls.delimType = pyret_delimiter_type.OPENING;
      ls.deferedOpened.fn++;
      ls.tokens.push("REACTOR", "WANTCOLON");
    } else if (state.lastToken === "table") {
      ls.delimType = pyret_delimiter_type.OPENING;
      ls.deferedOpened.fn++;
      ls.tokens.push("TABLE", "WANTCOLON");
    } else if (state.lastToken === "load-table") {
      ls.delimType = pyret_delimiter_type.OPENING;
      ls.deferedOpened.fn++;
      ls.tokens.push("LOADTABLE", "WANTCOLON");
    } else if (state.lastToken === "select") {
      ls.delimType = pyret_delimiter_type.OPENING;
      ls.deferedOpened.fn++;
      ls.tokens.push("SELECT", "WANTCOLON");
    } else if (state.lastToken === "extend") {
      ls.delimType = pyret_delimiter_type.OPENING;
      ls.deferedOpened.fn++;
      ls.tokens.push("EXTEND", "WANTCOLON");
    } else if (state.lastToken === "transform") {
      ls.delimType = pyret_delimiter_type.OPENING;
      ls.deferedOpened.fn++;
      ls.tokens.push("TRANSFORM", "WANTCOLON");
    } else if (state.lastToken === "extract") {
      ls.delimType = pyret_delimiter_type.OPENING;
      ls.deferedOpened.fn++;
      ls.tokens.push("EXTRACT", "WANTCOLON");
    } else if (state.lastToken === "sieve") {
      ls.delimType = pyret_delimiter_type.OPENING;
      ls.deferedOpened.fn++;
      ls.tokens.push("SIEVE", "WANTCOLON");
    } else if (state.lastToken === "order") {
      ls.delimType = pyret_delimiter_type.OPENING;
      ls.deferedOpened.fn++;
      ls.tokens.push("ORDER", "WANTCOLON");
    } else if (state.lastToken === "ref-graph") {
      ls.deferedOpened.g++;
      ls.tokens.push("GRAPH", "WANTCOLON");
    } else if (state.lastToken === "[") {
      ls.deferedOpened.o++;
      ls.tokens.push("ARRAY");
      ls.delimType = pyret_delimiter_type.OPENING;
    } else if (state.lastToken === "]") {
      ls.delimType = pyret_delimiter_type.CLOSING;
      if (firstTokenInLine) ls.curClosed.o++;
      else ls.deferedClosed.o++;
      if (hasTop(ls.tokens, "ARRAY"))
        ls.tokens.pop();
      while (hasTop(ls.tokens, "VAR")) {
        ls.tokens.pop();
        ls.deferedClosed.v++;
      }
    } else if (state.lastToken === "{") {
      ls.deferedOpened.o++;
      if (state.maybeShorthandLambda)
        ls.tokens.push("BRACEDEXPR");
      else
        ls.tokens.push("BRACEDEXPR_NOLAMBDA");
      ls.delimType = pyret_delimiter_type.OPENING;
    } else if (state.lastToken === "}") {
      ls.delimType = pyret_delimiter_type.CLOSING;
      if (firstTokenInLine) ls.curClosed.o++;
      else ls.deferedClosed.o++;
      if (hasTop(ls.tokens, "FIELD")) {
        ls.tokens.pop();
        if (ls.curOpened.f > 0) ls.curOpened.f--;
        else if (ls.deferedOpened.f > 0) ls.deferedOpened.f--;
        else ls.curClosed.f++;
      }
      if (hasTop(ls.tokens, "OBJECT")
          || hasTop(ls.tokens, "BRACEDEXPR")
          || hasTop(ls.tokens, "BRACEDEXPR_NOLAMBDA")
          || hasTop(ls.tokens, "TUPLE")
          || hasTop(ls.tokens, "SHORTHANDLAMBDA"))
        ls.tokens.pop();
      while (hasTop(ls.tokens, "VAR")) {
        ls.tokens.pop();
        ls.deferedClosed.v++;
      }
    } else if (state.lastToken === "(") {
      ls.delimType = pyret_delimiter_type.OPENING;
      ls.deferedOpened.p++;
      if (hasTop(ls.tokens, "WANTOPENPAREN")) {
        ls.tokens.pop();
      } else if (hasTop(ls.tokens, "BRACEDEXPR")) {
        ls.tokens.pop();
        ls.tokens.push("SHORTHANDLAMBDA", "WANTCOLONORBLOCK");
      } else if (hasTop(ls.tokens, "OBJECT") || hasTop(ls.tokens, "SHARED")) {
        ls.tokens.push("FUN", "WANTCOLONORBLOCK");
        ls.deferedOpened.fn++;
      } else {
        ls.tokens.push("WANTCLOSEPAREN");
      }
    } else if (state.lastToken === ")") {
      ls.delimType = pyret_delimiter_type.CLOSING;
      if (ls.curOpened.p > 0) { ls.curOpened.p--; }
      else if (ls.deferedOpened.p > 0) { ls.deferedOpened.p--; }
      else {ls.deferedClosed.p++; }
      if (hasTop(ls.tokens, "WANTCLOSEPAREN"))
        ls.tokens.pop();
      while (hasTop(ls.tokens, "VAR")) {
        ls.tokens.pop();
        ls.deferedClosed.v++;
      }
    } else if (state.lastToken === "end") {
      ls.delimType = pyret_delimiter_type.CLOSING;
      if (hasTop(ls.tokens, ["FIELD", "OBJECT", "DATA"])) {
        /* Handles situations such as
         * data A:
         *   | a with:
         *     b : 2 # <- indents as an object field
         * end
         */
        ls.curClosed.f++;
        ls.tokens.pop();
        ls.tokens.pop();
      } else if (hasTop(ls.tokens, ["OBJECT", "DATA"])) {
        //ls.curClosed.o++;
        ls.tokens.pop();
      } else if (hasTop(ls.tokens, ["TABLEROW", "TABLE"])
                 || hasTop(ls.tokens, ["LOADTABLESPEC", "LOADTABLE"])) {
        ls.tokens.pop();
        ls.curClosed.o++;
      }
      var top = peek(ls.tokens);
      var stillUnclosed = true;
      while (stillUnclosed && ls.tokens.length) {
        // Things that are not counted at all:
        //   wantcolon, wantcolonorequal, needsomething, wantopenparen
        // Things that are counted but not closable by end:
        if (top === "OBJECT" || top === "ARRAY") {
          if (ls.curOpened.o > 0) ls.curOpened.o--;
          else if (ls.deferedOpened.o > 0) ls.deferedOpened.o--;
          else ls.curClosed.o++;
        } else if (top === "WANTCLOSEPAREN") {
          if (ls.curOpened.p > 0) ls.curOpened.p--;
          else if (ls.deferedOpened.p > 0) ls.deferedOpened.p--;
          else ls.curClosed.p++;
        } else if (top === "FIELD") {
          if (ls.curOpened.f > 0) ls.curOpened.f--;
          else if (ls.deferedOpened.f > 0) ls.deferedOpened.f--;
          else ls.curClosed.f++;
        } else if (top === "VAR") {
          if (ls.curOpened.v > 0) ls.curOpened.v--;
          else if (ls.deferedOpened.v > 0) ls.deferedOpened.v--;
          else ls.curClosed.v++;
        } else if (top === "PROVIDE") {
          if (ls.curOpened.s > 0) ls.curOpened.s--;
          else if (ls.deferedOpened.s > 0) ls.deferedOpened.s--;
          else ls.curClosed.s++;
        }
        // Things that are counted, and closable by end:
        else if (pyret_openers_closed_by_end[top] === true) {
          if (ls.curOpened.fn > 0) ls.curOpened.fn--;
          else if (ls.deferedOpened.fn > 0) ls.deferedOpened.fn--;
          else ls.curClosed.fn++;
          stillUnclosed = false;
        } else if (top === "CASES" || top === "IFCOND") {
          if (ls.curOpened.c > 0) ls.curOpened.c--;
          else if (ls.deferedOpened.c > 0) ls.deferedOpened.c--;
          else ls.curClosed.c++;
          stillUnclosed = false;
        } else if (top === "DATA") {
          if (ls.curOpened.d > 0) ls.curOpened.d--;
          else if (ls.deferedOpened.d > 0) ls.deferedOpened.d--;
          else ls.curClosed.d++;
          stillUnclosed = false;
        } else if (top === "SHARED" || top === "CHECK") {
          if (ls.curOpened.s > 0) ls.curOpened.s--;
          else if (ls.deferedOpened.s > 0) ls.deferedOpened.s--;
          else ls.curClosed.s++;
          stillUnclosed = false;
        } else if (top === "TRY") {
          if (ls.curOpened.t > 0) ls.curOpened.t--;
          else if (ls.deferedOpened.t > 0) ls.deferedOpened.t--;
          else ls.curClosed.t++;
          stillUnclosed = false;
        } else if (top === "EXCEPT") {
          if (ls.curOpened.e > 0) ls.curOpened.e--;
          else if (ls.deferedOpened.e > 0) ls.deferedOpened.e--;
          else ls.curClosed.e++;
          stillUnclosed = false;
        } else if (top === "GRAPH") {
          if (ls.curOpened.g > 0) ls.curOpened.g--
          else if (ls.deferedOpened.g > 0) ls.deferedOpened.g--;
          else ls.curClosed.g++;
          stillUnclosed = false;
        }
        ls.tokens.pop();
        top = peek(ls.tokens);
      }
    } else if (state.lastToken === "*" && hasTop(ls.tokens, ["PROVIDE"])) {
      ls.deferedClosed.s++;
      ls.delimType = pyret_delimiter_type.CLOSING;
      ls.tokens.pop();
    }
    if (stream.match(/\s*$/, false)) { // End of line; close out nestings fields
      // console.log("We think we're at an end of line");
      // console.log("LineState is currently");
      // ls.print();
      ls.nestingsAtLineStart.addSelf(ls.curOpened).subSelf(ls.curClosed);
      while (hasTop(ls.tokens, "VAR")) {
        ls.tokens.pop();
        ls.curClosed.v++;
      }
      ls.nestingsAtLineEnd.addSelf(ls.curOpened).addSelf(ls.deferedOpened)
        .subSelf(ls.curClosed).subSelf(ls.deferedClosed);
      ls.tokens = ls.tokens.concat([]);
      ls.curOpened.zeroOut(); ls.deferedOpened.zeroOut();
      ls.curClosed.zeroOut(); ls.deferedClosed.zeroOut();
    }
    // console.log("LineState is now");
    // ls.print();
  }


  const INDENTATION = new Indent(1, 2, 2, 1, 1, 1, 1/*could be 0*/, 1, 1, 1, 1, 1, 1.5);

  function copyState(oldState) {
    return { tokenizer: oldState.tokenizer, lineState: oldState.lineState.copy(),
             lastToken: oldState.lastToken, lastContent: oldState.lastContent,
             commentNestingDepth: oldState.commentNestingDepth, inString: oldState.inString,
             dataNoPipeColon: oldState.dataNoPipeColon,
             sol: oldState.sol,
             maybeShorthandLambda: oldState.maybeShorthandLambda
           };
  }

  function indent(state, textAfter, fullLine) {
    var indentUnit = config.indentUnit;
    var taSS = new CodeMirror.StringStream(textAfter, config.tabSize);
    var sol = true;
    var inString = state.inString;
    // console.log("***** In indent, before processing textAfter (" + textAfter + ")");
    // state.lineState.print();
    state = copyState(state);
    if (state.commentNestingDepth > 0) {
      state.lineState.nestingsAtLineStart = state.lineState.nestingsAtLineEnd.copy();
    }
    if (/^\s*$/.test(textAfter)) {
      state.lineState.nestingsAtLineStart = state.lineState.nestingsAtLineEnd.copy();
    } else {
      // TODO: track nested comment state in here, to indent if needed
      while (!taSS.eol()) {
        var style = state.tokenizer(taSS, state);
        if (style !== "IGNORED-SPACE") {
          parse(sol, state, taSS, style);
          sol = false;
        }
      }
    }
    // console.log("***** In indent, after processing textAfter (" + textAfter + ")");
    // state.lineState.print();
    var indentSpec = state.lineState.nestingsAtLineStart;
    var indent = 0;
    for (var key in INDENTATION) {
      if (INDENTATION.hasOwnProperty(key))
        indent += (indentSpec[key] || 0) * INDENTATION[key];
    }
    if ((indentSpec.comments > 0) || (inString !== false)) {
      var spaces = fullLine.match(/\s*/)[0].length;
      if (spaces > 0)
        return spaces;
      else if (inString !== false)
        return inString;
      else
        return indent * indentUnit;
    } else if (/^\s*\|([^#]|$)/.test(fullLine)) {
      return (indent - 1) * indentUnit;
    } else {
      return indent * indentUnit;
    }
  }


  var external = {
    startState: function(basecolumn) {
      return {
        tokenizer: tokenBase,
        inString: false,
        commentNestingDepth: 0,
        lineState: new LineState([],
                                 new Indent(), new Indent(),
                                 new Indent(), new Indent(),
                                 new Indent(), new Indent(),
                                 pyret_delimiter_type.NONE ),
        sol: true
      };
    },
    blankLine: function blankLine(state) {
      // console.log("*** In BlankLine");
      state.lineState.nestingsAtLineStart = state.lineState.nestingsAtLineEnd.copy();
      // state.lineState.print();
    },

    copyState: copyState,

    token: function (stream, state) {
      // console.log("In token for stream = ");
      // console.log(stream);
      if (!state.sol && stream.sol()) {
        state.sol = true;
        state.indentation = stream.indentation();
      }
      var style = state.tokenizer(stream, state);
      if (style === "IGNORED-SPACE")
        return null;
      parse(state.sol, state, stream, style);
      state.sol = false;
      return style;
    },

    indent: indent,

    electricInput: new RegExp("(?:[de.\\]}|:]|\|#|[-enst\\*\\+/=<>^~]\\s|is%|is-not%)$"),

    fold: "pyret",

    delimiters: {opening: pyret_opening_tokens, closing: pyret_closing_tokens,
                 subkeywords: pyret_subkeywords,
                 lastSubkeywords: pyret_last_subkeywords,
                 special: pyret_special_delimiters,
                 types: pyret_delimiter_type},

    // FIXME: Should be deleted
    unprefixedContexts: ["SHARED", "OBJECT"]

  };
  return external;
});

// CodeMirror.defineMIME("text/x-pyret", "pyret");
