/*global define */
/*jslint unparam: true, node: true*/

var chalk = require("chalk");

define([], function() {
  function renderValue(runtime, val) {
    if(runtime.isPyretVal(val)) {
      return runtime.toReprJS(val, "_torepr");
    }
    return String(val);
  }

  function wordRegexp(words) {
    return new RegExp("^((" + words.join(")|(") + "))(?![a-zA-Z0-9-_])");
  }

  var pyret_indent_regex = new RegExp("^[a-zA-Z_][a-zA-Z0-9$_\\-]*");
  var pyret_keywords =
    wordRegexp(["fun", "method", "var", "when",
      "import", "provide", "data", "end",
      "except", "for", "from", "and",
      "or", "not", "as", "if",
      "else", "cases"]);
  var pyret_keywords_colon =
    wordRegexp(["doc", "try", "ask", "otherwise",
      "then", "with", "sharing", "where",
      "check", "block"]);
  var pyret_single_punctuation =
    new RegExp("^(["
      + ["\\:", "\\.", "<", ">",
	  ",", "^", ";", "|",
	  "=", "+", "*", "/",
	  "\\", "\\(", "\\)", "{",
	  "}", "\\[", "\\]"].join('')
      + "])");
  var pyret_double_punctuation =
    new RegExp("^(("
      + ["::", "==", ">=", "<=", "=>", "->", ":=", "<>"].join(")|(")
      + "))");
  var initial_operators = { "-": true, "+": true, "*": true, "/": true,
    "<": true, "<=": true, ">": true, ">=": true,
    "==": true, "<>": true, ".": true, "^": true,
    "is": true, "raises": true, "satisfies": true };
  var pyret_keywords_nested =
    new RegExp("(("
	+ ["otherwise", "then", "with", "sharing", "where"].join(")|(")
	+ "))");
  var pyret_unindent_single = /^(\||else|where)/g;
  var pyret_unindent_double = /^sharing/g;
  var pyret_indent_double = /^(data|ask|cases)/g;

  var styleMap = {
    'number': chalk.green,
    'string': chalk.green.dim,
    'comment': chalk.cyan,
    'builtin': chalk.gray,
    'keyword': chalk.magenta.dim,
    'variable': chalk.white,
    'function-name': chalk.white,
    'type': chalk.blue.dim
  };

  function ret(state, tokType, style) {
    state.lastToken = tokType;
    return style;
  }

  function mkTokenString(singleOrDouble) {
    return function(stream, state) {
      var insideRE = singleOrDouble === "'" ? /[^'\\]/ : /[^"\\]/;
      var endRE = singleOrDouble === "'" ? /'/ : /"/;

      while (!stream.eol()) {
        stream.eatWhile(insideRE);

        if (stream.eat('\\')) {
          stream.next();

          if (stream.eol()) {
            return ret(state, 'string', 'string');
	  }
        }
	else if (stream.eat(singleOrDouble)) {
	  return ret(state, 'string', 'string');
        }
	else {
          stream.eat(endRE);
	}
      }

      return ret(state, 'string', 'string');
    };
  }

  var tokenStringDouble = mkTokenString('"');
  var tokenStringSingle = mkTokenString("'");

  function getTokenStyle(stream, state) {
    if(stream.eatSpace()) {
      return 'IGNORED-SPACE';
    }

    var ch = stream.peek();

    //Comments
    if(ch === '#') {
      stream.skipToEnd();
      return ret(state, 'COMMENT', 'comment');
    }
    //Numbers
    if(stream.match(/^[0-9]+(\.[0-9]+)?/, true)) {
      return ret(state, 'number', 'number');
    }

    if(ch === '"') {
      stream.eat('"');
      return tokenStringDouble(stream, state);
    }
    if(ch === "'") {
      stream.eat("'");
      return tokenStringSingle(stream, state);
    }

    var match;

    //Question(ben) what are level 1 and level 2?
    if((match = stream.match(pyret_double_punctuation, true)) ||
	(match = stream.match(pyret_single_punctuation, true))) {
      if(state.dataNoPipeColon && (match[0] == ':' || match[0] == '|')) {
	state.dataNoPipeColon = false;
      }

      return ret(state, match[0], 'builtin');
    }
    if(match = stream.match(pyret_keywords, true)) {
      if(match[0] == 'data') {
	state.dataNoPipeColon = true;
      }

      return ret(state, match[0], 'keyword');
    }
    if(match = stream.match(pyret_keywords_colon, true)) {
      if(stream.peek() == ':') {
	return ret(state, match[0], 'keyword');
      }

      return ret(state, 'name', 'variable');
    }

    //Matches names
    if(match = stream.match(pyret_indent_regex, true)) {
      if (state.lastToken === "|" || state.lastToken === "::"
	  || state.lastToken === "data" || state.dataNoPipeColon) {
        state.dataNoPipeColon = false;
        return ret(state, 'name', 'type');
      }

      if(stream.match(/\s*\(/, false)) {
	return ret(state, 'name', 'function-name');
      }

      return ret(state, 'name', 'variable');
    }

    if (stream.eat("-")) {
      return ret(state, '-', '-', 'builtin');
    }

    stream.next();
    return null;
  }

  function tokenize(text, state, f) {
    var curStart = 0, curStyle = null;
    var stream = new StringStream(text, 2),
	style;

    while (!stream.eol()) {
      style = getTokenStyle(stream, state);

      if(curStyle !== style) {
        if (curStart < stream.start) {
	  f(stream.start, curStyle);
	}

        curStart = stream.start; curStyle = style;
      }

      stream.start = stream.pos;
    }

    if (curStart < stream.pos) {
      f(stream.pos, curStyle);
    }
  }

  function highlightLine(line) {
    var state = {
      dataNoPipeColon: false
    };
    var styles = [];
    var lastEnd = 0,
	i;
    var hl = "";

    tokenize(line, state, function(end, style) {
      styles.push(end, style);
    });

    for(i = 0; i < styles.length; i += 2) {
      if(styleMap[styles[i + 1]] !== undefined) {
	hl += styleMap[styles[i + 1]](line.substring(lastEnd, styles[i]));
      }
      else {
	hl += line.substring(lastEnd, styles[i]);
      }

      lastEnd = styles[i];
    }

    return hl;
  }

  function StringStream(string, tabSize) {
    this.pos = this.start = 0;
    this.string = string;
    this.tabSize = tabSize || 8;
    this.lastColumnPos = this.lastColumnValue = 0;
  }

  StringStream.prototype = {
    eol: function() {return this.pos >= this.string.length;},
    sol: function() {return this.pos == 0;},
    peek: function() {return this.string.charAt(this.pos) || undefined;},
    next: function() {
      if (this.pos < this.string.length)
        return this.string.charAt(this.pos++);
    },
    eat: function(match) {
      var ch = this.string.charAt(this.pos);
      if (typeof match == "string") var ok = ch == match;
      else var ok = ch && (match.test ? match.test(ch) : match(ch));
      if (ok) {++this.pos; return ch;}
    },
    eatWhile: function(match) {
      var start = this.pos;
      while (this.eat(match)){}
      return this.pos > start;
    },
    eatSpace: function() {
      var start = this.pos;
      while (/[\s\u00a0]/.test(this.string.charAt(this.pos))) ++this.pos;
      return this.pos > start;
    },
    skipToEnd: function() {this.pos = this.string.length;},
    skipTo: function(ch) {
      var found = this.string.indexOf(ch, this.pos);
      if (found > -1) {this.pos = found; return true;}
    },
    backUp: function(n) {this.pos -= n;},
    column: function() {
      if (this.lastColumnPos < this.start) {
        this.lastColumnValue = countColumn(this.string, this.start, this.tabSize, this.lastColumnPos, this.lastColumnValue);
        this.lastColumnPos = this.start;
      }
      return this.lastColumnValue;
    },
    indentation: function() {return countColumn(this.string, null, this.tabSize);},
    match: function(pattern, consume, caseInsensitive) {
      if (typeof pattern == "string") {
        var cased = function(str) {return caseInsensitive ? str.toLowerCase() : str;};
        var substr = this.string.substr(this.pos, pattern.length);
        if (cased(substr) == cased(pattern)) {
          if (consume !== false) this.pos += pattern.length;
          return true;
        }
      } else {
        var match = this.string.slice(this.pos).match(pattern);
        if (match && match.index > 0) return null;
        if (match && consume !== false) this.pos += match[0].length;
        return match;
      }
    },
    current: function(){return this.string.slice(this.start, this.pos);}
  };

  return {
    renderValue : renderValue,
    highlightLine : highlightLine,
    regex : {
      'PYRET_UNINDENT_SINGLE': pyret_unindent_single,
      'PYRET_UNINDENT_DOUBLE': pyret_unindent_double,
      'PYRET_INDENT_DOUBLE': pyret_indent_double,
      'PYRET_KEYWORDS_NESTED': pyret_keywords_nested
    }
  };
});
