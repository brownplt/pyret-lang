/*global define */
/*jslint unparam: true, node: true*/

var chalk = require("chalk");

define([], function() {
  var pyret_indent_regex = new RegExp("^[a-zA-Z_][a-zA-Z0-9$_\\-]*");
  var pyret_keywords =
    wordRegexp(["fun", "method", "var", "when",
      "import", "provide", "data", "end",
      "except", "for", "from", "and",
      "or", "not", "as", "if",
      "else", "cases", "check"]);
  var pyret_keywords_colon =
    wordRegexp(["doc", "try", "ask", "otherwise",
      "then", "with", "sharing", "where",
      "block"]);
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
  var pyret_unindent_single = /(^|\s+)(\||else|where)($|\b|\s+)/g;
  var pyret_unindent_double = /(^|\s+)sharing($|\b|\s+)/g;
  var pyret_indent_double = /(^|\s+)(data|ask|cases)($|\b|\s+)/g;

  var STYLE_MAP = {
    'default': {
      //Syntax highlighting
      'number': chalk.green,
      'string': chalk.green.dim,
      'comment': chalk.cyan,
      'builtin': chalk.gray,
      'keyword': chalk.magenta.dim,
      'variable': chalk.white,
      'function-name': chalk.white,
      'type': chalk.blue.dim,
      //Error message highlighting
      'error': chalk.red,
      'name': chalk.white,
      'loc': chalk.blue,
      'stack-trace': chalk.blue,
      //Check message highlighting
      'check-success': chalk.green,
      'check-failure': chalk.red,
      'check-neutral': chalk.blue
    }
  };

  var tokenStringDouble = makeTokenString('"');
  var tokenStringSingle = makeTokenString("'");

  function wordRegexp(words) {
    return new RegExp("^((" + words.join(")|(") + "))(?![a-zA-Z0-9-_])");
  }

  function countColumn(string, end, tabSize, startIndex, startValue) {
    var i, n;

    if (end == null) {
      end = string.search(/[^\s\u00a0]/);
      if (end == -1) {
	end = string.length;
      }
    }

    for (i = startIndex || 0, n = startValue || 0; i < end; ++i) {
      if (string.charAt(i) == "\t") {
	n += tabSize - (n % tabSize);
      }
      else {
	++n;
      }
    }
    return n;
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
      if (this.pos < this.string.length) {
	return this.string.charAt(this.pos++);
      }
    },
    eat: function(match) {
      var ch = this.string.charAt(this.pos);
      var ok;
      if (typeof match == "string") {
       	ok = ch == match;
      }
      else {
	ok = ch && (match.test ? match.test(ch) : match(ch));
      }
      if (ok) {
	++this.pos; return ch;
      }
    },
    eatWhile: function(match) {
      var start = this.pos;
      while (this.eat(match)){}
      return this.pos > start;
    },
    eatSpace: function() {
      var start = this.pos;
      while (/[\s\u00a0]/.test(this.string.charAt(this.pos))) {
	++this.pos;
      }
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
    indentation: function() {
      return countColumn(this.string, null, this.tabSize);
    },
    match: function(pattern, consume, caseInsensitive) {
      if (typeof pattern == "string") {
	var cased = function(str) {return caseInsensitive ? str.toLowerCase() : str;};
	var substr = this.string.substr(this.pos, pattern.length);
	if (cased(substr) == cased(pattern)) {
	  if (consume !== false) {
	    this.pos += pattern.length;
	  }
	  return true;
	}
      } else {
	var match = this.string.slice(this.pos).match(pattern);
	if (match && match.index > 0) {
	  return null;
	}
	if (match && consume !== false) {
	  this.pos += match[0].length;
	}
	return match;
      }
    },
    current: function(){return this.string.slice(this.start, this.pos);}
  };

  function ret(state, tokType, style) {
    state.lastToken = tokType;
    return style;
  }

  function makeTokenString(singleOrDouble) {
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

  return function(colorscheme) {
    var styleMap;

    if(colorscheme) {
      styleMap = STYLE_MAP[colorscheme];
    }
    else {
      styleMap = STYLE_MAP['default'];
    }

    //TODO: pass the color scheme to options
    function Renderer() {
      this.highlightLine = function(line) {
	var state = {
	  dataNoPipeColon: false
	};
	var styles = [];
	var lastEnd = 0;
	var hl = "",
	    styleFun,
	    i;

	tokenize(line, state, function(end, style) {
	  styles.push(end, style);
	});

	for(i = 0; i < styles.length; i += 2) {
	  styleFun = styleMap[styles[i + 1]];

	  if(styleFun !== undefined) {
	    hl += styleFun(line.substring(lastEnd, styles[i]));
	  }
	  else {
	    hl += line.substring(lastEnd, styles[i]);
	  }

	  lastEnd = styles[i];
	}

	return hl;
      };

      this.renderValue = function(runtime, val) {
	if(runtime.isPyretVal(val)) {
	  if(!runtime.isNothing(val)) {
	    return runtime.toReprJS(val, "_torepr");
	  }
	  else {
	    return "";
	  }
	}
	else {
	  return String(val);
	}
      };

      this.renderValueHighlight = function(runtime, val) {
	return this.highlightLine(this.renderValue(runtime, val));
      };

      //TODO: create these methods automatically by going throught he style map
      this.renderName = function(name) {
	return styleMap.name(name);
      };

      this.renderType = function(type) {
	return styleMap.type(type);
      };

      this.renderError = function(error) {
	return styleMap.error(error);
      };

      this.renderLoc = function(loc) {
	return styleMap.loc(loc);
      };

      this.renderStackTrace = function(stackTrace) {
	return styleMap['stack-trace'](stackTrace);
      };

      this.renderCheckSuccess = function(check) {
	return styleMap['check-success'](check);
      };

      this.renderCheckFailure = function(check) {
	return styleMap['check-failure'](check);
      };

      this.renderCheckNeutral = function(check) {
	return styleMap['check-neutral'](check);
      };

      this.drawAndPrintAnswer = function(runtime, answer) {
	var result = this.renderValue(runtime, answer);

	if(result !== "") {
	  console.log(this.highlightLine(result));
	}
      };

      this.drawSrcloc = function(runtime, s) {
	return this.renderLoc(s ? runtime.getField(s, "format").app(true) : "");
      };
    }

    function Indenter() {
      this.unindent = function(cmd) {
	if(cmd.match(pyret_unindent_single)) {
	  return Indenter.UNINDENT_SINGLE;
	}
	else if(cmd.match(pyret_unindent_double)) {
	  return Indenter.UNINDENT_DOUBLE;
	}
      };

      this.indent = function(cmd) {
	if(cmd.match(pyret_indent_double)) {
	  return Indenter.INDENT_DOUBLE;
	}
	else if(!cmd.match(pyret_unindent_single)) {
	  return Indenter.INDENT_SINGLE;
	}
      };

      this.matchNested = function(cmd) {
	return !!cmd.match(pyret_keywords_nested);
      };

      this.matchColon = function(cmd) {
	return !!cmd.match(/:$/g);
      };

      this.matchEnd = function(cmd) {
	return !!cmd.match(/end$/g);
      };

      this.getIndent = function(line, indentArray, indent) {
	var indentSize = indentArray.reduce(function(prev, cur, i, a) {
	  if(cur === Indenter.INDENT_DOUBLE) {
	    return prev + 2;
	  }
	  if(cur === Indenter.INDENT_SINGLE) {
	    return prev + 1;
	  }
	  if(cur === Indenter.UNINDENT_DOUBLE) {
	    return prev - 1;
	  }

	  return prev;
	}, 0);
	var trimmed = line.trim();

	if(trimmed === Indenter.END) {
	  var iStack = indentArray.filter(function(n) {
	    return n === Indenter.INDENT_SINGLE || n === Indenter.INDENT_DOUBLE;
	  });
	  var f = iStack[0];

	  if(f === Indenter.INDENT_SINGLE) {
	    return new Array(indentSize).join(indent);
	  }

	  if (f === Indenter.INDENT_DOUBLE && indentSize > 0) {
	    return new Array(indentSize - 1).join(indent);
	  }

	  return new Array(indentSize + 1).join(indent);
	}

	if(trimmed.match(pyret_unindent_single)) {
	  return new Array(indentSize).join(indent);
	}

	if(trimmed.match(pyret_unindent_double) && indentSize > 0) {
	  return new Array(indentSize - 1).join(indent);
	}

	return new Array(indentSize + 1).join(indent);
      };
    }

    Indenter.INDENT_SINGLE = "is";
    Indenter.INDENT_DOUBLE = "id";
    Indenter.UNINDENT_SINGLE = "us";
    Indenter.UNINDENT_DOUBLE = "ud";
    Indenter.END = "end";

    return {
      Renderer : Renderer,
      Indenter : Indenter
    };
  };
});
