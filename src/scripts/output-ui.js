/*global define */
/*jslint unparam: true, node: true*/

var chalk = require("chalk");

Array.prototype.exclude = function(arr) {
  return this.filter(function(a) {
    return arr.indexOf(a) === -1;
  });
};

String.prototype.repeat = function(num) {
  return new Array(num + 1).join(this);
};

String.prototype.eat = function(reg) {
  return this.replace(reg, "");
};

String.prototype.eatNext = function(reg) {
  return this.slice(1, this.length);
};

String.prototype.eatUntil = function(reg) {
  var str = this;

  while(str && !str.match(reg)) {
    str = str.eatNext();
  }

  return str.eat(reg);
};

define([], function() {
  var pyret_indent_regex = new RegExp("^[a-zA-Z_][a-zA-Z0-9$_\\-]*");
  var pyret_keywords =
    wordRegexp(["fun", "method", "var", "when",
      "import", "provide", "data", "end",
      "except", "for", "from", "and",
      "or", "not", "as", "if",
      "else", "cases", "check", "lam"]);
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
  var pyret_initial_operators =
    new RegExp("^("
      + [ "\\-", "\\+", "\\*", "/", "<",
	  "<=", ">", ">=", "==", "<>",
	  "\\.", "\\^", "is", "raises", "satisfies"].join("|")
      + ")");
  var pyret_no_indent =
    new RegExp("(("
	+ ["otherwise", "then", "with", "sharing", "where"].join(")|(")
	+ "))");
  var pyret_unindent_single_soft = /^\s*\|($|\b|\s+)/;
  var pyret_unindent_single_hard = /^\s*(else|where)($|\b|\s+)/;
  var pyret_unindent_double = /^\s*sharing($|\b|\s+)/;
  var pyret_indent_double = /^\s*(data|ask|cases)($|\b|\s+)/;
  var pyret_colon = /^\s*:($|\b|\s+)/;
  var pyret_end = /^\s*end($|\b|\s+)/;
  var pyret_open_braces = /^\s*(\(|\[|\{)/;
  var pyret_close_braces = /^\s*(\)|\]|\})/;

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
      this.unindent = function(indentArray) {
	var lastIndent = indentArray.shift();

	while(lastIndent && lastIndent !== Indenter.INDENT_SINGLE
	    && lastIndent !== Indenter.INDENT_DOUBLE) {
	  lastIndent = indentArray.shift();
	}

	return indentArray;
      };

      this.indent = function(cmd, indentArray) {
	var lastCmd = "";
	var addColon = true;

	while(cmd.length > 0 && cmd !== lastCmd) {
	  lastCmd = cmd;

	  if(cmd.match(pyret_no_indent)) {
	    cmd = cmd.eat(pyret_no_indent).eatUntil(pyret_colon);
	  }
	  else if(cmd.match(pyret_unindent_single_soft)) {
	    cmd = cmd.eat(pyret_unindent_single_soft);
	  }
	  else if(cmd.match(pyret_unindent_single_hard)) {
	    cmd = cmd.eat(pyret_unindent_single_hard);
	    indentArray = this.unindent(indentArray);
	  }
	  else if(cmd.match(pyret_unindent_double)) {
	    cmd = cmd.eat(pyret_unindent_double);
	    indentArray.unshift(Indenter.UNINDENT);
	  }
	  else if(cmd.match(pyret_indent_double)) {
	    cmd = cmd.eat(pyret_indent_double).eatUntil(pyret_colon);
	    indentArray.unshift(Indenter.INDENT_DOUBLE);
	  }
	  else if(cmd.match(pyret_double_punctuation)) {
	    cmd = cmd.eat(pyret_double_punctuation);
	  }
	  else if(cmd.match(pyret_colon)) {
	    cmd = cmd.eat(pyret_colon);

	    if(!addColon) {
	      addColon = true;
	    }
	    else {
	      indentArray.unshift(Indenter.INDENT_SINGLE);
	    }
	  }
	  else if(cmd.match(pyret_open_braces)) {
	    cmd = cmd.eat(pyret_open_braces);
	    indentArray.unshift(Indenter.INDENT_SINGLE);
	    addColon = false;
	  }
	  else if(cmd.match(pyret_close_braces)) {
	    cmd = cmd.eat(pyret_close_braces);
	    indentArray = this.unindent(indentArray);
	    addColon = true;
	  }
	  else if(cmd.match(pyret_end)) {
	    cmd = cmd.eat(pyret_end);
	    indentArray = this.unindent(indentArray);
	  }
	  else if(cmd.match(pyret_keywords)) {
	    cmd = cmd.eat(pyret_keywords);
	    addColon = true;
	  }
	  else if(cmd.match(pyret_keywords_colon)) {
	    cmd = cmd.eat(pyret_keywords_colon);

	    if(cmd.match(pyret_colon)) {
	      cmd = cmd.eat(pyret_colon);
	    }
	  }
	  else if(cmd.match(pyret_indent_regex)) {
	    cmd = cmd.eat(pyret_indent_regex).eat(/^\s*/);
	  }
	  else {
	    cmd = cmd.eatNext();
	  }
	}
	return indentArray;
      };

      this.getIndent = function(line, indentArray, indent) {
	var trimmed = line.trim();
	var indentSize = indentArray.reduce(function(prev, cur, i, a) {
	  if(cur === Indenter.INDENT_DOUBLE) {
	    return prev + 2;
	  }
	  else if(cur === Indenter.INDENT_SINGLE) {
	    return prev + 1;
	  }
	  else if(cur === Indenter.UNINDENT) {
	    return prev - 1;
	  }

	  return prev;
	}, 0);

	if(trimmed.match(pyret_end)) {
	  var iStack = indentArray.filter(function(n) {
	    return n === Indenter.INDENT_SINGLE || n === Indenter.INDENT_DOUBLE;
	  });
	  var f = iStack[0];

	  if(f === Indenter.INDENT_SINGLE) {
	    return indent.repeat(indentSize - 1);
	  }

	  if (f === Indenter.INDENT_DOUBLE && indentSize > 0) {
	    return indent.repeat(indentSize - 2);
	  }
	}
	else if(trimmed.match(pyret_initial_operators)) {
	  return indent.repeat(indentSize + 1);
	}
	else if(trimmed.match(pyret_unindent_single_soft) ||
	    trimmed.match(pyret_unindent_single_hard)) {
	  return indent.repeat(indentSize - 1);
	}
	else if(trimmed.match(pyret_unindent_double) && indentSize > 0) {
	  return indent.repeat(indentSize - 2);
	}

	return indent.repeat(indentSize);
      };
    }

    Indenter.INDENT_SINGLE = "is";
    Indenter.INDENT_SINGLE = "is";
    Indenter.INDENT_DOUBLE = "id";
    Indenter.UNINDENT = "u";

    return {
      Renderer : Renderer,
      Indenter : Indenter
    };
  };
});
