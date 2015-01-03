/*global define */
/*jslint unparam: true, node: true*/

var events = require("events");
var keypress = require("keypress");
var chalk = require("chalk");

define([], function() {
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

  function onKeypress(ch, key) {
    //TODO: make sure that this is compatible on all OS's
    if(key && key.name === "return") {
      var cmdTrimmed = this.curLine.trim();
      var cmdLen = cmdTrimmed.length;
      var newCmd = cmdTrimmed;

      this.syncHistory(true);
      this.resetLine();

      if(cmdTrimmed.match(pyret_unindent_single)) {
	this.nestStack.unshift("us");
      }
      else if(cmdTrimmed.match(pyret_unindent_double)) {
	this.nestStack.unshift("ud");
      }

      if(cmdTrimmed == "end") {
	this.commandQueue.push(cmdTrimmed);

	var lastNest = this.nestStack.shift();

	while(lastNest !== "is" && lastNest !== "id") {
	  lastNest = this.nestStack.shift();
	}

	if(this.nestStack.length === 0) {
	  newCmd = this.commandQueue.join("\n");
	  this.commandQueue = [];
	}
	else {
	  this.prompt();
	  return;
	}
      }
      else if(cmdTrimmed.match(pyret_keywords_nested)) {
	this.commandQueue.push(cmdTrimmed);
	this.prompt();
	return;
      }
      else if(cmdTrimmed.match(/:$/g)) {
	this.commandQueue.push(cmdTrimmed);

	if(cmdTrimmed.match(pyret_indent_double)) {
	  this.nestStack.unshift("id");
	}
	else {
	  this.nestStack.unshift("is");
	}

	this.prompt();
	return;
      }

      if(this.nestStack.length > 0) {
	this.commandQueue.push(cmdTrimmed);
	this.prompt();
	return;
      }

      this.lineNumber = 1;
      this.nestStack = [];

      this.emit('command', newCmd);
    }
    else if(key && key.name === "up") {
      if(this.history.length > 1) {
	this.future.unshift(this.history.shift());
	this.curLine = this.history[0].cur;
	this.historyMarker -= 1;
	this.syncLine(true);
      }
    }
    else if(key && key.name === "down") {
      if(this.future.length > 0) {
	this.history.unshift(this.future.shift());
	this.curLine = this.history[0].cur;
	this.historyMarker += 1;

	this.syncLine(true);
      }
    }
    else if(key && key.name === "right"){
      this.keyRight();
    }
    else if(key && key.name === "left") {
      this.keyLeft();
    }
    else if(key && key.name === "backspace") {
      if(this.cursorPosition > 0) {
	this.curLine = this.curLine.substring(0, this.cursorPosition - 1)
	  + this.curLine.substring(this.cursorPosition, this.curLine.length);

	this.syncHistory(false);
      }

      this.syncLine();
      this.keyLeft();
    }
    else if(key && key.ctrl && key.name === "c") {
      if(this.nestStack.length > 0) {
	this.nestStack = [];
	this.commandQueue = [];

	this.syncHistory();
	this.resetLine();
	this.prompt();
      }
      else {
	process.exit();
      }
    }
    //TODO: how to decide what keys pass through?
    else {
      if(this.cursorPosition < this.curLine.length) {
	this.curLine = this.curLine.substring(0, this.cursorPosition)
	  + ch
	  + this.curLine.substring(this.cursorPosition, this.curLine.length);
      }
      else {
	this.curLine += ch;
      }

      this.syncHistory(false);
      this.syncLine();
      this.keyRight();
    }
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

  function InputUI(rt, input, output) {
    this.runtime = rt;
    this.input = input;
    this.output = output;
    keypress(this.input);

    this.history = [{"old": "", "cur": ""}];
    this.future = [];
    this.curLine = "";
    this.promptSymbol = ">>";
    this.promptString = "";
    this.indent = "  ";
    this.interactionsNumber = 0;
    this.lineNumber = 1;
    this.cursorPosition = 0;
    this.historyMarker = 0;

    this.commandQueue = [];
    this.nestStack = [];

    this.input.setRawMode(true);
    this.input.setEncoding("utf8");
    this.input.resume();
    this.input.on("keypress", onKeypress.bind(this));
  }

  InputUI.prototype.setPrompt = function(ps) {
    this.promptString = ps;
  };

  InputUI.prototype.prompt = function() {
    if(this.nestStack.length === 0) {
      this.interactionsNumber += 1;
    }

    this.promptString = this.interactionsNumber + "::" + this.lineNumber++ + " " + this.promptSymbol + " ";

    this.output.write("\n" + this.promptString + this.getIndent());
  };

  InputUI.prototype.getIndent = function() {
    var indentSize = this.nestStack.reduce(function(prev, cur, i, a) {
      if(cur === "id") {
	return prev + 2;
      }
      else if(cur === "is") {
	return prev + 1;
      }
      else if(cur === "ud") {
	return prev - 1;
      }
      else {
	return prev;
      }
    }, 0);
    var trimmed = this.curLine.trim();

    if(trimmed == "end") {
      iStack = this.nestStack.filter(function(n) {
	return n === "is" || n === "id";
      });
      var f = iStack[0];

      if(f === "is") {
	return new Array(indentSize).join(this.indent);
      }
      else if (f === "id" && indentSize > 0) {
	return new Array(indentSize - 1).join(this.indent);
      }
      else {
	return new Array(indentSize + 1).join(this.indent);
      }
    }
    else if(trimmed.match(pyret_unindent_single)) {
      return new Array(indentSize).join(this.indent);
    }
    else if(trimmed.match(pyret_unindent_double) && indentSize > 0) {
      return new Array(indentSize - 1).join(this.indent);
    }

    return new Array(indentSize + 1).join(this.indent);
  };

  InputUI.prototype.getInteractionsNumber = function() {
    return this.interactionsNumber
  };

  InputUI.prototype.resetLine = function() {
    this.curLine = "";
    this.cursorPosition = 0;
  }

  InputUI.prototype.syncCursor = function() {
    this.output.cursorTo(this.cursorPosition
	+ this.promptString.length
	+ this.getIndent().length);
  };

  InputUI.prototype.syncLine = function(gotoEol) {
    var hl = highlightLine(this.curLine);

    this.output.clearLine();
    this.output.cursorTo(0);
    this.output.write(this.promptString + this.getIndent());
    this.output.write(hl);

    if(gotoEol) {
      this.cursorPosition = this.curLine.length;
    }
  };

  InputUI.prototype.syncHistory = function(isNewline) {
    var spaceRegex = /^\s*$/g;

    if(!(this.curLine.match(spaceRegex))) {
      this.history[0] = {"old": this.history[0].old, "cur": this.curLine};
    }

    if(isNewline) {
      if(this.historyMarker >= 0) {
	this.history = this.history.slice(0, this.historyMarker).map(function(l) {
	  return {"old": l.old, "cur": l.old};
	}).concat(this.history.slice(this.historyMarker, this.history.length));
      }

      if(this.future.length > 0) {
	if(this.historyMarker < 0) {
	  this.future = this.future.slice(0, -this.historyMarker).map(function(l) {
	    return {"old": l.old, "cur": l.old};
	  }).concat(this.future.slice(-this.historyMarker, this.future.length));
	}

	this.future.filter(function(l) {
	  return !(l.old.match(spaceRegex));
	});

	while(this.future.length > 0) {
	  this.history.unshift(this.future.shift());
	}
      }

      this.historyMarker = 0;

      if(!(this.curLine.match(spaceRegex) || (this.history.length > 1
	      && this.history[1].old === this.curLine))) {
	var oldLine = this.history[0].old;

	if(!(oldLine.match(spaceRegex) || oldLine === this.curLine)) {
	  this.history.unshift({"old": "", "cur": ""});
	  this.history[0] = {"old": this.curLine, "cur": this.curLine};
	  this.history.unshift({"old": "", "cur": ""});
	}
	else if(oldLine !== this.curLine) {
	  this.history[0] = {"old": this.curLine, "cur": this.curLine};
	  this.history.unshift({"old": "", "cur": ""});
	}
      }
    }
    else if(this.historyMarker <= 0) {
      this.historyMarker = 1;
    }
  }

  InputUI.prototype.keyRight = function() {
    if(this.cursorPosition < this.curLine.length) {
      this.cursorPosition += 1;
    }

    this.syncCursor();
  };

  InputUI.prototype.keyLeft = function() {
    if(this.cursorPosition > 0) {
      this.cursorPosition -= 1;
    }

    this.syncCursor();
  };

  InputUI.prototype.__proto__ = events.EventEmitter.prototype;

  //TODO: add options to this function
  return function(runtime) {
    return new InputUI(runtime, process.stdin, process.stdout);
  }
});
