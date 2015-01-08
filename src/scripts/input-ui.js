/*global define */
/*jslint unparam: true, node: true*/

var events = require("events");
var chalk = require("chalk");
var keypress = require("keypress");

define(["./output-ui"], function(outputUI) {
  var functionKeyCodeReAnywhere = new RegExp('(?:\x1b+)(O|N|\\[|\\[\\[)(?:' + [
    '(\\d+)(?:;(\\d+))?([~^$])',
    '(?:M([@ #!a`])(.)(.))', // mouse
    '(?:1;)?(\\d+)?([a-zA-Z])'
  ].join('|') + ')');
  var metaKeyCodeReAnywhere = /(?:\x1b)([a-zA-Z0-9])/;

  function codePointAt(str, index) {
    var code = str.charCodeAt(index);
    var low;
    if (0xd800 <= code && code <= 0xdbff) { // High surrogate
      low = str.charCodeAt(index + 1);
      if (!isNaN(low)) {
	code = 0x10000 + (code - 0xd800) * 0x400 + (low - 0xdc00);
      }
    }
    return code;
  }

  function stripVTControlCharacters(str) {
    str = str.replace(new RegExp(functionKeyCodeReAnywhere.source, 'g'), '');
    return str.replace(new RegExp(metaKeyCodeReAnywhere.source, 'g'), '');
  }

  function isFullWidthCodePoint(code) {
    if (isNaN(code)) {
      return false;
    }

    // Code points are derived from:
    // http://www.unicode.org/Public/UNIDATA/EastAsianWidth.txt
    if (code >= 0x1100 && (
	code <= 0x115f ||  // Hangul Jamo
	0x2329 === code || // LEFT-POINTING ANGLE BRACKET
	0x232a === code || // RIGHT-POINTING ANGLE BRACKET
	// CJK Radicals Supplement .. Enclosed CJK Letters and Months
	(0x2e80 <= code && code <= 0x3247 && code !== 0x303f) ||
	// Enclosed CJK Letters and Months .. CJK Unified Ideographs Extension A
	0x3250 <= code && code <= 0x4dbf ||
	// CJK Unified Ideographs .. Yi Radicals
	0x4e00 <= code && code <= 0xa4c6 ||
	// Hangul Jamo Extended-A
	0xa960 <= code && code <= 0xa97c ||
	// Hangul Syllables
	0xac00 <= code && code <= 0xd7a3 ||
	// CJK Compatibility Ideographs
	0xf900 <= code && code <= 0xfaff ||
	// Vertical Forms
	0xfe10 <= code && code <= 0xfe19 ||
	// CJK Compatibility Forms .. Small Form Variants
	0xfe30 <= code && code <= 0xfe6b ||
	// Halfwidth and Fullwidth Forms
	0xff01 <= code && code <= 0xff60 ||
	0xffe0 <= code && code <= 0xffe6 ||
	// Kana Supplement
	0x1b000 <= code && code <= 0x1b001 ||
	// Enclosed Ideographic Supplement
	0x1f200 <= code && code <= 0x1f251 ||
	// CJK Unified Ideographs Extension B .. Tertiary Ideographic Plane
	0x20000 <= code && code <= 0x3fffd)) {
      return true;
    }
    return false;
  }

  function numLines(str) {
    var matches = str.match(/.*\n|.+$/g);
    return matches ? matches.length : 1;
  }

  function onKeypress(ch, key) {
    //TODO: add a shift return and a tab keypress
    if(key && !(key.shift) && key.name === "return") {
      this.enter();
    }
    else if(key && key.shift && key.name === "up") {
      this.keyShiftUp();
    }
    else if(key && key.name === "up") {
      this.keyUp();
    }
    else if(key && key.shift && key.name === "down") {
      this.keyShiftDown();
    }
    else if(key && key.name === "down") {
      this.keyDown();
    }
    else if(key && key.name === "right"){
      this.keyRight();
    }
    else if(key && key.name === "left") {
      this.keyLeft();
    }
    else if(key && key.name === "backspace") {
      this.backspace();
    }
    else if(key && key.ctrl && key.name === "c") {
      this.keyboardInterrupt();
    }
    //TODO: how to decide what keys pass through?
    else {
      this.addChar(ch);
    }
  }

  function InputUI(rt, input, output) {
    this.runtime = rt;
    this.input = input;
    this.output = output;
    keypress(this.input);

    this.history = [{"old": "", "cur": "", "block": "", "blockSize": 1}];
    this.historyIndex = 0;
    this.historyUpdate = 0;
    this.blockSize = 0;
    this.curLine = "";
    this.promptSymbol = ">>";
    this.promptString = "";
    this.indent = "  ";
    this.interactionsNumber = 0;
    this.lineNumber = 1;
    this.cursorPosition = 0;

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

    this.resetLine();
    this.addIndent();

    this.promptString = this.interactionsNumber
      + "::"
      + this.lineNumber++
      + " "
      + this.promptSymbol + " ";
    this.output.write("\n" + this.promptString + this.curLine);
  };

  InputUI.prototype.addChar = function(ch) {
    if(this.cursorPosition < this.curLine.length) {
      this.curLine = this.curLine.substring(0, this.cursorPosition)
	+ ch
	+ this.curLine.substring(this.cursorPosition, this.curLine.length);
    }
    else {
      this.curLine += ch;
    }

    this.addIndent();
    this.syncHistory(false);
    this.keyRight();
  };

  InputUI.prototype.getIndent = function() {
    var indentSize = this.nestStack.reduce(function(prev, cur, i, a) {
      if(cur === "id") {
	return prev + 2;
      }
      if(cur === "is") {
	return prev + 1;
      }
      if(cur === "ud") {
	return prev - 1;
      }

      return prev;
    }, 0);
    var trimmed = this.curLine.trim();

    if(trimmed === "end") {
      var iStack = this.nestStack.filter(function(n) {
	return n === "is" || n === "id";
      });
      var f = iStack[0];

      if(f === "is") {
	return new Array(indentSize).join(this.indent);
      }
      if (f === "id" && indentSize > 0) {
	return new Array(indentSize - 1).join(this.indent);
      }

      return new Array(indentSize + 1).join(this.indent);
    }
    if(trimmed.match(outputUI.regex.PYRET_UNINDENT_SINGLE)) {
      return new Array(indentSize).join(this.indent);
    }
    if(trimmed.match(outputUI.regex.PYRET_UNINDENT_DOUBLE) && indentSize > 0) {
      return new Array(indentSize - 1).join(this.indent);
    }

    return new Array(indentSize + 1).join(this.indent);
  };

  InputUI.prototype.addIndent = function() {
    var lineNoIndent = this.curLine.replace(/^(\s*)/, "");
    var lineIndent = this.getIndent() + lineNoIndent;

    this.cursorPosition += lineIndent.length - this.curLine.length;
    this.curLine = lineIndent;
  };

  InputUI.prototype.getInteractionsNumber = function() {
    return this.interactionsNumber;
  };

  InputUI.prototype.resetLine = function() {
    this.curLine = "";
    this.cursorPosition = 0;
    this.rowOffset = 0;
  };

  InputUI.prototype.getCursorPos = function() {
    return this.getDisplayPos(
	this.promptString
	+ this.curLine.slice(0, this.cursorPosition));
  };

  InputUI.prototype.getDisplayPos = function(str) {
    var offset = 0;
    var col = this.output.columns;
    var row = 0;
    var code, i;
    str = stripVTControlCharacters(str);

    for (i = 0; i < str.length; i++) {
      code = codePointAt(str, i);

      if (code >= 0x10000) {
	i++;
      }

      if (code === 0x0a) {
	//Note(ben) accounts for lines within multiline strings that are longer
	//than the width of the terminal
	row += 1 + (offset % col === 0 && offset > 0 ? (offset / col) - 1
	 : (offset - (offset % col)) / col);
	offset = 0;
	continue;
      }

      if (isFullWidthCodePoint(code)) {
	//Note(ben) full width code points will start on the next line if 1 away
	//from the end of the current line
	if ((offset + 1) % col === 0) {
	  offset++;
	}

	offset += 2;
      }
      else {
	offset++;
      }
    }

    var cols = offset % col;
    var rows = row + (offset - cols) / col;
    return {cols: cols, rows: rows};
  };

  //TODO: add indents
  InputUI.prototype.syncLine = function(gotoEol) {
    // line length
    var line = this.promptString + this.curLine;
    var dispPos = this.getDisplayPos(line);
    var lineCols = dispPos.cols;
    var lineRows = dispPos.rows;

    // cursor position
    if(gotoEol) {
      this.cursorPosition = this.curLine.length;
    }

    var cursorPos = this.getCursorPos();

    // first move to the bottom of the current line, based on cursor pos
    var rowOffset = this.rowOffset || 0;

    if (rowOffset > 0) {
      this.output.moveCursor(0, -rowOffset);
    }

    // Cursor to left edge.
    this.output.cursorTo(0);
    // erase data
    this.output.clearScreenDown();

    // Write the prompt and the current buffer content.
    this.output.write(this.promptString);
    this.output.write(outputUI.highlightLine(this.curLine));

    // Force terminal to allocate a new line
    if (lineCols === 0) {
      this.output.write(' ');
    }

    // Move cursor to original position.
    this.output.cursorTo(cursorPos.cols);

    var diff = lineRows - cursorPos.rows;

    if (diff > 0) {
      this.output.moveCursor(0, -diff);
    }

    this.rowOffset = cursorPos.rows;
  };

  InputUI.prototype.syncHistory = function(isNewline) {
    var spaceRegex = /^\s*$/g;

    if(!(this.curLine.match(spaceRegex)) || this.curLine === "") {
      this.history[this.historyIndex] = {
	"old": this.history[this.historyIndex].old,
	"cur": this.curLine,
	"block": this.history[this.historyIndex].block,
	"blockSize": this.history[this.historyIndex].blockSize};
    }

    if(isNewline) {
      if(this.historyUpdate >= 0) {
	this.history = this.history.slice(0, this.historyUpdate).map(function(l) {
	  return {"old": l.old, "cur": l.old, "block": l.block, "blockSize": l.blockSize};
	}).concat(this.history.slice(this.historyUpdate, this.history.length));
      }

      var oldLine = this.history[0].old;
      var oldBlock = this.history[0].block;

      if(!(this.curLine.match(spaceRegex) || (this.history.length > 1
	      && this.history[1].old === oldLine
	      && this.history[1].block === oldBlock))) {

	//TODO: will this cause problems?
	if(!(oldLine.match(spaceRegex) || oldLine === this.curLine)) {
	  this.history.unshift({"old": "", "cur": "", "block": "", "blockSize": 1});
	  this.history[0] = {
	    "old": this.curLine,
	    "cur": this.curLine,
	    "block": this.curLine,
	    "blockSize": 1};
	  this.history.unshift({"old": "", "cur": "", "block": "", "blockSize": 1});
	}
	else if(oldLine !== this.curLine) {
	  this.history[0] = {
	    "old": this.curLine,
	    "cur": this.curLine,
	    "block": this.curLine,
	    "blockSize": 1};
	  this.history.unshift({"old": "", "cur": "", "block": "", "blockSize": 1});
	}
      }

      this.historyIndex = 0;
      this.historyUpdate = 0;
    }
    else {
      this.historyUpdate = Math.max(this.historyIndex + 1, this.historyUpdate);
    }
  };

  InputUI.prototype.historyPrev = function() {
    if(this.historyIndex < this.history.length - 1) {
      this.historyIndex += 1;
      this.curLine = this.history[this.historyIndex].cur;
      this.syncLine(true);
    }
  };

  InputUI.prototype.historyNext = function() {
    if(this.historyIndex > 0) {
      this.historyIndex -= 1;
      this.curLine = this.history[this.historyIndex].cur;
      this.syncLine(true);
    }
  };

  InputUI.prototype.enter = function() {
    var matches = this.curLine.match(/.*\n|.+$/g);
    var cmdTrimmed;

    this.syncHistory(true);
    this.syncLine(true);
    this.blockSize += 1;

    if(matches && matches.length > 1) {
      matches.slice(0, matches.length - 1).forEach(function(m) {
	cmdTrimmed = m.replace(/(\s*)$/, "");
	this.newline(cmdTrimmed, false);
      }, this);

      cmdTrimmed = matches.pop().replace(/(\s*)$/, "");
      this.newline(cmdTrimmed, true);
    }
    else {
      this.newline(this.curLine, true);
    }
  };

  InputUI.prototype.newline = function(cmd, printPrompt) {
    var newCmd = cmd;

    if(cmd.match(outputUI.regex.PYRET_UNINDENT_SINGLE)) {
      this.nestStack.unshift("us");
    }
    else if(cmd.match(outputUI.regex.PYRET_UNINDENT_DOUBLE)) {
      this.nestStack.unshift("ud");
    }

    if(cmd.match(/end$/g)) {
      this.commandQueue.push(cmd);

      var lastNest = this.nestStack.shift();

      while(lastNest && lastNest !== "is" && lastNest !== "id") {
	lastNest = this.nestStack.shift();
      }

      if(this.nestStack.length === 0) {
	newCmd = this.commandQueue.join("\n");
	this.history[1].block = newCmd;
	this.history[1].blockSize = this.blockSize;
	this.history[this.blockSize].block = newCmd;
	this.history[this.blockSize].blockSize = this.blockSize;
      }
      else {
	if(printPrompt) {
	  this.prompt();
	}
	return;
      }
    }
    else if(cmd.match(outputUI.regex.PYRET_KEYWORDS_NESTED)) {
      this.commandQueue.push(cmd);
      if(printPrompt) {
	this.prompt();
      }
      return;
    }
    else if(cmd.match(/:$/g)) {
      this.commandQueue.push(cmd);

      if(cmd.match(outputUI.regex.PYRET_INDENT_DOUBLE)) {
	this.nestStack.unshift("id");
      }
      else if(!cmd.match(outputUI.regex.PYRET_UNINDENT_SINGLE)) {
	this.nestStack.unshift("is");
      }

      if(printPrompt) {
	this.prompt();
      }
      return;
    }

    if(this.nestStack.length > 0) {
      this.commandQueue.push(cmd);
      if(printPrompt) {
	this.prompt();
      }
      return;
    }

    this.nestStack = [];
    this.commandQueue = [];
    this.lineNumber = 1;
    this.blockSize = 0;

    this.output.write("\n");
    this.emit('command', newCmd);
  };

  InputUI.prototype.keyShiftUp = function() {
    this.historyIndex += this.history[this.historyIndex].blockSize;

    if(this.historyIndex >= this.history.length - 1) {
      this.historyIndex = this.history.length - 1;
    }

    this.curLine = this.history[this.historyIndex].block;
    this.syncLine(true);
  };

  InputUI.prototype.keyUp = function() {
    var matches = this.curLine.slice(0, this.cursorPosition).match(/.*\n/g);

    if(matches) {
      var lastMatch = matches.pop();
      this.cursorPosition -= lastMatch.length;
      this.syncLine();
    }
    else {
      this.historyPrev();
    }
  };

  InputUI.prototype.keyShiftDown = function() {
    this.historyIndex -= this.history[this.historyIndex].blockSize;

    if(this.historyIndex <= 0) {
      this.historyIndex = 0;
    }

    this.curLine = this.history[this.historyIndex].block;
    this.syncLine(true);
  };

  InputUI.prototype.keyDown = function() {
    var matches = this.curLine.slice(this.cursorPosition, this.curLine.length).match(/.*\n/g);

    if(matches) {
      var firstMatch = matches.shift();
      this.cursorPosition += firstMatch.length;
      this.syncLine();
    }
    else {
      this.historyNext();
    }
  };

  InputUI.prototype.keyRight = function() {
    if(this.cursorPosition < this.curLine.length) {
      this.cursorPosition += 1;
    }

    this.syncLine();
  };

  InputUI.prototype.keyLeft = function() {
    if(this.cursorPosition > 0) {
      this.cursorPosition -= 1;
    }

    this.syncLine();
  };

  InputUI.prototype.backspace = function() {
    if(this.cursorPosition > 0) {
      this.curLine = this.curLine.substring(0, this.cursorPosition - 1)
	+ this.curLine.substring(this.cursorPosition, this.curLine.length);

      this.syncHistory(false);
    }

    this.keyLeft();
  };

  InputUI.prototype.keyboardInterrupt = function() {
    this.syncLine(true);

    if(this.nestStack.length > 0 || this.curLine !== "") {
      //TODO: abstract this
      this.nestStack = [];
      this.commandQueue = [];
      this.lineNumber = 1;
      this.blockSize = 0;

      this.syncHistory();
      this.resetLine();
      this.prompt();
    }
    else {
      process.exit();
    }
  };

  InputUI.prototype.__proto__ = events.EventEmitter.prototype;

  //TODO: add options to this function
  return function(runtime) {
    return new InputUI(runtime, process.stdin, process.stdout);
  }
});
