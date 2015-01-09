/*global define */
/*jslint unparam: true, node: true*/

var events = require("events");
var chalk = require("chalk");
var keypress = require("keypress");

define(["./output-ui"], function(outputLib) {
  var outputUI = outputLib('default');
  var renderer = new outputUI.Renderer();
  var Indenter = outputUI.Indenter;
  var indenter = new Indenter();

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

  function numLines(line) {
    var matches = line.match(/.*\n|.+$/g);

    if(matches) {
      var lastMatch = matches[matches.length - 1];

      if(lastMatch.charAt(lastMatch.length - 1) === "\n") {
	return matches.length + 1;
      }
      else {
	return matches.length;
      }
    }
    else {
      return 1;
    }
  }

  function onKeypress(ch, key) {
    //TODO: add a shift return and a tab keypress
    if(key && key.name === "return") {
      if(this.lastKey === "return") {
	this.lastKey = "";
	clearTimeout(this.enterVar);

	this.addChar("\n");
      }
      else {
	this.lastKey = "return";

	this.enterVar = setTimeout(function() {
	  this.lastKey = "";
	  this.enter();
	}.bind(this), 200);
      }
    }
    else if(key && key.name === "up") {
      this.keyUp();
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

    this.history = [{"old": "", "cur": ""}];
    this.historyIndex = 0;
    this.historyUpdate = 0;
    this.line = "";
    this.promptSymbol = ">>";
    this.promptString = "";
    this.indent = "  ";
    this.interactionsNumber = 0;
    //this.lineNumber = 0;
    this.cursorPosition = 0;

    this.lastKey = "";

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
      + "::1 "
      + this.promptSymbol + " ";
    this.output.write("\n" + this.promptString + this.line);
  };

  InputUI.prototype.resetLine = function() {
    this.line = "";
    this.cursorPosition = 0;
    this.rowOffset = 0;
  };

  InputUI.prototype.resetNest = function(cmd) {
    this.nestStack = [];
    this.commandQueue = [];
    //this.lineNumber = 0;

    this.output.write("\n");
    this.emit('command', cmd);
  };

  InputUI.prototype.addChar = function(ch) {
    if(this.cursorPosition < this.line.length) {
      this.line = this.line.substring(0, this.cursorPosition)
	+ ch
	+ this.line.substring(this.cursorPosition, this.line.length);
    }
    else {
      this.line += ch;
    }

    this.addIndent();
    this.syncHistory(false);
    this.keyRight();
  };

  InputUI.prototype.getLine = function(offset) {
    var matches = this.line.match(/.*\n|.+$/g);
    var cursorPos = this.getCursorPos();
    var lineIndex = cursorPos.rows + offset;

    lineIndex = lineIndex < 0 ? 0 : lineIndex;
    lineIndex = lineIndex > matches.length - 1? matches.length - 1 : lineIndex;

    if(matches) {
      return matches[lineIndex];
    }
    else {
      return this.line;
    }
  };

  InputUI.prototype.prettify = function(line) {
    var matches = line.match(/.*\n|.+$/g);

    if(matches) {
      var lastMatch = matches[matches.length - 1];
      var s = matches.shift();
      var lineNumber = 2;
      var startLine = lineNumber++
	+ "  "
	+ new Array(this.interactionsNumber.toString().length + 1).join(" ")
	+ "... ";

      matches.forEach(function(m) {
	s += startLine + m;
	startLine = lineNumber
	  + new Array(3 - (lineNumber++).toString().length + 1).join(" ")
	  + new Array(this.interactionsNumber.toString().length + 1).join(" ")
	  + "... ";
      }, this);

      if(lastMatch.charAt(lastMatch.length - 1) === "\n") {
	s += startLine;
      }

      return s;
    }
    else {
      return line;
    }
  }

  InputUI.prototype.addIndent = function() {
    var lineNoIndent = this.line.replace(/^(\s*)/, "");
    var lineIndent = indenter.getIndent(this.line, this.nestStack, this.indent)
      + lineNoIndent;

    this.cursorPosition += lineIndent.length - this.line.length;
    this.line = lineIndent;
  };

  InputUI.prototype.getInteractionsNumber = function() {
    return this.interactionsNumber;
  };

  InputUI.prototype.getCursorPos = function() {
    return this.getDisplayPos(
	this.promptString
	+ this.prettify(this.line.slice(0, this.cursorPosition)));
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
    var prettified = this.prettify(this.line);
    var line = this.promptString + prettified;
    var dispPos = this.getDisplayPos(line);
    var lineCols = dispPos.cols;
    var lineRows = dispPos.rows;

    // cursor position
    if(gotoEol || this.cursorPosition > this.line.length) {
      this.cursorPosition = this.line.length;
    }
    else if(this.cursorPosition < 0) {
      this.cursorPosition = 0;
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
    this.output.write(renderer.highlightLine(prettified));

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

    if(!(this.line.match(spaceRegex)) || this.line === "") {
      this.history[this.historyIndex] = {
	"old": this.history[this.historyIndex].old,
	"cur": this.line};
    }

    if(isNewline) {
      if(this.historyUpdate >= 0) {
	this.history = this.history.slice(0, this.historyUpdate).map(function(l) {
	  return {"old": l.old, "cur": l.old};
	}).concat(this.history.slice(this.historyUpdate, this.history.length));
      }

      if(!(this.line.match(spaceRegex) || (this.history.length > 1 &&
	      this.history[1].old === this.line))) {
	this.history[0] = {
	  "old": this.line,
	  "cur": this.line};
	this.history.unshift({"old": "", "cur": ""});
      }

      this.historyIndex = 0;
      this.historyUpdate = 0;
    }
    else {
      this.historyUpdate = Math.max(this.historyIndex + 1, this.historyUpdate);
    }
  };

  InputUI.prototype.enter = function() {
    this.syncHistory(true);
    this.syncLine(true);
    this.output.write("\n");
    this.emit('command', this.line);
  };

  /*
  InputUI.prototype.enter = function() {
    var matches = this.line.match(/.*\n|.+$/g);
    var cmdTrimmed;

    this.syncHistory(true);
    this.syncLine(true);

    if(matches && matches.length > 1) {
      var lastMatch = matches.pop();

      matches.forEach(function(m) {
	cmdTrimmed = m.replace(/(\s*)$/, "");
	this.newline(cmdTrimmed, false);
      }, this);

      cmdTrimmed = lastMatch.replace(/(\s*)$/, "");
      this.newline(cmdTrimmed, true);
    }
    else {
      this.newline(this.line, true);
    }
  };
  */

  InputUI.prototype.newline = function(cmd, printPrompt) {
    var newCmd = cmd;
    var unindent = indenter.unindent(cmd);

    if(unindent) {
      this.nestStack.unshift(unindent);
    }

    if(indenter.matchNested(cmd)) {
      this.commandQueue.push(cmd);

      if(printPrompt) {
	this.prompt();
      }
    }
    else if(indenter.matchColon(cmd)) {
      this.commandQueue.push(cmd);

      var indent = indenter.indent(cmd);

      if(indent) {
	this.nestStack.unshift(indent);
      }

      if(printPrompt) {
	this.prompt();
      }
    }
    else if(indenter.matchEnd(cmd)) {
      this.commandQueue.push(cmd);

      var lastNest = this.nestStack.shift();

      while(lastNest && lastNest !== Indenter.INDENT_SINGLE
	  && lastNest !== Indenter.INDENT_DOUBLE) {
	lastNest = this.nestStack.shift();
      }

      if(this.nestStack.length > 0) {
	if(printPrompt) {
	  this.prompt();
	}
      }
      else {
	var newCmd = this.commandQueue.join("\n");
	this.resetNest(newCmd);
      }
    }
    else if(this.nestStack.length > 0) {
      this.commandQueue.push(cmd);

      if(printPrompt) {
	this.prompt();
      }
    }
    else {
      this.resetNest(newCmd);
    }
  };

  InputUI.prototype.historyPrev = function() {
    if(this.historyIndex < this.history.length - 1) {
      this.historyIndex += 1;
      this.line = this.history[this.historyIndex].cur;
      this.syncLine(true);
    }
  };

  InputUI.prototype.historyNext = function() {
    if(this.historyIndex > 0) {
      this.historyIndex -= 1;
      this.line = this.history[this.historyIndex].cur;
      this.syncLine(true);
    }
  };

  /*
  InputUI.prototype.blockHistoryPrev = function() {
    if(this.historyIndex < this.history.length - 1) {
      this.historyIndex += 1;

      var lastEntry = this.history[this.historyIndex];

      while(this.historyIndex < this.history.length - 1 && lastEntry.block === "") {
	this.historyIndex++;
	lastEntry = this.history[this.historyIndex];
      }

      if(lastEntry.block === "") {
	this.line = lastEntry.cur;
      }
      else {
	this.line = lastEntry.block;
      }

      this.syncLine(true);
    }
  };

  InputUI.prototype.blockHistoryNext = function() {
    if(this.historyIndex > 0) {
      this.historyIndex -= 1;

      var lastEntry = this.history[this.historyIndex];

      while(this.historyIndex > 0 && lastEntry.block === "") {
	this.historyIndex--;
	lastEntry = this.history[this.historyIndex];
      }

      if(lastEntry.block === "") {
	this.line = lastEntry.cur;
      }
      else {
	this.line = lastEntry.block;
      }

      this.syncLine(true);
    }
  };
  */

  InputUI.prototype.keyUpBase = function() {
    if(numLines(this.line.slice(0, this.cursorPosition)) > 1) {
      var curLine = this.getLine(-1);

      if(curLine[curLine.length - 1] !== "\n") {
	this.cursorPosition -= curLine.length + 1;
      }
      else {
	this.cursorPosition -= curLine.length;
      }

      this.syncLine();
    }
    else {
      this.historyPrev();
    }
  };

  InputUI.prototype.keyDownBase = function() {
    if(numLines(this.line.slice(this.cursorPosition, this.line.length)) > 1) {
      var curLine = this.getLine(0);

      if(curLine[curLine.length - 1] !== "\n") {
	this.cursorPosition += curLine.length + 1;
      }
      else {
	this.cursorPosition += curLine.length;
      }

      this.syncLine();
    }
    else {
      this.historyNext();
    }
  };

  //TODO: make including this functionality command line options
  InputUI.prototype.keyUp = function() {
    if(this.historyIndex < this.history.length - 1 &&
	numLines(this.line) > 1 &&
	numLines(this.line.slice(0, this.cursorPosition)) === 1) {
      if(this.lastKey === "up") {
	this.lastKey = "";
	clearTimeout(this.upVar);

	this.keyUpBase();
      }
      else {
	this.lastKey = "up";

	this.upVar = setTimeout(function() {
	  this.lastKey = "";
	}.bind(this), 200);
      }
    }
    else {
      this.keyUpBase();
    }
  };

  InputUI.prototype.keyDown = function() {
    if(this.historyIndex > 0 &&
	numLines(this.line) > 1 &&
	numLines(this.line.slice(0, this.cursorPosition))
	=== numLines(this.line)) {
      if(this.lastKey === "down") {
	this.lastKey = "";
	clearTimeout(this.downVar);

	this.keyDownBase();
      }
      else {
	this.lastKey = "down";

	this.downVar = setTimeout(function() {
	  this.lastKey = "";
	}.bind(this), 200);
      }
    }
    else {
      this.keyDownBase();
    }
  };

  InputUI.prototype.keyRight = function() {
    if(this.cursorPosition < this.line.length) {
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
      this.line = this.line.substring(0, this.cursorPosition - 1)
	+ this.line.substring(this.cursorPosition, this.line.length);

      this.syncHistory(false);
    }

    this.keyLeft();
  };

  InputUI.prototype.keyboardInterrupt = function() {
    this.syncLine(true);

    if(this.nestStack.length > 0 || this.line !== "") {
      this.nestStack = [];
      this.commandQueue = [];
      //this.lineNumber = 0;

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
