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
    if(key && (key.name === "return" || key.name === "enter")) {
      this.return();
    }
    else if(key && key.name === "tab") {
      this.tab();
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
      if(ch) {
	this.addChar(ch);
      }
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
    this.cursorPosition = 0;

    this.lastKey = "";

    this.indentArray = [];

    this.input.setRawMode(true);
    this.input.setEncoding("utf8");
    this.input.resume();
    this.input.on("keypress", onKeypress.bind(this));
  }

  /*Prompt*/
  InputUI.prototype.prompt = function() {
    if(this.indentArray.length === 0) {
      this.interactionsNumber += 1;
    }

    this.resetLine();

    this.promptString = this.interactionsNumber
      + "::1 "
      + this.promptSymbol + " ";
    this.output.write("\n" + this.promptString + this.line);
  };

  /*Getters*/
  InputUI.prototype.getInteractionsNumber = function() {
    return this.interactionsNumber;
  };

  InputUI.prototype.getLine = function(offset, ignoreNl) {
    var matches = this.line.match(/.*\n|.+$/g);
    var cursorPos = this.getCursorPos();
    var lineIndex = cursorPos.rows + offset;

    if(matches) {
      lineIndex = lineIndex < 0 ? 0 : lineIndex;
      lineIndex = lineIndex > matches.length - 1 ? matches.length - 1 : lineIndex;

      var lastMatch = matches[matches.length - 1];

      if(lineIndex === matches.length - 1 &&
	  lastMatch.charAt(lastMatch.length - 1) === "\n" &&
	  offset >= 0 && !ignoreNl) {
	return "";
      }
      else {
	return matches[lineIndex];
      }
    }
    else {
      return this.line;
    }
  };

  InputUI.prototype.getLinesBefore = function(offset) {
    var matches = this.line.match(/.*\n|.+$/g);
    var cursorPos = this.getCursorPos();
    var lineIndex = cursorPos.rows + offset;

    if(matches) {
      lineIndex = lineIndex < 0 ? 0 : lineIndex;
      lineIndex = lineIndex > matches.length ? matches.length : lineIndex;

      var lastMatch = matches[matches.length - 1];

      if(lineIndex === matches.length - 1
	  && lastMatch.charAt(lastMatch.length - 1) === "\n") {
	return matches;
      }
      else {
	return matches.slice(0, lineIndex);
      }
    }
    else {
      return [];
    }
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

  /*Reset functions*/
  InputUI.prototype.resetLine = function() {
    this.line = "";
    this.cursorPosition = 0;
    this.rowOffset = 0;
  };

  InputUI.prototype.resetNest = function(cmd) {
    this.indentArray = [];

    this.output.write("\n");
    this.emit('command', cmd);
  };

  /*Modifying state functions*/
  InputUI.prototype.addChar = function(ch) {
    if(this.cursorPosition < this.line.length) {
      this.line = this.line.substring(0, this.cursorPosition)
	+ ch
	+ this.line.substring(this.cursorPosition, this.line.length);
    }
    else {
      this.line += ch;
    }

    this.cursorPosition += 1;

    if(ch === " ") {
      this.refreshLine();
    }
    else {
      this.addIndent();
    }

    this.syncHistory();
  };

  InputUI.prototype.addIndent = function() {
    this.syncIndentArray(0);

    var curLine = this.getLine(0, false);
    var indent = indenter.getIndent(curLine, this.indentArray, this.indent);
    var lineNoIndent = curLine.replace(/^([^\S\n]+)/, "");
    var lineIndent = indent + lineNoIndent;
    var diff = lineIndent.length - curLine.length;

    var oldCursorPos = this.getCursorPos();
    this.line = this.replaceLine(0, lineIndent);
    this.cursorPosition += diff;
    var newCursorPos = this.getCursorPos();

    if(oldCursorPos.rows !== newCursorPos.rows) {
      this.cursorPosition -= diff;
      this.cursorPosition -= (oldCursorPos.cols - this.promptString.length);
    }

    this.refreshLine();
  };

  InputUI.prototype.addHistory = function() {
    var spaceRegex = /^\s*$/g;

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
  };

  InputUI.prototype.syncHistory = function() {
    this.history[this.historyIndex] = {
      "old": this.history[this.historyIndex].old,
      "cur": this.line};

    this.historyUpdate = Math.max(this.historyIndex + 1, this.historyUpdate);
  };

  InputUI.prototype.syncIndentArray = function(offset) {
    var matches = this.getLinesBefore(offset);
    this.indentArray = [];

    if(matches) {
      matches.forEach(function(cmd) {
	cmd = cmd.trim();
	this.indentArray = indenter.indent(cmd, this.indentArray);
      }, this);
    }
  };

  /*Display functions*/
  InputUI.prototype.replaceLine = function(offset, str) {
    var matches = this.line.match(/.*\n|.+$/g);
    var cursorPos = this.getCursorPos();
    var lineIndex = cursorPos.rows + offset;

    if(matches) {
      lineIndex = lineIndex < 0 ? 0 : lineIndex;
      lineIndex = lineIndex > matches.length - 1 ? matches.length - 1 : lineIndex;

      var lastMatch = matches[matches.length - 1];

      if(lineIndex === matches.length - 1
	  && lastMatch.charAt(lastMatch.length - 1) === "\n") {
	matches.push(str);
      }
      else {
	matches[lineIndex] = str;
      }

      return matches.join("");
    }
    else {
      return "";
    }
  };

  InputUI.prototype.prettify = function(line) {
    var matches = line.match(/.*\n|.+$/g);

    if(matches) {
      var lastMatch = matches[matches.length - 1];
      var s = renderer.highlightLine(matches.shift());
      var lineNumber = 2;
      var startLine = lineNumber++
	+ "  "
	+ new Array(this.interactionsNumber.toString().length + 1).join(" ")
	+ "... ";

      matches.forEach(function(m) {
	s += startLine + renderer.highlightLine(m);
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
  };

  InputUI.prototype.refreshLine = function(gotoEol) {
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
    this.output.write(line);

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

  /*Keypress and related functions*/
  InputUI.prototype.return = function(cmd) {
    if(this.canRun()) {
      this.run();
    }
    else {
      this.addChar("\n");
    }
  };

  InputUI.prototype.canRun = function() {
    var curLine = this.getLine(0, false);
    var cursorPos = this.getCursorPos();
    var savePos = this.cursorPosition;

    this.cursorPosition = this.line.length;
    var displayPos = this.getCursorPos();
    this.cursorPosition = savePos;
    this.syncIndentArray(1);

    return (this.indentArray.length === 0 && displayPos.rows === 0)
      || (curLine === "" && cursorPos.rows === displayPos.rows);
  };

  InputUI.prototype.run = function() {
    this.addHistory();
    this.refreshLine(true);
    this.output.write("\n");
    this.emit('command', this.line);
  };

  InputUI.prototype.tab = function() {
    if(this.lastKey === "tab") {
      this.lastKey = "";
      clearTimeout(this.tabVar);

      var oldCursorPos = this.cursorPosition;
      var lastCursorPos = -1;
      this.cursorPosition = 0;

      while(this.cursorPosition !== lastCursorPos) {
	lastCursorPos = this.cursorPosition;
	this.addIndent();
	this.keyDownBase(true);
      }

      this.cursorPosition = oldCursorPos;
      this.refreshLine();
    }
    else {
      this.lastKey = "tab";

      this.tabVar = setTimeout(function() {
	this.lastKey = "";
	this.addIndent();
      }.bind(this), 200);
    }
  };

  InputUI.prototype.historyPrev = function() {
    if(this.historyIndex < this.history.length - 1) {
      this.historyIndex += 1;
      this.line = this.history[this.historyIndex].cur;
      this.refreshLine(true);
    }
  };

  InputUI.prototype.historyNext = function() {
    if(this.historyIndex > 0) {
      this.historyIndex -= 1;
      this.line = this.history[this.historyIndex].cur;
      this.refreshLine(true);
    }
  };

  InputUI.prototype.keyUpBase = function(noHistory) {
    if(numLines(this.line.slice(0, this.cursorPosition)) > 1) {
      var nextLine = this.getLine(-1, true);
      var oldCursorPos = this.getCursorPos();
      this.cursorPosition -= nextLine.length;
      var newCursorPos = this.getCursorPos();

      if(newCursorPos.rows !== oldCursorPos.rows - 1 && oldCursorPos.rows > 0) {
	this.cursorPosition += nextLine.length;
	this.cursorPosition -= (oldCursorPos.cols - this.promptString.length) + 1;
      }
      else {
	this.refreshLine();
      }

    }
    else if(!noHistory) {
      this.historyPrev();
    }
  };

  InputUI.prototype.keyDownBase = function(noHistory) {
    if(numLines(this.line.slice(this.cursorPosition, this.line.length)) > 1) {
      var curLine = this.getLine(0, true);
      var nextLine = this.getLine(1, true);
      var oldCursorPos = this.getCursorPos();
      this.cursorPosition += curLine.length;
      var newCursorPos = this.getCursorPos();

      if(newCursorPos.rows !== oldCursorPos.rows + 1) {
	this.cursorPosition -= (oldCursorPos.cols - this.promptString.length);
	this.cursorPosition += nextLine.length - 1;
      }

      this.refreshLine();
    }
    else if(!noHistory) {
      this.historyNext();
    }
  };

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

    this.refreshLine();
  };

  InputUI.prototype.keyLeft = function() {
    if(this.cursorPosition > 0) {
      this.cursorPosition -= 1;
    }

    this.refreshLine();
  };

  InputUI.prototype.backspace = function() {
    if(this.cursorPosition > 0) {
      this.line = this.line.substring(0, this.cursorPosition - 1)
	+ this.line.substring(this.cursorPosition, this.line.length);

      this.cursorPosition -= 1;
      this.refreshLine();
      this.syncHistory();
    }
  };

  InputUI.prototype.keyboardInterrupt = function() {
    this.refreshLine(true);

    if(this.indentArray.length > 0 || this.line !== "") {
      this.indentArray = [];

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
