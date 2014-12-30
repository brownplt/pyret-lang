/*global define */
/*jslint unparam: true, todo: true, node: true*/

var events = require("events");
var keypress = require("keypress");
//var chalk = require("chalk");

define([], function() {
  function highlightLine(line) {

  }

  function onKeypress(ch, key) {
    if(key && key.name === "return") {
      var cmdTrimmed = this.curLine.trim();
      var cmdLen = cmdTrimmed.length;
      var newCmd = cmdTrimmed;

      this.syncHistory();
      this.history.unshift(this.curLine);
      this.curLine = "";
      this.cursorPosition = 0;

      if(cmdLen > 2) {
	if(cmdTrimmed.substring(cmdLen - 2, cmdLen) == "=>") {
	  this.commandQueue.push(cmdTrimmed);

	  if(cmdTrimmed.substring(0, 1) != "|") {
	    this.nestStack.unshift("=>");
	  }

	  this.prompt();
	  return;
	}
	else if (cmdTrimmed.substring(cmdLen - 1, cmdLen) == ":") {
	  this.commandQueue.push(cmdTrimmed);

	  if(cmdTrimmed.substring(0, 1) != "|" && cmdTrimmed.substring(0, 4) != "else") {
	    this.nestStack.unshift(":");
	  }

	  this.prompt();
	  return;
	}
	else if(cmdTrimmed == "end") {
	  if(this.nestStack.length === 1) {
	    this.commandQueue.push(cmdTrimmed);

	    /*
	    this.commandQueue.forEach(function(cmd) {
	      this.history.shift();
	    });
	    */
	    newCmd = this.commandQueue.join("\n");
	    this.commandQueue = [];
	  }
	  else if (this.nestStack.length > 1) {
	    this.commandQueue.push(cmdTrimmed);
	    this.nestStack.shift();
	    this.prompt();
	    return;
	  }
	}
	else if(this.nestStack.length > 0) {
	  this.commandQueue.push(cmdTrimmed);
	  this.prompt();
	  return;
	}
      }
      else if(this.nestStack.length > 0) {
	this.commandQueue.push(cmdTrimmed);
	this.prompt();
	return;
      }

      this.lineNumber = 1;
      this.nestStack = [];

      this.emit('command', newCmd);
    }
    else if(key && key.name === "up") {
      if(this.history.length > 0) {
	this.future.unshift(this.curLine);
	this.curLine = this.history.shift();
	this.syncLine(true);
      }
    }
    else if(key && key.name === "down") {
      if(this.future.length > 0) {
	this.history.unshift(this.curLine);
	this.curLine = this.future.shift();
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
      this.syncHistory();

      if(this.cursorPosition > 0) {
	this.curLine = this.curLine.substring(0, this.cursorPosition - 1)
	  + this.curLine.substring(this.cursorPosition, this.curLine.length);
      }

      this.syncLine();
      this.keyLeft();
    }
    else if(key && key.ctrl && key.name === "c") {
      if(this.nestStack.length > 0) {
	//Note(ben) abstract these resets further
	this.nestStack = [];
	this.commandQueue = [];
	this.curLine = "";
	this.prompt();
      }
      else {
	process.exit();
      }
    }
    //TODO: how to decide what keys pass through?
    else {
      //Note(ben) should call all syncs on edits
      this.syncHistory();

      if(this.cursorPosition < this.curLine.length) {
	this.curLine = this.curLine.substring(0, this.cursorPosition)
	  + ch
	  + this.curLine.substring(this.cursorPosition, this.curLine.length);
	//this.curLine = highlightLine(this.curLine);
      }
      else {
	this.curLine += ch;
      }

      this.syncLine();
      this.keyRight();
    }
  }

  //TODO: add options to this function
  return function(runtime) {
    function InputUI(rt, input, output) {
      this.runtime = rt;
      this.input = input;
      this.output = output;
      keypress(this.input);

      this.history = [];
      this.future = [];
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
      this.output.write("\n");

      if(this.nestStack.length === 0) {
	this.interactionsNumber += 1;
      }

      this.promptString = this.interactionsNumber + "::" + this.lineNumber++ + " " + this.promptSymbol + " ";

      this.output.write(this.promptString + this.getIndent());
    };

    InputUI.prototype.getIndent = function() {
      if(this.curLine.trim() === "end") {
	return new Array(this.nestStack.length).join(this.indent);
      }

      return new Array(this.nestStack.length + 1).join(this.indent);
    };

    InputUI.prototype.getLineNumber = function() {
      return this.lineNumber
    };

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

    InputUI.prototype.syncCursor = function() {
      this.output.cursorTo(this.cursorPosition + this.promptString.length + this.getIndent().length);
    };

    InputUI.prototype.syncLine = function(gotoEol) {
      this.output.clearLine();
      this.output.cursorTo(0);
      this.output.write(this.promptString + this.getIndent() + this.curLine);

      if(gotoEol) {
	this.cursorPosition = this.curLine.length;
      }
    };

    InputUI.prototype.syncHistory = function() {
      //get rid of other empties
      if(this.future.length > 0) {
	this.history.unshift(this.curLine);

	this.future = this.future.filter(function(l) {
	  return l !== "";
	});

	while(this.future.length > 0) {
	  this.history.unshift(this.future.shift());
	}
      }
    };

    InputUI.prototype.__proto__ = events.EventEmitter.prototype;

    return new InputUI(runtime, process.stdin, process.stdout);
  }
});
