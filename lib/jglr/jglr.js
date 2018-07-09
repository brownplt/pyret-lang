define("jglr/jglr", ["jglr/rnglr"], function(E) {
  const SrcLoc = E.SrcLoc
  const NS_PER_SEC = 1e9;
  var start;
  function startTimer() {
    if (typeof window !== "undefined" && window.performance) {
      start = window.performance.now();
    } else if (typeof process !== "undefined" && process.hrtime) {
      start = process.hrtime();
    }
  }
  function endTimer() {
    if (typeof window !== "undefined" && window.performance) {
      return window.performance.now() - start;
    } else if (typeof process !== "undefined" && process.hrtime) {
      return process.hrtime(start);
    }
  }

  /*! https://mths.be/startswith v0.2.0 by @mathias */
  if (!String.prototype.startsWith) {
    (function() {
      'use strict'; // needed to support `apply`/`call` with `undefined`/`null`
      var defineProperty = (function() {
        // IE 8 only supports `Object.defineProperty` on DOM elements
        try {
	  var object = {};
	  var $defineProperty = Object.defineProperty;
	  var result = $defineProperty(object, object, object) && $defineProperty;
        } catch(error) {}
        return result;
      }());
      var toString = {}.toString;
      var startsWith = function(search) {
        if (this == null) {
	  throw TypeError();
        }
        var string = String(this);
        if (search && toString.call(search) == '[object RegExp]') {
	  throw TypeError();
        }
        var stringLength = string.length;
        var searchString = String(search);
        var searchLength = searchString.length;
        var position = arguments.length > 1 ? arguments[1] : undefined;
        // `ToInteger`
        var pos = position ? Number(position) : 0;
        if (pos != pos) { // better `isNaN`
	  pos = 0;
        }
        var start = Math.min(Math.max(pos, 0), stringLength);
        // Avoid the `indexOf` call if no match is possible
        if (searchLength + start > stringLength) {
	  return false;
        }
        var index = -1;
        while (++index < searchLength) {
	  if (string.charCodeAt(start + index) != searchString.charCodeAt(index)) {
	    return false;
	  }
        }
        return true;
      };
      if (defineProperty) {
        defineProperty(String.prototype, 'startsWith', {
	  'value': startsWith,
	  'configurable': true,
	  'writable': true
        });
      } else {
        String.prototype.startsWith = startsWith;
      }
    }());
  }




  //////////////////////////////////////////////////
  ///////////////////////// Old Tokenizer
  //////////////////////////////////////////////////

  var Tokenizer = (function() {
    function Tokenizer(ignore_ws, Tokens) {
      this.ignore_ws = ignore_ws;
      this.Tokens = Tokens;
    }
    Tokenizer.prototype.isEOF = function(tok) { return tok === E.EOF; }
    Tokenizer.prototype.tokenizeFrom = function(str) {
      this.str = str;
      this.curLine = 1;
      this.curCol = 0;
      this.pos = 0;
      this.len = str.length;
      delete this.curTok;
      delete E.EOF.pos;
      SrcLoc.reset();
      this.times = {};
    }
    const STICKY_REGEXP = ''; //(function() { try { new RegExp("", 'y'); return 'y' } catch(e) { return ''; }})();

    Tokenizer.prototype.hasNext = function() { return this.pos <= this.len; }
    Tokenizer.prototype.isEmpty = function() { return this.length == 0; }
    if (STICKY_REGEXP === 'y') {
      Tokenizer.prototype.positionTokenRegexp = function(tok) { tok.val.lastIndex = this.pos; };
      Tokenizer.prototype.updateString = function(match) { };
      Tokenizer.prototype.updatePosition = function(tok, match) { this.pos = tok.val.lastIndex; };
    } else {
      Tokenizer.prototype.positionTokenRegexp = function(tok) { };
      Tokenizer.prototype.updateString = function(match) { this.str = this.str.slice(match[0].length); };
      Tokenizer.prototype.updatePosition = function(tok, match) { this.pos += match[0].length; };
    }
    Tokenizer.prototype.makeToken = function (tok_type, s, pos) { 
      var t = new E.Token(tok_type, s);
      t.pos = pos;
      return t;
    }
    Tokenizer.prototype.postProcessMatch = function(tok, match, str) { return tok.name; }
    Tokenizer.prototype.unshiftToken = function(prev_tok) { this.curTok = prev_tok; }
    Tokenizer.prototype.next = function() {
      delete this.skippedWhitespace;
      startTimer();
      while (this.hasNext()) { // Surround the tokenizer loop...
        if (this.pos == this.len) { 
          this.pos++; 
          E.EOF.pos = SrcLoc.make(this.curLine, this.curCol, this.pos, this.curLine, this.curCol, this.pos);
          this.curTok = E.EOF;
          return E.EOF; 
        }
        for (var i = 0; i < this.Tokens.length; i++) {
          var tok = this.Tokens[i];
          this.positionTokenRegexp(tok);
          var match = tok.val.exec(this.str);
          if (match !== null) {
            var tok_type = this.postProcessMatch(tok, match, this.str);
            this.updateString(match);
            var p = this.pos;
            var l = this.curLine;
            var c = this.curCol;
            var s = match[0];
            var lines = s.split("\n").length - 1; // From jsPerf, this is UNBELIEVABLY much faster than the prior implementation
            this.updatePosition(tok, match);
            this.curLine += lines;
            if (lines === 0)
              this.curCol += s.length;
            else
              this.curCol = s.length - s.lastIndexOf("\n") - 1;
            if (this.ignore_ws && (tok_type === "WS" || tok_type === "COMMENT")) {
              // ... in case we're ignoring whitespace
              var endT = endTimer();
              if (this.times[tok_type] === undefined) this.times[tok_type] = [];
              this.times[tok_type].push((endT[0] * NS_PER_SEC + endT[1]) / NS_PER_SEC);
              var newWS = SrcLoc.make(l, c, p, this.curLine, this.curCol, this.pos);
              if (this.skippedWhitespace === undefined)
                this.skippedWhitespace = newWS;
              else
                this.skippedWhitespace = this.skippedWhitespace.combine(newWS);
              break;
            }
            var t = this.makeToken(tok_type, s, SrcLoc.make(l, c, p, this.curLine, this.curCol, this.pos));
            this.curTok = t;
            var endT = endTimer();
            if (this.times[this.curTok.name] === undefined) this.times[this.curTok.name] = [];
            this.times[this.curTok.name].push((endT[0] * NS_PER_SEC + endT[1]) / NS_PER_SEC);
            return t;
          }
        }
      }
    }

    return Tokenizer;
  })();

  //////////////////////////////////////////////////
  ///////////////////////// New Tokenizer
  //////////////////////////////////////////////////
  
  var Tokenizer2 = (function() {
    const ws = new RegExp("\\s+", "g");

    function merge(dest, src) {
      for (var first in src) {
        var cur = dest[first];
        if (cur === undefined)
          cur = dest[first] = [];
        var ext = src[first];
        for (var i = 0; i < ext.length; i++) {
          cur.push(ext[i]);
        }
      }
    }
    
    function Tokenizer(spec) {
      this.ignore = spec.ignore;

      this.whitespace = spec.whitespace;
      this.keywords = Object.create(null);
      merge(this.keywords, spec.symbols);
      merge(this.keywords, spec.comments);
      merge(this.keywords, spec.keywords);
      for (var first in this.keywords) {
        this.keywords[first].sort(function(a, b) {
          // sort in descending length, then by which needs priorWhitespace, then alphabetically
          if (b.val.length === a.val.length) {
            if (a.needsWs && !b.needsWs) return -1;
            if (b.needsWs && !a.needsWs) return 1;
            return (a < b) ? -1 : 0;
          }
          return b.val.length - a.val.length;
        });
      }
    }
    Tokenizer.prototype.tokenizeFrom = function(str) {
      if (typeof str.normalize === "function")
        this.str = str.normalize("NFC");
      else
        this.str = str;
      
      this.len = str.length; // XXX Not necessarily unicode-aware
      this.curCol = 0;
      this.curLine = 1;
      this.pos = 0;
      this.priorWhitespace = true;
      delete this.curTok;
      delete E.EOF.pos;
      SrcLoc.reset();
      this.times = {};
    }
    Tokenizer.prototype.isEOF = function(tok) { return tok === E.EOF; }
    Tokenizer.prototype.isEmpty = function() { return this.length == 0; }
    Tokenizer.prototype.hasNext = function() { return this.pos <= this.len; }
    Tokenizer.prototype.unshiftToken = function(prev_tok) { this.curTok = prev_tok; }
    Tokenizer.prototype.addWhitespace = function(ws_loc) {
      if (this.skippedWhitespace === undefined)
        this.skippedWhitespace = ws_loc;
      else
        this.skippedWhitespace = this.skippedWhitespace.combine(ws_loc);
    }
    Tokenizer.prototype.next = function() {
      delete this.skippedWhitespace;
      this.curTok = undefined;
      var startT = startTimer();
      while (this.curTok === undefined && this.hasNext()) {
        if (this.pos == this.len) { 
          this.pos++;
          this.curCol++;
          E.EOF.pos = SrcLoc.make(this.curLine, this.curCol, this.pos, this.curLine, this.curCol, this.pos);
          this.curTok = E.EOF;
          return E.EOF; 
        }
        var c = this.str[this.pos];  // XXX not unicode aware
        var candidates = this.keywords[c];
        var line = this.curLine, col = this.curCol, pos = this.pos;
        if (candidates) {
          for (var i = 0; i < candidates.length; i++) {
            var candidate = candidates[i];
            if (this.str.startsWith(candidate.val, this.pos)) {
              if (candidate.process) {
                this.curTok = candidate.process.call(this, candidate);
                if (this.curTok) break;
              } else {
                var noFollow = candidate.noFollow;
                var mustFollow = candidate.mustFollow;
                var match = false;
                if (candidate.needsWs && !this.priorWhitespace) {
                  match = false;
                } else if (!noFollow && !mustFollow) {
                  match = true;
                } else if (noFollow && !noFollow.has(this.str[this.pos + candidate.val.length])) {
                  match = true;
                } else if (mustFollow && mustFollow.has(this.str[this.pos + candidate.val.length])) {
                  match = true;
                }
                if (match) {
                  this.curCol += candidate.val.length;
                  this.pos += candidate.val.length;
                  var t = this.makeToken(candidate.name, candidate.val,
                                         SrcLoc.make(line, col, pos, this.curLine, this.curCol, this.pos),
                                         candidate);
                  this.priorWhitespace = false;
                  this.curTok = t;
                  break;
                }
              }
            }
          }
        }
        // Handle whitespace
        if (this.curTok === undefined && this.whitespace[c]) {
          this.priorWhitespace = true;
          while (this.whitespace[c]) {
            this.pos++;
            this.curCol++;
            if (c === "\n") {
              this.curLine++;
              this.curCol = 0;
            }
            c = this.str[this.pos];  // XXX not unicode aware
          }
          this.curTok = this.makeWSToken(line, col, pos);
        }
        if (this.curTok === undefined) {
          // FAILOVER
          var t = this.makeToken("UNKNOWN", this.str[this.pos],
                                 SrcLoc.make(line, col, pos, line, col + 1, pos + 1));
          this.curCol++;
          this.pos++;
          this.curTok = t;
        }

        if (this.ignore.has(this.curTok.name)) {
          var endT = endTimer();
          if (this.times[this.curTok.name] === undefined) this.times[this.curTok.name] = [];
          this.times[this.curTok.name].push((endT[0] * NS_PER_SEC + endT[1]) / NS_PER_SEC);
          this.curTok = undefined;
        }
      }
      var endT = endTimer();
      if (this.times[this.curTok.name] === undefined) this.times[this.curTok.name] = [];
      this.times[this.curTok.name].push((endT[0] * NS_PER_SEC + endT[1]) / NS_PER_SEC);
      return this.curTok;
    }

    return Tokenizer;
  })();

  return {
    'Tokenizer': Tokenizer,
    'Tokenizer2': Tokenizer2,
    'STICKY_REGEXP': '',
    'Atom': E.Atom,
    'Nonterm': E.Nonterm,
    'Token': E.Token,
    'Rule': E.Rule,
    'Grammar': E.Grammar,
    'SetOfSets': E.SetOfSets,
    'EOF': E.EOF,
    'EPSILON': E.EPSILON,
    'SrcLoc': E.SrcLoc
  };
});
