define("nearley/nearley-helpers", [], function () {
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

  function SrcLoc(startRow, startCol, startChar, endRow, endCol, endChar) {
    this.startRow = startRow;
    this.startCol = startCol;
    this.startChar = startChar;
    this.endRow = endRow;
    this.endCol = endCol;
    this.endChar = endChar;
  }
  SrcLoc.cache = {};
  SrcLoc.reset = function () { SrcLoc.cache = {}; }
  SrcLoc.make = function (startRow, startCol, startChar, endRow, endCol, endChar) {
    var byStartChar = SrcLoc.cache[startChar];
    if (byStartChar === undefined)
      byStartChar = SrcLoc.cache[startChar] = {};
    var byEndChar = byStartChar[endChar];
    if (byEndChar === undefined)
      byEndChar = byStartChar[endChar] = new SrcLoc(startRow, startCol, startChar, endRow, endCol, endChar);
    return byEndChar;
  }
  SrcLoc.prototype.toString = function (show_span) {
    var ret = this.startRow + ":" + this.startCol;
    if (show_span)
      ret += "-" + this.endRow + ":" + this.endCol;
    return ret;
  }
  SrcLoc.prototype.combine = function (that) {
    if (this.startChar < that.startChar) {
      if (this.endChar < that.endChar) {
        return SrcLoc.make(this.startRow, this.startCol, this.startChar, that.endRow, that.endCol, that.endChar);
      } else {
        return SrcLoc.make(this.startRow, this.startCol, this.startChar, this.endRow, this.endCol, this.endChar);
      }
    } else {
      if (this.endChar < that.endChar) {
        return SrcLoc.make(that.startRow, that.startCol, that.startChar, that.endRow, that.endCol, that.endChar);
      } else {
        return SrcLoc.make(that.startRow, that.startCol, that.startChar, this.endRow, this.endCol, this.endChar);
      }
    }
  }
  SrcLoc.prototype.posAtStart = function () {
    return SrcLoc.make(this.startRow, this.startCol, this.startChar, this.startRow, this.startCol, this.startChar);
  }
  SrcLoc.prototype.posAtEnd = function () {
    return SrcLoc.make(this.endRow, this.endCol, this.endChar, this.endRow, this.endCol, this.endChar);
  }

  function Atom() { }
  Atom.equals = function (thiz, that) {
    if (thiz === that) return true;
    if ((thiz instanceof Nonterm) && (that instanceof Nonterm) && (thiz.name == that.name)) return true;
    if ((thiz instanceof Token) && (that instanceof Token) && (thiz.name == that.name)) return true;
    return false;
  }
  Atom.equals.toString = function () { return "Atom.equals" };
  Atom.fromSerializable = function (obj) {
    if (obj === undefined) return undefined;
    if (typeof obj === "string") {
      if (obj === "EOF") return EOF;
      if (obj === "EPSILON") return EPSILON;
      if (obj === "HASH") return HASH;
      var c = obj.charAt(0);
      if (c === "'") return new Token(obj.substring(1));
      if (c === "@") return new Nonterm(obj.substring(1));
    }
    if (obj.type === "Nonterm") return new Nonterm(obj.name);
    if (obj.type === "Token") return new Token(obj.name, obj.value);
    if (obj.type === "EOF") return EOF;
    if (obj.type === "EPSILON") return EPSILON;
    if (obj.type === "HASH") return HASH;
    return null;
  }
  function Nonterm(name) {
    this.name = name;
    this.key = "@" + this.name;
    this.asString = this.name;
  }
  Nonterm.prototype = Object.create(Atom.prototype);
  Nonterm.prototype.toString = function () { /*throw new Error("Don't call me!");*/ return this.name; }
  Nonterm.prototype.toSerializable = function () { return this.key; }
  function Token(name, value) {
    this.name = name;
    if (value !== undefined) {
      this.value = value;
      this.key = "'" + this.name + ":" + this.value;
      this.asString = "'" + this.name;
    } else {
      this.value = name;
      this.key = "'" + this.name;
      this.asString = this.key;
    }
  }
  Token.prototype = Object.create(Atom.prototype);
  Token.prototype.toRepr = function (showVal) {
    if (showVal && this.name !== this.value)
      return "('" + this.name + " " + JSON.stringify(this.value) + ")";
    else
      return this.asString;
  }
  Token.prototype.toString = function () { /*throw new Error("Don't call me!");*/ return this.asString; }
  Token.prototype.toSerializable = function () {
    if (this.name !== this.value)
      return { type: "Token", name: this.name, value: this.value };
    else
      return "'" + this.name;
  }

  const EOF = Object.create(Token.prototype,
    {
      name: { enumerable: true, value: "EOF" },
      asString: { enumerable: true, value: "$" },
      toString: { value: function () { /*throw new Error("Don't call me!");*/ return "$"; } },
      toRepr: {
        value: function (showVal) {
          if (showVal) return "<end of file>";
          return "$";
        }
      },
      key: { enumerable: true, value: "$" },
      toSerializable: { value: function () { return "EOF"; } }
    });
  const EPSILON = Object.create(Atom.prototype,
    {
      name: { enumerable: true, value: "EPSILON" },
      asString: { enumerable: true, value: "ε" },
      toString: { value: function () { /*throw new Error("Don't call me!");*/ return this.key; } },
      toRepr: { value: function () { return "ε"; } },
      key: { enumerable: true, value: "ε" },
      toSerializable: { value: function () { return "EPSILON"; } }
    });
  const HASH = Object.create(Atom.prototype,
    {
      name: { enumerable: true, value: "HASH" },
      asString: { enumerable: true, value: "#" },
      toString: { value: function () { /*throw new Error("Don't call me!");*/ return "#"; } },
      toRepr: { value: function () { return "#"; } },
      key: { enumerable: true, value: "#" },
      toSerializable: { value: function () { return "HASH"; } }
    })

  var Tokenizer = (function () {
    function Tokenizer(ignore_ws, Tokens) {
      this.ignore_ws = ignore_ws;
      this.Tokens = Tokens;
    }
    Tokenizer.prototype.isEOF = function (tok) { return tok === EOF; }
    Tokenizer.prototype.tokenizeFrom = function (str) {
      this.str = str;
      this.curLine = 1;
      this.curCol = 0;
      this.pos = 0;
      this.len = str.length;
      delete this.curTok;
      delete EOF.pos;
      SrcLoc.reset();
      this.times = {};
    }
    const STICKY_REGEXP = ''; //(function() { try { new RegExp("", 'y'); return 'y' } catch(e) { return ''; }})();

    Tokenizer.prototype.hasNext = function () { return this.pos <= this.len; }
    Tokenizer.prototype.isEmpty = function () { return this.length == 0; }
    if (STICKY_REGEXP === 'y') {
      Tokenizer.prototype.positionTokenRegexp = function (tok) { tok.val.lastIndex = this.pos; };
      Tokenizer.prototype.updateString = function (match) { };
      Tokenizer.prototype.updatePosition = function (tok, match) { this.pos = tok.val.lastIndex; };
    } else {
      Tokenizer.prototype.positionTokenRegexp = function (tok) { };
      Tokenizer.prototype.updateString = function (match) { this.str = this.str.slice(match[0].length); };
      Tokenizer.prototype.updatePosition = function (tok, match) { this.pos += match[0].length; };
    }
    Tokenizer.prototype.makeToken = function (tok_type, s, pos) {
      var t = new Token(tok_type, s);
      t.pos = pos;
      return t;
    }
    Tokenizer.prototype.postProcessMatch = function (tok, match, str) { return tok.name; }
    Tokenizer.prototype.unshiftToken = function (prev_tok) { this.curTok = prev_tok; }
    Tokenizer.prototype.next = function () {
      delete this.skippedWhitespace;
      startTimer();
      while (this.hasNext()) { // Surround the tokenizer loop...
        if (this.pos == this.len) {
          this.pos++;
          EOF.pos = SrcLoc.make(this.curLine, this.curCol, this.pos, this.curLine, this.curCol, this.pos);
          this.curTok = EOF;
          return;
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
            return {
              value: t.value,
              type: t.name,
              pos: t.pos,
              toString: t.toRepr,
              name: t.name,
            };
          }
        }
      }
    }

    Tokenizer.prototype.save = function () {
      return {
        ignore_ws: this.ignore_ws,
        Tokens: this.Tokens,
        str: this.str,
        curLine: this.curLine,
        curCol: this.curCol,
        pos: this.pos,
        len: this.len,
        curTok: this.curTok,
        times: this.times,
      };
    }

    Tokenizer.prototype.reset = function (data, info) {
      if (!info) {
        this.tokenizeFrom(data);
        return;
      }
      this.ignore_ws = info.ignore_ws;
      this.Tokens = info.Tokens;
      this.str = info.str;
      this.curLine = info.curLine;
      this.curCol = info.curCol;
      this.pos = info.pos;
      this.len = info.len;
      this.curTok = info.curTok;
      this.times = info.times;
    }

    Tokenizer.prototype.formatError = function (token) {
      console.log("error with token:\n" + JSON.stringify(token, null, 2));
    }

    Tokenizer.prototype.has = function (name) {
      return true;
    }

    return Tokenizer;
  })();

  var Tokenizer2 = (function () {
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
        this.keywords[first].sort(function (a, b) {
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
    Tokenizer.prototype.tokenizeFrom = function (str) {
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
      delete EOF.pos;
      SrcLoc.reset();
      this.times = {};
    }
    Tokenizer.prototype.isEOF = function (tok) { return tok === EOF; }
    Tokenizer.prototype.isEmpty = function () { return this.length == 0; }
    Tokenizer.prototype.hasNext = function () { return this.pos <= this.len; }
    Tokenizer.prototype.unshiftToken = function (prev_tok) { this.curTok = prev_tok; }
    Tokenizer.prototype.addWhitespace = function (ws_loc) {
      if (this.skippedWhitespace === undefined)
        this.skippedWhitespace = ws_loc;
      else
        this.skippedWhitespace = this.skippedWhitespace.combine(ws_loc);
    }
    Tokenizer.prototype.next = function () {
      delete this.skippedWhitespace;
      this.curTok = undefined;
      var startT = startTimer();
      while (this.curTok === undefined && this.hasNext()) {
        if (this.pos == this.len) {
          this.pos++;
          this.curCol++;
          EOF.pos = SrcLoc.make(this.curLine, this.curCol, this.pos, this.curLine, this.curCol, this.pos);
          this.curTok = EOF;
          return EOF;
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
    'SrcLoc': SrcLoc,
    'Tokenizer': Tokenizer,
    'Tokenizer2': Tokenizer2,
    'Token': Token,
    'EOF': EOF,
    'STICKY_REGEXP': '',
  };
});