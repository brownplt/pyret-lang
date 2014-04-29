define(["./rnglr"], function(E) {
  const SrcLoc = E.SrcLoc

  function Tokenizer(ignore_ws, Tokens) {
    this.ignore_ws = ignore_ws;
    this.Tokens = Tokens;
  }
  Tokenizer.prototype.tokenizeFrom = function(str) {
    this.str = str;
    this.curLine = 1;
    this.curCol = 0;
    this.pos = 0;
    this.len = str.length;
    delete this.curTok;
    delete E.EOF.pos;
    SrcLoc.reset();
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
  Tokenizer.prototype.postProcessMatch = function(tok, match) { return tok.name; }
  Tokenizer.prototype.next = function() {
    delete this.skippedWhitespace;
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
          var tok_type = this.postProcessMatch(tok, match);
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
            this.skippedWhitespace = SrcLoc.make(l, c, p, this.curLine, this.curCol, this.pos);
            break;
          }
          var t = this.makeToken(tok_type, s, SrcLoc.make(l, c, p, this.curLine, this.curCol, this.pos));
          this.curTok = t;
          return t;
        }
      }
    }
  }

  return {
    'Tokenizer': Tokenizer,
    'STICKY_REGEXP': STICKY_REGEXP,
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
