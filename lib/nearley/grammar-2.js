(function () {
function id(x) { return x[0]; }

const R = require('requirejs');

R.config({paths: {'jglr': '../jglr', 'nearley': '.'}});

const T = R('nearley/nearley-tokenizer');
const tokenizer = T.Tokenizer;
const E = R('jglr/jglr');
const SrcLoc = E.SrcLoc;

const toString = function(show_pos) {
  var toStr = "(" + this.name;
  if (show_pos)
    toStr += "@" + this.pos.toString(true);
  for (var i = 0; i < this.kids.length; i++) {
    if (this.kids[i].toRepr)
      toStr += " " + this.kids[i].toRepr(true);
    else if (this.kids[i].asString)
      toStr += " " + this.kids[i].asString
    else
      toStr += " " + this.kids[i].toString(true);
  }
  toStr += ")";
  return toStr;
};

const defaultAction = function (name) {
  const ret = function (kids) {
    var useful_kids = [];
    for (var i = 0; i < kids.length; i++) {
      if (kids[i].shouldInline === true) useful_kids = useful_kids.concat(kids[i].kids);
      else useful_kids.push(kids[i]);
    }
    if (kids.length > 0) {
      const pos = kids[0].pos.combine(kids[kids.length  - 1].pos);
      return { name, kids: useful_kids, toString, pos };
    } else if (tokenizer.pos !== undefined && tokenizer.curCol !== undefined && tokenizer.curLine !== undefined) {
      const pos = SrcLoc.make(tokenizer.curLine, tokenizer.curCol, tokenizer.pos, tokenizer.curLine, tokenizer.curCol, tokenizer.pos);
      return { name, kids: useful_kids, toString, pos };
    } else {
      const pos = SrcLoc.make(1, 0, 0, 1, 0, 0);
      return { name, kids: useful_kids, toString, pos };
    }
  }
  return ret;
};

const inlineAction = function (name) {
  const ret = function (kids) {
    const defaultRet = defaultAction(name)(kids);
    defaultRet.shouldInline = true;
    return defaultRet;
  }
  return ret;
};

const listSnocAction = function(tail, head, shouldInline) {
  const ret = function (kids) {
    const name = tail;
    var useful_kids = [];
    for (var i = 0; i < kids.length; i++) {
      if (kids[i].name === head) {
        if (kids[i].shouldInline === true)
          useful_kids = useful_kids.concat(kids[i].kids);
        else
          useful_kids.push(kids[i]);
      } else if (kids[i].name === tail) useful_kids = useful_kids.concat(kids[i].kids); 
    }
    if (kids.length > 0) {
      const pos = kids[0].pos.combine(kids[kids.length  - 1].pos);
      return { name, kids: useful_kids, toString, pos, shouldInline };
    } else if (tokenizer.pos !== undefined && tokenizer.curCol !== undefined && tokenizer.curLine !== undefined) {
      const pos = SrcLoc.make(tokenizer.curLine, tokenizer.curCol, tokenizer.pos, tokenizer.curLine, tokenizer.curCol, tokenizer.pos);
      return { name, kids: useful_kids, toString, pos, shouldInline };
    } else {
      const pos = SrcLoc.make(1, 0, 0, 1, 0, 0);
      return { name, kids: useful_kids, toString, pos, shouldInline };
    }
  }
  return ret;
}

var grammar = {
	Lexer: tokenizer,
  ParserRules: [
  {"name": "num", "symbols": ["num_I0*"], "postprocess": defaultAction("num")},
  {"name": "num_I0*", "symbols": [], "postprocess": inlineAction("num_I0*")},
  {"name": "num_I0*", "symbols": ["num_I0*", "num_I0"], "postprocess": listSnocAction("num_I0*", "num_I0", true)},
  {"name": "num_I0", "symbols": [(tokenizer.has("NUMBER") ? {type: "NUMBER"} : "NUMBER")], "postprocess": inlineAction("num_I0")},
  ],
  ParserStart: "num"
}
if (typeof module !== 'undefined'&& typeof module.exports !== 'undefined') {
	module.exports = grammar;
} else {
	window.grammar = grammar;
}
})();
