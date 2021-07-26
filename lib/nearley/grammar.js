(function () {
function id(x) { return x[0]; }

const R = require('requirejs');

R.config({paths: {'jglr': '../jglr', 'nearley': '.'}});

const T = R('nearley/nearley-tokenizer');
const E = R('jglr/jglr');
const SrcLoc = E.SrcLoc;
const tokenizer = T.Tokenizer;

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
  const ret = function (kids, location) {
    console.log(name + " " + location);
    console.log(tokenizer.pos + " " + tokenizer.curCol + " " + tokenizer.curLine);
    var useful_kids = [];
    for (var i = 0; i < kids.length; i++) {
      if (kids[i].shouldInline === true) useful_kids = useful_kids.concat(kids[i].kids);
      else useful_kids.push(kids[i]);
    }
    var pos = (kids.length > 0 ? kids[0].pos.combine(kids[kids.length  - 1].pos) : SrcLoc.make(0, 0, 0, 0, 0, 0));
    return { name, kids: useful_kids, toString, pos: pos };
  }
  return ret;
};

const inlineAction = function (name) {
  const ret = function (kids, location) {
    const defaultRet = defaultAction(name)(kids, location);
    defaultRet.shouldInline = true;
    return defaultRet;
  }
  return ret;
};

var grammar = {
	Lexer: tokenizer,
  ParserRules: [
  {"name": "add", "symbols": ["negate-opt", (tokenizer.has("NUMBER") ? {type: "NUMBER"} : "NUMBER"), (tokenizer.has("PLUS") ? {type: "PLUS"} : "PLUS"), "negate-opt", "num"], "postprocess": defaultAction("add")},
  {"name": "negate-opt", "symbols": ["negate"], "postprocess": defaultAction("negate-opt")},
  {"name": "negate", "symbols": ["negate_I0?"], "postprocess": defaultAction("negate")},
  {"name": "negate_I0?", "symbols": [], "postprocess": inlineAction("negate_I0?")},
  {"name": "negate_I0?", "symbols": ["negate_I0"], "postprocess": inlineAction("negate_I0?")},
  {"name": "negate_I0", "symbols": [(tokenizer.has("DASH") ? {type: "DASH"} : "DASH")], "postprocess": inlineAction("negate_I0")},
  {"name": "num", "symbols": ["num_I0?"], "postprocess": defaultAction("num")},
  {"name": "num_I0?", "symbols": [], "postprocess": inlineAction("num_I0?")},
  {"name": "num_I0?", "symbols": ["num_I0"], "postprocess": inlineAction("num_I0?")},
  {"name": "num_I0", "symbols": [(tokenizer.has("NUMBER") ? {type: "NUMBER"} : "NUMBER")], "postprocess": inlineAction("num_I0")},
  ],
  ParserStart: "add"
}
if (typeof module !== 'undefined'&& typeof module.exports !== 'undefined') {
	module.exports = grammar;
} else {
	window.grammar = grammar;
}
})();
