(function () {
function id(x) { return x[0]; }

const R = require('requirejs');

R.config({paths: {'jglr': '../jglr', 'nearley': '.'}});

const T = R('nearley/nearley-tokenizer');
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
  const ret = function (kids) {
    var useful_kids = [];
    for (var i = 0; i < kids.length; i++) {
      if (kids[i].shouldInline === true) useful_kids = useful_kids.concat(kids[i].kids);
      else useful_kids.push(kids[i]);
    }
    var pos = (kids.length > 0 ? kids[0].pos.combine(kids[kids.length  - 1].pos) : "NEED TO FIGURE OUT WHAT TO DO HERE");
    return { name, kids: useful_kids, toString, pos: pos };
  }
  return ret;
};

var grammar = {
	Lexer: tokenizer,
  ParserRules: [
  {"name": "p", "symbols": ["s"], "postprocess": defaultAction("p")},
  {"name": "s", "symbols": ["s", (tokenizer.has("PLUS") ? {type: "PLUS"} : "PLUS"), "m"], "postprocess": defaultAction("s")},
  {"name": "s", "symbols": ["m"], "postprocess": defaultAction("s")},
  {"name": "m", "symbols": ["m", (tokenizer.has("TIMES") ? {type: "TIMES"} : "TIMES"), "t"], "postprocess": defaultAction("m")},
  {"name": "m", "symbols": ["t"], "postprocess": defaultAction("m")},
  {"name": "t", "symbols": [(tokenizer.has("NUMBER") ? {type: "NUMBER"} : "NUMBER")], "postprocess": defaultAction("t")},
  ],
  ParserStart: "p"
}
if (typeof module !== 'undefined'&& typeof module.exports !== 'undefined') {
	module.exports = grammar;
} else {
	window.grammar = grammar;
}
})();
