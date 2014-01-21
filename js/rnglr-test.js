const R = require("requirejs")

R(['os', 'fs', './rnglr'], function(os, fs, E) {
os.EOL = '\n';
const Grammar = E.Grammar
const Nonterm = E.Nonterm
const Token = E.Token
const OrderedSet = E.OrderedSet
const SrcLoc = E.SrcLoc

var g = new Grammar("Example", "S");
// g.addRule("S", [new Token("id"), new Nonterm("B"), new Nonterm("S"), new Nonterm("S")]);
// g.addRule("S", [new Token("a")]);
// g.addRule("S", []);
// g.addRule("B", []);

g.addRule("S", [new Nonterm("S"), new Token("+"), new Nonterm("S")]);
g.addRule("S", [new Token("id")]);

// g.addRule("S", [new Nonterm("A"), new Nonterm("B")]);
// g.addRule("S", [new Nonterm("B"), new Nonterm("A")]);
// g.addRule("A", [new Nonterm("B")]);
// g.addRule("A", []);
// g.addRule("B", [new Nonterm("S")]);
// g.addRule("B", []);

function printColl(col) {
  var s = ""
  for (var name in col) {
    if (s !== "") s += "\n";
    s += "  " + name + " => {";
    var first = true;
    for (var tok in col[name]) {
      if (first) {
        s += col[name][tok].toString()
        first = false;
      } else 
        s += ", " + col[name][tok].toString()
    }
    s += "}";
  }
  return s;
}


g.initializeParser();
// console.log(g.toString());

// console.log("All reachable LR(1) states:")
// for (var i = 0; i < g.states.size(); i++)
//   console.log(g.completeClosure(g.states.get(i)).toString(true) + "\n")

// console.log("Are any grammar productions cyclic? " + g.checkForCycles());

function token_stream(toks) {
  var cur = 0;
  return { 
    hasNext: function() { return cur <= toks.length; },
    isEmpty: function() { return toks.length == 0; },
    next: function() { 
      if (cur < toks.length) {
        var t = toks[cur++]; 
        if (typeof t === "string")
          t = new Token(t)
        t.pos = SrcLoc.make(cur-1,cur-1,cur-1,cur,cur,cur);
        return t;
      } else {
        E.EOF.pos = SrcLoc.make(cur,cur,cur,cur,cur,cur);
        cur++;
        return E.EOF;
      }
    },
    reset: function() { cur = 0; }
  }
}

function id(x) { return new Token("id", x); }
function tok(t) { return new Token(t); }
//var tokens = token_stream([id("b"), id("b"), id("a"), id("a"), id("a"), id("b")]);
//var tokens = token_stream([id("b1"), id("b2"), new Token("a"), new Token("a")]);
var tokens = token_stream([id("a"), tok("+"), id("b"), tok("+"), id("c"), tok("+"), id("d"), tok("+"), id("e"), tok("+"), id("f")]);

// fs.unlink('parser-initial.txt');
// log4js.configure({
//   appenders: [ { type: 'file', filename: 'parser-initial.txt', layout: { type: 'pattern', pattern: "%m" } } ],
//   replaceConsole: true
// });
// console.log(g.printTables());
// var parsed = g.parse(tokens);
// if (parsed !== undefined) {
//   var parses = g.constructAllParses(parsed, "");
//   console.log("Constructed " + parses.length + " parses:")
//   for (var i = 0; i < parses.length; i++)
//     console.log(parses[i].toString());
// }

// tokens.reset();

// fs.unlink('parser-rebuilt.txt');
// log4js.configure({
//   appenders: [ { type: 'file', filename: 'parser-rebuilt.txt', layout: { type: 'pattern', pattern: "%m" } } ],
//   replaceConsole: true
// });
var g2 = Grammar.fromSerializable(JSON.parse(JSON.stringify(g.toSerializable())));
console.log(g2.printTables());
var parsed2 = g2.parse(tokens);
if (parsed2 !== undefined) {
  var parses = g2.constructAllParses(parsed2, "");
  console.log("Constructed " + parses.length + " parses:")
  for (var i = 0; i < parses.length; i++)
    console.log(parses[i].toString());
}
  var countParses = g2.countAllParses(parsed2);
  console.log("There are " + countParses + " potential parses");
});
