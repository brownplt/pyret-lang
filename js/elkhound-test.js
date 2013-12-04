const E = require('./elkhound.js');
const Grammar = E.Grammar
const Nonterm = E.Nonterm
const Lit = E.Lit
const Token = E.Token
const OrderedSet = E.OrderedSet

var g = new Grammar("Example", "S");
// g.addRule("E", [new Nonterm("T"), new Nonterm("E'")]);
// g.addRule("E'", [new Lit("+"), new Nonterm("T"), new Nonterm("E'")]);
// g.addRule("E'", []);
// g.addRule("T", [new Nonterm("F"), new Nonterm("T'")]);
// g.addRule("T'", [new Lit("*"), new Nonterm("F"), new Nonterm("T'")]);
// g.addRule("T'", []);
// g.addRule("F", [new Lit("("), new Nonterm("E"), new Lit(")")]);
// g.addRule("F", [new Token("id")]);

// g.addRule("S", [new Nonterm("E")]);
// g.addRule("E", [new Nonterm("E"), new Lit("+"), new Nonterm("T")]);
// g.addRule("E", [new Nonterm("T")]);
// g.addRule("T", [new Nonterm("T"), new Lit("*"), new Nonterm("F")]);
// g.addRule("T", [new Nonterm("F")]);
// g.addRule("F", [new Lit("("), new Nonterm("E"), new Lit(")")]);
// g.addRule("F", [new Token("id")]);

// g.addRule("S'", [new Nonterm("S")]);
// g.addRule("S", [new Nonterm("C"), new Nonterm("C")]);
// g.addRule("C", [new Lit("e"), new Nonterm("C")]);
// g.addRule("C", [new Lit("d")]);

g.addRule("S", [new Nonterm("E"), E.EOF]);
g.addRule("E", [new Token("id")]);
g.addRule("E", [new Nonterm("E"), new Token("*"), new Nonterm("E")]);
g.addRule("E", [new Nonterm("E"), new Token("+"), new Nonterm("E")]);

// g.addRule("S", [new Nonterm("A")])
// g.addRule("A", [new Nonterm("A"), new Lit("+"), new Nonterm("B")]);
// g.addRule("A", [new Lit("a")]);
// g.addRule("B", [new Lit("b")]);

console.log(g.toString());

g.computeFirstSets();
g.computeFollowSets();

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

console.log("First sets:");
console.log(printColl(g.first))

console.log("Follow sets:");
console.log(printColl(g.follow))

// var init_rule = g.rules[g.start][0].withLookahead(E.EOF);
// var init_set = new OrderedSet([init_rule]);
// var lr1set = g.completeClosure(init_set);
// console.log("LR(1) Initial Set:");
// console.log(lr1set.toString(true));

g.computeStates();
console.log("All reachable LR(1) states:")
for (var i = 0; i < g.states.size(); i++)
  console.log(g.states.get(i).toString(true) + "\n")
console.log(g.printTables());
var ambiguities = g.checkForLALRAmbiguity();
if (ambiguities.length == 0) {
  console.log("Unambiguous grammar!");
} else {
  for (var i = 0; i < ambiguities.length; i++)
    console.log(ambiguities[i]);
}

function token_stream(toks) {
  var cur = 0;
  return { 
    hasNext: function() { return cur <= toks.length; },
    next: function() { 
      if (cur < toks.length) {
        var t = toks[cur++]; 
        if (typeof t === "string")
          t = new Lit(t)
        return t;
      } else {
        cur++;
        return E.EOF;
      }
    },
    reset: function() { cur = 0; }
  }
}

function id(x) { return new Token("id", x); }
var tokens = token_stream([id("i"), new Token("+"), id("i"), new Token("*"), id("i")]);
var parsed = g.parseLALR(tokens);
console.log(JSON.stringify(parsed, null, "  "));

tokens.reset();
console.log("\n\n\nTrying GLR parsing now...");
var glr_parsed = g.parseGLR(tokens);
console.log(JSON.stringify(glr_parsed, null, "  "));


// var g_json = JSON.stringify(g.toJSON(), null, "  ")
// var g2 = Grammar.fromJSON(JSON.parse(g_json))

// tokens.reset();
// var reparsed = g2.parseLALR(tokens);
// function deepEqual(obj1, obj2) {
//   if (obj1 === obj2) return true;
//   var propCount1 = 0;
//   for (var name in obj1)
//     if (obj1.hasOwnProperty(name))
//       propCount1++;
//   var propCount2 = 0;
//   for (var name in obj2)
//     if (obj2.hasOwnProperty(name))
//       propCount2++;
//   if (propCount1 !== propCount2) return false;
//   for (var name in obj1)
//     if (obj1.hasOwnProperty(name))
//       if (!deepEqual(obj1[name], obj2[name]))
//         return false;
//   return true;
// }
// console.log("After picking the parse tables and reloading, are parse trees equal? " + deepEqual(parsed, reparsed))

