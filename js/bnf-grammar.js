const E = require('./elkhound.js');
const Grammar = E.Grammar
const Nonterm = E.Nonterm
const Lit = E.Lit
const Token = E.Token
const OrderedSet = E.OrderedSet

function Tokenizer(str, ignore_ws) {
  this.str = str;
  this.curLine = 1;
  this.curCol = 0;
  this.ignore_ws = ignore_ws;
  this.pos = 0;
  this.len = str.length;
}

const STICKY_REGEXP = (function() { try { new RegExp("", 'y'); return 'y' } catch(e) { return ''; }})();

const ws = new RegExp("^\\s+", STICKY_REGEXP);
const comment = new RegExp("^#.*(?:\\n|\\r|\\r\\n|\\n\\r)", STICKY_REGEXP)
const colon = new RegExp("^:", STICKY_REGEXP);
const bar = new RegExp("^\\|", STICKY_REGEXP);
const lbrack = new RegExp("^\\[", STICKY_REGEXP);
const rbrack = new RegExp("^\\]", STICKY_REGEXP);
const lparen = new RegExp("^\\(", STICKY_REGEXP);
const rparen = new RegExp("^\\)", STICKY_REGEXP);
const star = new RegExp("^\\*", STICKY_REGEXP);
const name = new RegExp("^[A-Za-z_0-9-]+", STICKY_REGEXP);
const dquot_str = 
  new RegExp("^\"(?:" +
             "\\\\[01234567]{1,3}" +
             "|\\\\x[0-9a-fA-F]{1,2}" + 
             "|\\\\u[0-9a-fA-f]{1,4}" + 
             "|\\\\[\r\n]{1,2}" + 
             "|\\\\[\\\\nrt\"']" + 
             "|[^\\\"])+\"", STICKY_REGEXP);
const squot_str = 
  new RegExp("^\"(?:" +
             "\\\\[01234567]{1,3}" +
             "|\\\\x[0-9a-fA-F]{1,2}" + 
             "|\\\\u[0-9a-fA-f]{1,4}" + 
             "|\\\\[\r\n]{1,2}" + 
             "|\\\\[\\\\nrt\"']" + 
             "|[^'])+\"", STICKY_REGEXP);
const Tokens = {
  WS: ws, 
  COMMENT: comment, 
  COLON: colon, 
  BAR: bar, 
  LBRACK: lbrack, 
  RBRACK: rbrack, 
  LPAREN: lparen, 
  RPAREN: rparen, 
  STAR: star, 
  NAME: name, 
  DQUOT_STR: dquot_str, 
  SQUOT_STR: squot_str 
};

function countChar(haystack, needle) {
  var count = -1; // Because the body of the loop will run at least once
  // Start at -2 so that we can check the very first character
  for (var i = -2; i !== -1; i = haystack.indexOf(needle, i + 1)) {
    count++;
  }
  return count;
}


function SrcLoc(startRow, startCol, startChar, endRow, endCol, endChar) {
  this.startRow = startRow;
  this.startCol = startCol;
  this.startChar = startChar;
  this.endRow = endRow;
  this.endCol = endCol;
  this.endChar = endChar;
}
SrcLoc.prototype.toString = function(show_span) {
  var ret = this.startRow + ":" + this.startCol;
  if (show_span)
    ret += "-" + this.endRow + ":" + this.endCol;
  return ret;
}

Tokenizer.prototype.hasNext = function() { return this.pos <= this.len; }
if (STICKY_REGEXP === 'y') {
  Tokenizer.prototype.next = function() {
    while (this.hasNext()) { // Surround the tokenizer loop...
      if (this.pos == this.len) { this.pos++; return E.EOF; }
      for (var tok_type in Tokens) {
        var tok = Tokens[tok_type];
        if (!tok instanceof RegExp) continue;
        tok.lastIndex = this.pos;
        var match = tok.exec(this.str);
        if (match !== null) {
          var p = this.pos;
          var l = this.curLine;
          var c = this.curCol;
          var s = match[0];
          var lines = countChar(s, "\n");
          this.pos = tok.lastIndex;
          this.curLine += lines;
          if (lines === 0)
            this.curCol += s.length;
          else
            this.curCol = s.length - s.lastIndexOf("\n");
          if (this.ignore_ws && (tok_type === "WS" || tok_type === "COMMENT")) {
            // ... in case we're ignoring whitespace
            break;
          }
          var t = new E.Token(tok_type, match[0]);
          t.pos = new SrcLoc(l, c, p, this.curLine, this.curCol, this.pos);
          return t;
        }
      }
    }
  }
} else {
  Tokenizer.prototype.next = function() {
    while (this.hasNext()) { // Surround the tokenizer loop...
      if (this.pos == this.len) { this.pos++; return E.EOF; }
      for (var tok_type in Tokens) {
        var tok = Tokens[tok_type];
        if (!tok instanceof RegExp) continue;
        var match = tok.exec(this.str);
        if (match !== null) {
          this.str = this.str.slice(match[0].length);
          var p = this.pos;
          var l = this.curLine;
          var c = this.curCol;
          var s = match[0];
          var lines = countChar(s, "\n");
          this.pos += s.length;
          this.curLine += lines;
          if (lines === 0)
            this.curCol += s.length;
          else
            this.curCol = s.length - s.lastIndexOf("\n");
          if (this.ignore_ws && (tok_type === "WS" || tok_type === "COMMENT")) {
            // ... in case we're ignoring whitespace
            break;
          }
          var t = new E.Token(tok_type, match[0]);
          t.pos = new SrcLoc(l, c, p, this.curLine, this.curCol, this.pos);
          return t;
        }
      }
    }
  }
}

function ListCons(hd, tl, shouldInline) {
  return function(kids) {
    var useful_kids = [];
    for (var i = 0; i < kids.length; i++) {
      if (kids[i].name === hd) {
        if (kids[i].shouldInline === true)
          useful_kids = useful_kids.concat(kids[i].kids);
        else
          useful_kids.push(kids[i]);
      } else if (kids[i].name === tl) useful_kids = useful_kids.concat(kids[i].kids); 
    }
    var start = (kids.length > 0 ? kids[kids.length - 1].startColumn : undefined);
    return { name: tl, kids: useful_kids, toString: E.Rule.defaultASTToString, startColumn: start,
             shouldInline: shouldInline };
  }
}

function Inline(kids) {
  var ret = E.Rule.defaultAction.call(this, kids);
  ret.shouldInline = true;
  return ret;
}

var g = new Grammar("BNF", "G");
g.addRule("G", [new Nonterm("Rule"), new Nonterm("G")], ListCons("Rule", "G"));
g.addRule("G", [new Nonterm("Rule")]);
g.addRule("Rule", [new Token("NAME"), new Lit(":"), new Nonterm("Alts")]);
g.addRule("Alts", [new Nonterm("Items"), new Lit("|"), new Nonterm("Alts")], ListCons("Items", "Alts"));
g.addRule("Alts", [new Nonterm("Items")]);
g.addRule("Items", [new Nonterm("Item"), new Nonterm("Items")], ListCons("Item", "Items"));
g.addRule("Items", []);
g.addRule("Item", [new Nonterm("Name")], Inline);
g.addRule("Item", [new Nonterm("Str")], Inline);
g.addRule("Item", [new Nonterm("Opt")], Inline);
g.addRule("Item", [new Nonterm("Star")], Inline);
g.addRule("Item", [new Nonterm("Paren")], Inline);
g.addRule("Name", [new Token("NAME")]);
g.addRule("Str", [new Token("DQUOT_STR")]);
g.addRule("Str", [new Token("SQUOT_STR")]);
g.addRule("Opt", [new Lit("["), new Nonterm("Alts"), new Lit("]")]);
g.addRule("Paren", [new Lit("("), new Nonterm("Alts"), new Lit(")")], Inline);
g.addRule("Star", [new Nonterm("Item"), new Lit("*")]);
g.initializeParser();





function generateGrammar(bnf, name) {
  var ret = [];
  var firstRule = undefined;
  for (var i = 0; i < bnf.kids.length; i++) {
    if (bnf.kids[i].name === "Rule") {
      firstRule = bnf.kids[i].kids[0].value;
      break;
    }
  }
  ret.push("var g = new Grammar(" + JSON.stringify(name) + ", " + JSON.stringify(firstRule) + ");");
  for (var i = 0; i < bnf.kids.length; i++) {
    var rule = generateRule(bnf.kids[i]);
    for (var j = 0; j < rule.rules.length; j++)
      ret.push(rule.rules[j]);
  }
  return ret;
}

function generateRule(rule) {
  var ruleName = rule.kids[0].value;
  var ret = {rules: []}
  var gen = generateAlts(ruleName, rule.kids[1], false);
  ret.rules = ret.rules.concat(gen.rules);
  return ret;
}

function generateAlts(ruleName, alts, shouldInline) {
  var ret = {rules: []}
  if (alts.kids.length === 1) {
    var gen = generateAlt(ruleName, alts.kids[0], true);
    if (shouldInline)
        ret.rules.push("g.addRule(" + JSON.stringify(ruleName) + ", [" + gen.rhs + "], E.Rule.Inline)");
      else
        ret.rules.push("g.addRule(" + JSON.stringify(ruleName) + ", [" + gen.rhs + "])");
    ret.rules = ret.rules.concat(gen.rules);
  } else {
    for (var i = 0; i < alts.kids.length; i++) {
      var gen = generateAlt(ruleName +"_A"+ i, alts.kids[i], true);
      if (shouldInline)
        ret.rules.push("g.addRule(" + JSON.stringify(ruleName) + ", [" + gen.rhs + "], E.Rule.Inline)");
      else
        ret.rules.push("g.addRule(" + JSON.stringify(ruleName) + ", [" + gen.rhs + "])");
      ret.rules = ret.rules.concat(gen.rules);
    }
  }
  return ret;
}
function generateAlt(ruleName, alt, shouldInline) {
  var ret = {rules: []}
  var gen = generateItems(ruleName, alt.kids, shouldInline);
  if (shouldInline) 
    ret.rhs = gen.rhs;
  else
    ret.rules.push("g.addRule(" + JSON.stringify(ruleName) + ", [" + gen.rhs + "])");
  ret.rules = ret.rules.concat(gen.rules);
  return ret;
}
function generateItems(ruleName, items) {
  var ret = {rules: []}
  var genRhs = [];
  for (var i = 0; i < items.length; i++) {
    var gen = generateItem(ruleName +"_I"+ i, items[i]);
    ret.rules = ret.rules.concat(gen.rules);
    genRhs.push(gen.rhs);
  }
  ret.rhs = genRhs.join(", ");
  return ret;
}

function generateItem(ruleName, item) {
  if (item.name === "Name") {
    if (item.kids[0].value.toUpperCase() === item.kids[0].value)
      return {rhs: "new Token(" + JSON.stringify(item.kids[0].value) + ")", rules: []};
    else
      return {rhs: "new Nonterm(" + JSON.stringify(item.kids[0].value) + ")", rules: []};
  } else if (item.name === "Str") {
    return {rhs: "new Lit(" + item.kids[0].value + ")", rules: []};
  } else if (item.name === "Star") {
    var ret = {rules: []};
    var newNameOne = ruleName;
    var newNameStar = newNameOne + "_star";
    var json_newNameOne = JSON.stringify(newNameOne);
    var json_newNameStar = JSON.stringify(newNameStar);
    ret.rules.push("g.addRule(" + json_newNameStar + ", [], E.Rule.Inline);");
    ret.rules.push("g.addRule(" + json_newNameStar + ", " +
                   "[new Nonterm(" + json_newNameOne + ")" + ", new Nonterm(" + json_newNameStar + ")], " +
                   "E.Rule.ListCons(" + json_newNameOne + ", " + json_newNameStar + ", true));");
    if ((item.kids.length === 1) && item.kids[0].name === "Items") {
      var gen = generateItems(newNameOne, item.kids[0]);
      ret.rules = ret.rules.concat(gen.rules);
    } else if (item.kids.length === 1) {
      var gen = generateItem(newNameOne, item.kids[0]);
      ret.rules.push("g.addRule(" + json_newNameOne + ", [" + gen.rhs + "], E.Rule.Inline);");
    } else {
      console.log("Got a star item with multiple direct children? " + JSON.stringify(item));
    }
    ret.rhs = "new Nonterm(" + json_newNameStar + ")";
    return ret;
  } else if (item.name === "Opt") {
    var ret = {rules: []};
    var newNameOne = ruleName;
    var newNameOpt = newNameOne + "_opt";
    var json_newNameOne = JSON.stringify(newNameOne);
    var json_newNameOpt = JSON.stringify(newNameOpt);
    ret.rules.push("g.addRule(" + json_newNameOpt + ", [], E.Rule.Inline);");
    ret.rules.push("g.addRule(" + json_newNameOpt + ", [new Nonterm(" + json_newNameOne + ")], E.Rule.Inline);");
    var gen = generateAlts(newNameOne, item.kids[0], true);
    ret.rules = ret.rules.concat(gen.rules);
    ret.rhs = "new Nonterm(" + json_newNameOpt + ")";
    return ret;
  } else if (item.name === "Alts") {
    var ret = generateAlts(ruleName, item, true);
    ret.rhs = "new Nonterm(" + JSON.stringify(ruleName) + ")";
    return ret;
  } else {
    console.log("Unknown item " + JSON.stringify(item));
    return "UNKNOWN";
  }
}


const fs = require("fs");
var data = fs.readFileSync("../src/lang/grammar.rkt", "utf8");
// var data = "app-args: PARENNOSPACE [app-arg-elt* binop-expr] \")\"\n" +
// "app-arg-elt: binop-expr \",\""

var toks = new Tokenizer(data, true);
var parsed = g.parse(toks);
if (parsed !== null) {
  //console.log(parsed.toString(true));
  var grammar_name = "grammar";
  var bnfJS = generateGrammar(parsed, grammar_name);
  var out = fs.createWriteStream("grammar.js");
  out.write("const fs = require('fs');\n");
  out.write("const E = require('./elkhound.js');\nconst Grammar = E.Grammar\nconst Nonterm = E.Nonterm\n");
  out.write("const Lit = E.Lit\nconst Token = E.Token\nconst OrderedSet = E.OrderedSet\nconst Rule = E.Rule\n\n");
  out.write(bnfJS.join("\n"));
  out.write("\n\n");
  out.write("g.initializeParser(true);\n")
  out.write("var ambiguities = g.checkForLALRAmbiguity();\n");
  out.write("if (ambiguities.length == 0) {\n");
  out.write("  console.log(\"Unambiguous grammar!\");\n");
  out.write("} else {\n");
  out.write("  for (var i = 0; i < ambiguities.length; i++)\n");
  out.write("    console.log(ambiguities[i]);\n");
  out.write("}\n");
  out.write("var g_json = JSON.stringify(g.toSerializable(), null, '  ');\n");
  out.write("var out = fs.createWriteStream('pyret-parser.js');\n");
  out.write("out.write(\"const E = require('./elkhound.js');\\nconst Grammar = E.Grammar\\nconst Nonterm = E.Nonterm\\n\");\n");
  out.write("out.write(\"const Lit = E.Lit\\nconst Token = E.Token\\nconst OrderedSet = E.OrderedSet\\nconst Rule = E.Rule\\n\\n\");\n");
  out.write("out.write(\"var g_json = \" + g_json + \";\\n\");\n");
  out.write("out.write(\"exports.PyretGrammar = Grammar.fromSerializable(g_json);\\n\");\n");
  out.write("out.end();\n");
  out.end();
}


