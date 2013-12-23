const E = require('./rnglr.js');
const Grammar = E.Grammar
const Nonterm = E.Nonterm
const Token = E.Token
const OrderedSet = E.OrderedSet
const SrcLoc = E.SrcLoc

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

Tokenizer.prototype.hasNext = function() { return this.pos <= this.len; }
Tokenizer.prototype.isEmpty = function() { return this.length == 0; }
if (STICKY_REGEXP === 'y') {
  Tokenizer.prototype.next = function() {
    while (this.hasNext()) { // Surround the tokenizer loop...
      if (this.pos == this.len) { 
        this.pos++; 
        E.EOF.pos = SrcLoc.make(this.curLine, this.curCol, this.pos, this.curLine, this.curCol, this.pos);
        return E.EOF; 
      }
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
          t.pos = SrcLoc.make(l, c, p, this.curLine, this.curCol, this.pos);
          return t;
        }
      }
    }
  }
} else {
  Tokenizer.prototype.next = function() {
    while (this.hasNext()) { // Surround the tokenizer loop...
      if (this.pos == this.len) { 
        this.pos++; 
        E.EOF.pos = SrcLoc.make(this.curLine, this.curCol, this.pos, this.curLine, this.curCol, this.pos);
        return E.EOF; 
      }
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
          t.pos = SrcLoc.make(l, c, p, this.curLine, this.curCol, this.pos);
          return t;
        }
      }
    }
  }
}

const Inline = E.Rule.Inline;
const ListCons = E.Rule.ListCons;
const KeepOnly = E.Rule.KeepOnly;

var g = new Grammar("BNF", "G");
g.addRule("G", [new Nonterm("Rule"), new Nonterm("G")], ListCons("Rule", "G"));
g.addRule("G", [new Nonterm("Rule")]);
g.addRule("Rule", [new Token("NAME"), new Token("COLON"), new Nonterm("Alts")]);
g.addRule("Alts", [new Nonterm("Items"), new Token("BAR"), new Nonterm("Alts")], ListCons("Items", "Alts"));
g.addRule("Alts", [new Nonterm("Items")]);
g.addRule("Items", [new Nonterm("Item"), new Nonterm("Items")], ListCons("Item", "Items"));
g.addRule("Items", []);
g.addRule("Item", [new Nonterm("Name")], Inline);
g.addRule("Item", [new Nonterm("Opt")], Inline);
g.addRule("Item", [new Nonterm("Star")], Inline);
g.addRule("Item", [new Nonterm("Paren")], Inline);
g.addRule("Name", [new Token("NAME")]);
g.addRule("Opt", [new Token("LBRACK"), new Nonterm("Alts"), new Token("RBRACK")], KeepOnly(["Alts"]));
g.addRule("Paren", [new Token("LPAREN"), new Nonterm("Alts"), new Token("RPAREN")], KeepOnly(["Alts"], true));
g.addRule("Star", [new Nonterm("Item"), new Token("STAR")], KeepOnly(["Item"]));
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
  var gen = generateAlts(ruleName, rule.kids[2], false);
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
    if (item.kids[0].name === "Alts") {
      var gen = generateAlts(newNameOne, item.kids[0], true);
      ret.rules = ret.rules.concat(gen.rules);
    } else {
      var gen = generateItems(newNameOne, [item.kids[0]]);
      ret.rules = ret.rules.concat(gen.rules);
      ret.rules.push("g.addRule(" + JSON.stringify(newNameOne) + ", [" + gen.rhs + "], E.Rule.Inline)");
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
var data = fs.readFileSync("grammar-full.rkt", "utf8");
//var data = fs.readFileSync("grammar-small.rkt", "utf8");

var toks = new Tokenizer(data, true);
var parsed = g.parse(toks);
if (parsed !== undefined) {
  //console.log(parsed.toString(true));
  var grammar_name = "grammar";
  var parses = g.constructAllParses(parsed, "");
  console.log("Found " + parses.length + " parses");
  // console.log(parses[0].toString());
  var bnfJS = generateGrammar(parses[0], grammar_name);
  var out = fs.createWriteStream("grammar.js");
  out.write("const fs = require('fs');\n");
  out.write("const E = require('./rnglr.js');\nconst Grammar = E.Grammar\nconst Nonterm = E.Nonterm\n");
  out.write("const Token = E.Token\nconst OrderedSet = E.OrderedSet\nconst Rule = E.Rule\n\n");
  out.write(bnfJS.join("\n"));
  out.write("\n\n");
  out.write("g.initializeParser(true);\n")
  out.write("var cycles = g.checkForCycles();\n");
  out.write("if (cycles) {\n");
  out.write("  console.log(\"Non-cyclic grammar!\");\n");
  out.write("} else {\n");
  out.write("  for (var i = 0; i < cycles.length; i++)\n");
  out.write("    console.log(cycles[i]);\n");
  out.write("}\n");
  out.write("var g_json = JSON.stringify(g.toSerializable(), null, '  ');\n");
  out.write("var out = fs.createWriteStream('pyret-parser.js');\n");
  out.write("out.write(\"const E = require('./rnglr.js');\\nconst Grammar = E.Grammar\\nconst Nonterm = E.Nonterm\\n\");\n");
  out.write("out.write(\"const Token = E.Token\\nconst OrderedSet = E.OrderedSet\\nconst Rule = E.Rule\\n\\n\");\n");
  out.write("out.write(\"var g_json = \" + g_json + \";\\n\");\n");
  out.write("out.write(\"exports.PyretGrammar = Grammar.fromSerializable(g_json);\\n\");\n");
  out.write("out.end();\n");
  out.write("\n\n");
  out.write("const T = require('./pyret-tokenizer.js');\n");
  out.write("var data = \"#lang pyret\\n\\nimport \\\"foo\\\" as bar\\na\";\n");
  out.write("const toks = new T.Tokenizer(data, true);\n");
  out.write("var parsed = g.parse(toks);\n");
  out.write("console.log(g.printSPPFasDot());\n");
  out.write("console.log(g.printGSSasDot());\n");
  out.write("console.log(\"Result:\");\n");
  out.write("console.log(g.constructAllParses(parsed)[0].toString(true));\n");
  out.end();
}




