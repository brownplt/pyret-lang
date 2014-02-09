const R = require('requirejs');

R.config({
  baseUrl: "./"
});

R(['lib/jglr/tokenizer', 'lib/jglr/jglr', 'fs'], function(T, E, fs) {
  const Grammar = E.Grammar
  const Nonterm = E.Nonterm
  const Token = E.Token
  const SrcLoc = E.SrcLoc
  const Tokenizer = T.Tokenizer;
  const STICKY_REGEXP = T.STICKY_REGEXP;

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
  const Tokens = [
    {name: "WS", val: ws},
    {name: "COMMENT", val: comment},
    {name: "COLON", val: colon},
    {name: "BAR", val: bar},
    {name: "LBRACK", val: lbrack},
    {name: "RBRACK", val: rbrack},
    {name: "LPAREN", val: lparen},
    {name: "RPAREN", val: rparen},
    {name: "STAR", val: star},
    {name: "NAME", val: name},
  ];

  const toks = new Tokenizer(true, Tokens);

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


  var data = fs.readFileSync(process.argv[2], "utf8");
  //var data = fs.readFileSync("grammar-small.rkt", "utf8");


  toks.tokenizeFrom(data);
  var parsed = g.parse(toks);
  if (parsed !== undefined) {
    //console.log(parsed.toString(true));
    var grammar_name = "grammar";
    //console.log("g.count: ", g.countAllParses(parsed));
    var parses = g.constructAllParses(parsed, "");
    console.log("Found " + parses.length + " parses");
    // console.log(parses[0].toString());
    var bnfJS = generateGrammar(parses[0], grammar_name);
    var filename = process.argv[3];
    var out = fs.createWriteStream(filename);
    out.write("const R = require('requirejs');\n\n");
    out.write("R.config({ baseUrl: __dirname });\n");
    out.write("console.log(__dirname);\n");
    out.write("R(['fs', '../../../lib/jglr/jglr', './pyret-tokenizer'], function(fs, E, T) {\n");
    out.write("  const Grammar = E.Grammar\n");
    out.write("  const Nonterm = E.Nonterm\n");
    out.write("  const Token = E.Token\n");
    out.write("  const Rule = E.Rule\n\n");
    out.write("  " + bnfJS.join("\n  "));
    out.write("\n\n");
    out.write("  g.initializeParser(true);\n")
    out.write("  var cycles = g.checkForCycles();\n");
    out.write("  if (cycles) {\n");
    out.write("    console.log(\"Non-cyclic grammar!\");\n");
    out.write("  } else {\n");
    out.write("    for (var i = 0; i < cycles.length; i++)\n");
    out.write("      console.log(cycles[i]);\n");
    out.write("  }\n");
    out.write("  var g_json = JSON.stringify(g.toSerializable(), null, '  ');\n");
    out.write("  var filename = process.argv[2];\n");
    out.write("  var out = fs.createWriteStream(filename);\n");

    out.write("  out.write(\"define(['../../../lib/jglr/jglr'],\\n\");\n");
    out.write("  out.write(\"/** @param {{Grammar : {fromSerializable : !Function}, Nonterm : !Object, Token : !Object, Rule : !Object}} E */\\n\");\n");
    out.write("  out.write(\"function(E) {\\n\");\n");
    out.write("  out.write(\"  const Grammar = E.Grammar;\\n\");\n");
    out.write("  out.write(\"  const Nonterm = E.Nonterm;\\n\");\n");
    out.write("  out.write(\"  const Token = E.Token;\\n\");\n");
    out.write("  out.write(\"  const Rule = E.Rule;\\n\\n\");\n");
    out.write("  out.write(\"  var g_json = \" + g_json.replace(/\\n/g, \"\\n  \") + \";\\n\");\n");
    out.write("  out.write(\"  return { PyretGrammar: Grammar.fromSerializable(g_json) };\\n\");\n");
    out.write("  out.write(\"});\\n\");\n");
    out.write("  out.end();\n");
    out.write("});\n");
    out.end();
  } else {
    var next_tok = toks.next();
    var msg = "Error reading grammar at token " + next_tok.toString(true) + " at position " + next_tok.pos.toString(true);
    throw msg;
  }
});
