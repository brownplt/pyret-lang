const R = require('requirejs');

R.config({
  baseUrl: "./"
});

R(['lib/jglr/jglr', 'fs'], function(E, fs) {
  const Grammar = E.Grammar
  const Nonterm = E.Nonterm
  const Token = E.Token
  const SrcLoc = E.SrcLoc
  const Tokenizer = E.Tokenizer;
  const STICKY_REGEXP = E.STICKY_REGEXP;

  const ws = new RegExp("^\\s+", STICKY_REGEXP);
  const comment = new RegExp("^#.*(?:\\n|\\r|\\r\\n|\\n\\r)", STICKY_REGEXP)
  const colon = new RegExp("^:", STICKY_REGEXP);
  const comma = new RegExp("^,", STICKY_REGEXP);
  const bar = new RegExp("^\\|", STICKY_REGEXP);
  const lbrack = new RegExp("^\\[", STICKY_REGEXP);
  const rbrack = new RegExp("^\\]", STICKY_REGEXP);
  const lbrace = new RegExp("^\\{", STICKY_REGEXP);
  const rbrace = new RegExp("^\\}", STICKY_REGEXP);
  const lparen = new RegExp("^\\(", STICKY_REGEXP);
  const rparen = new RegExp("^\\)", STICKY_REGEXP);
  const star = new RegExp("^\\*", STICKY_REGEXP);
  const plus = new RegExp("^\\+", STICKY_REGEXP);
  const slashstar = new RegExp("^/\\*", STICKY_REGEXP);
  const starslash = new RegExp("^\\*/", STICKY_REGEXP);
  const name = new RegExp("^[A-Za-z_0-9-]+", STICKY_REGEXP);
  const anychar = new RegExp("^[^]", STICKY_REGEXP);
  const Tokens = [
    {name: "WS", val: ws},
    {name: "COMMENT", val: comment},
    {name: "COMMA", val: comma},
    {name: "COLON", val: colon},
    {name: "BAR", val: bar},
    {name: "LBRACK", val: lbrack},
    {name: "RBRACK", val: rbrack},
    {name: "LBRACE", val: lbrace},
    {name: "RBRACE", val: rbrace},
    {name: "LPAREN", val: lparen},
    {name: "RPAREN", val: rparen},
    {name: "SLASHSTAR", val: slashstar},
    {name: "STARSLASH", val: starslash},
    {name: "STAR", val: star},
    {name: "PLUS", val: plus},
    {name: "NAME", val: name},
    {name: "UNKNOWN", val: anychar},
  ];

  const toks = new Tokenizer(true, Tokens);

  const Inline = E.Rule.Inline;
  const ListCons = E.Rule.ListCons;
  const KeepOnly = E.Rule.KeepOnly;

  var g = new Grammar("BNF", "G");
  g.addRule("G", [new Nonterm("OptConfigs"), new Nonterm("Rules")]);
  g.addRule("OptConfigs", [new Token("SLASHSTAR"), new Nonterm("Configs"), new Token("STARSLASH")], 
            KeepOnly(["Configs"]));
  g.addRule("OptConfigs", []);
  g.addRule("Configs", [new Nonterm("Config"), new Nonterm("Configs")], ListCons("Config", "Configs"));
  g.addRule("Configs", [new Nonterm("Config")]);
  g.addRule("Config", [new Token("NAME"), new Token("COLON"), new Token("NAME")]);
  g.addRule("Rules", [new Nonterm("Rule"), new Nonterm("Rules")], ListCons("Rule", "Rules"));
  g.addRule("Rules", [new Nonterm("Rule")]);
  g.addRule("Rule", [new Token("NAME"), new Token("COLON"), new Nonterm("Alts")]);
  g.addRule("Alts", [new Nonterm("Alt"), new Token("BAR"), new Nonterm("Alts")], ListCons("Alt", "Alts"));
  g.addRule("Alts", [new Nonterm("Alt")]);
  g.addRule("Alt", [new Nonterm("Items")]);
  g.addRule("Alt", [new Nonterm("Items"), new Token("LBRACE"), new Nonterm("Flags"), new Token("RBRACE")], KeepOnly(["Items", "Flags"]));
  g.addRule("Flags", [new Nonterm("Flag"), new Token("COMMA"), new Nonterm("Flags")], ListCons("Flag", "Flags"));
  g.addRule("Flags", [new Nonterm("Flag")]);
  g.addRule("Flag", [new Token("NAME"), new Token("COLON"), new Token("NAME")]);
  g.addRule("Items", [new Nonterm("Item"), new Nonterm("Items")], ListCons("Item", "Items"));
  g.addRule("Items", []);
  g.addRule("Item", [new Nonterm("Name")], Inline);
  g.addRule("Item", [new Nonterm("Opt")], Inline);
  g.addRule("Item", [new Nonterm("Star")], Inline);
  g.addRule("Item", [new Nonterm("Plus")], Inline);
  g.addRule("Item", [new Nonterm("Paren")], Inline);
  g.addRule("Name", [new Token("NAME")]);
  g.addRule("Opt", [new Token("LBRACK"), new Nonterm("Alts"), new Token("RBRACK")], KeepOnly(["Alts"]));
  g.addRule("Paren", [new Token("LPAREN"), new Nonterm("Alts"), new Token("RPAREN")], KeepOnly(["Alts"], true));
  g.addRule("Star", [new Nonterm("Item"), new Token("STAR")], KeepOnly(["Item"]));
  g.addRule("Plus", [new Nonterm("Item"), new Token("PLUS")], KeepOnly(["Item"]));
  g.initializeParser();





  function generateGrammar(bnf, name) {
    var config = generateOptConfig(bnf.kids[0]);
    config.name = config.name || name || "Grammar";
    return { config: config,
             output: generateRules(bnf.kids[1], config) };
  }
  function generateOptConfig(optconfig) {
    if (optconfig.kids.length > 0)
      return generateConfig(optconfig.kids[0]);
    else
      return {};
  }
  function generateConfig(config) {
    var ret = {};
    for (var i = 0; i < config.kids.length; i++) {
      ret[config.kids[i].kids[0].value] = config.kids[i].kids[2].value;
    }
    return ret;
  }
  function generateRules(rules, config) {
    var ret = [];
    var firstRule = undefined;
    for (var i = 0; i < rules.kids.length; i++) {
      if (rules.kids[i].name === "Rule") {
        firstRule = rules.kids[i].kids[0].value;
        break;
      }
    }
    ret.push("var g = new Grammar(" + JSON.stringify(config.name) + ", " + JSON.stringify(firstRule) + ");");
    for (var i = 0; i < rules.kids.length; i++) {
      var rule = generateRule(rules.kids[i]);
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
      if (shouldInline) {
        if (gen.flags)
          ret.rules.push("g.addRule(" + JSON.stringify(ruleName) + ", [" + gen.rhs + "], E.Rule.Inline, " 
                         + JSON.stringify(gen.flags) + ")");
        else
          ret.rules.push("g.addRule(" + JSON.stringify(ruleName) + ", [" + gen.rhs + "], E.Rule.Inline)");
      } else {
        if (gen.flags)
          ret.rules.push("g.addRule(" + JSON.stringify(ruleName) + ", [" + gen.rhs + "], undefined, " 
                         + JSON.stringify(gen.flags) + ")");
        else
          ret.rules.push("g.addRule(" + JSON.stringify(ruleName) + ", [" + gen.rhs + "])");
      }
      ret.rules = ret.rules.concat(gen.rules);
    } else {
      for (var i = 0; i < alts.kids.length; i++) {
        var gen = generateAlt(ruleName +"_A"+ i, alts.kids[i], true);
        if (shouldInline) {
          if (gen.flags)
            ret.rules.push("g.addRule(" + JSON.stringify(ruleName) + ", [" + gen.rhs + "], E.Rule.Inline, " 
                           + JSON.stringify(gen.flags) + ")");
          else
            ret.rules.push("g.addRule(" + JSON.stringify(ruleName) + ", [" + gen.rhs + "], E.Rule.Inline)");
        } else {
          if (gen.flags)
            ret.rules.push("g.addRule(" + JSON.stringify(ruleName) + ", [" + gen.rhs + "], undefined, " 
                           + JSON.stringify(gen.flags) + ")");
          else
            ret.rules.push("g.addRule(" + JSON.stringify(ruleName) + ", [" + gen.rhs + "])");
        }
        ret.rules = ret.rules.concat(gen.rules);
      }
    }
    return ret;
  }
  function generateAlt(ruleName, alt, shouldInline) {
    var ret = {rules: []}
    var flags = undefined;
    var gen = generateItems(ruleName, alt.kids[0].kids);
    if (shouldInline) 
      ret.rhs = gen.rhs;
    else
      ret.rules.push("g.addRule(" + JSON.stringify(ruleName) + ", [" + gen.rhs + "])");
    if (alt.kids.length > 1) {
      ret.flags = generateFlags(alt.kids[1]);
    }
    ret.rules = ret.rules.concat(gen.rules);
    return ret;
  }

  function generateFlags(flags) {
    var ret = {};
    for (var i = 0; i < flags.kids.length; i++) {
      var key = flags.kids[i].kids[0].value;
      var val = flags.kids[i].kids[2].value;
      var valAsNum = Number(val);
      if (!isNaN(valAsNum))
        ret[key] = valAsNum;
      else
        ret[key] = val;
    }
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
    } else if (item.name === "Plus") {
      var ret = {rules: []};
      var newNameOne = ruleName;
      var newNamePlus = newNameOne + "_plus";
      var json_newNameOne = JSON.stringify(newNameOne);
      var json_newNamePlus = JSON.stringify(newNamePlus);
      ret.rules.push("g.addRule(" + json_newNamePlus + ", " +
                     "[new Nonterm(" + json_newNameOne + ")], E.Rule.Inline);");
      ret.rules.push("g.addRule(" + json_newNamePlus + ", " +
                     "[new Nonterm(" + json_newNameOne + ")" + ", new Nonterm(" + json_newNamePlus + ")], " +
                     "E.Rule.ListCons(" + json_newNameOne + ", " + json_newNamePlus + ", true));");
      if (item.kids[0].name === "Alts") {
        var gen = generateAlts(newNameOne, item.kids[0], true);
        ret.rules = ret.rules.concat(gen.rules);
      } else {
        var gen = generateItems(newNameOne, [item.kids[0]]);
        ret.rules = ret.rules.concat(gen.rules);
        ret.rules.push("g.addRule(" + JSON.stringify(newNameOne) + ", [" + gen.rhs + "], E.Rule.Inline)");
      }
      ret.rhs = "new Nonterm(" + json_newNamePlus + ")";
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
    // console.log(parsed.toString(true));
    // console.log("g.count: ", g.countAllParses(parsed));
    var parses = g.constructAllParses(parsed);
    console.log("Found " + parses.length + " parses");
    // console.log(parses[0].toString());
    var bnfJS = generateGrammar(parses[0]);
    var filename = process.argv[3];
    var out = fs.createWriteStream(filename);
    out.write("const R = require('requirejs');\n\n");
    out.write("R.config({ baseUrl: __dirname });\n");
    out.write("console.log(__dirname);\n");
    out.write("R(['fs', '../../../lib/jglr/jglr'], function(fs, E) {\n");
    out.write("  const Grammar = E.Grammar\n");
    out.write("  const Nonterm = E.Nonterm\n");
    out.write("  const Token = E.Token\n");
    out.write("  const Rule = E.Rule\n\n");
    out.write("  " + bnfJS.output.join("\n  "));
    out.write("\n\n");
    out.write("  g.initializeParser(true);\n")
    out.write("  var cycles = g.checkForCycles();\n");
    out.write("  if (cycles === false) {\n");
    out.write("    console.log(\"Non-cyclic grammar -- all good!\");\n");
    out.write("  } else {\n");
    out.write("    console.log(\"Grammar has \" + cycles.length + \" cycles!\");\n");
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
    out.write("  out.write(\"  return { " + bnfJS.config.name + ": Grammar.fromSerializable(g_json) };\\n\");\n");
    out.write("  out.write(\"});\\n\");\n");
    out.write("  out.end();\n");
    out.write("});\n");
    out.end();
  } else {
    var next_tok = toks.curTok || toks.next();
    var msg = "Error reading grammar at token " + next_tok.toString(true) + " at position " + next_tok.pos.toString(true);
    throw msg;
  }
});
