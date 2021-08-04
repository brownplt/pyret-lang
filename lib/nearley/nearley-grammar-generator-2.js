// usage: node nearley-grammar-generator.js <some-bnf> <some-output-file>

const nearley = require("nearley");
const grammar = require("./bnf-grammar.js");
const fs = require("fs");

function generateGrammar(bnf, name) {
  var config = generateOptConfig(bnf.kids[0]);
  config.name = config.name || name || "Grammar";
  return {
    config: config,
    output: generateRules(bnf.kids[1], config)
  };
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
    if (rules.kids[i].name === "rule") {
      firstRule = rules.kids[i].kids[0].value;
      break;
    }
  }
  ret.push("ParserRules: [");
  var allRules = {};
  var ruleNames = []
  for (var i = 0; i < rules.kids.length; i++) {
    var rule = rules.kids[i];
    var ruleName = rule.kids[0].value;
    if (allRules[ruleName] === undefined) {
      allRules[ruleName] = rule;
      ruleNames.push(ruleName);
    } else {
      var alts = allRules[ruleName].kids[2];
      alts.kids.push.apply(alts.kids, rule.kids[2].kids);
    }
  }
  for (var i = 0; i < ruleNames.length; i++) {
    var rule = generateRule(allRules[ruleNames[i]]);
    for (var j = 0; j < rule.rules.length; j++)
      ret.push(rule.rules[j]);
  }
  ret.push("],");
  ret.push("ParserStart: " + JSON.stringify(firstRule));
  return ret;
}

function generateRule(rule) {
  var ruleName = rule.kids[0].value;
  var ret = { rules: [] }
  var gen = generateAlts(ruleName, rule.kids[2], false);
  ret.rules = ret.rules.concat(gen.rules);
  return ret;
}

function generateAlts(ruleName, alts, shouldInline) {
  var ret = { rules: [] }
  if (alts.kids.length === 1) {
    var gen = generateAlt(ruleName, alts.kids[0], true);
    if (shouldInline) {
      if (gen.flags) {
        ret.rules.push("{\"name\": " + JSON.stringify(ruleName) + ", \"symbols\": [" + gen.rhs + "], \"postprocess\": inlineAction(" + JSON.stringify(ruleName) + "), "
          + JSON.stringify(gen.flags) + "},");
      }
      else
        ret.rules.push("{\"name\": " + JSON.stringify(ruleName) + ", \"symbols\": [" + gen.rhs + "], \"postprocess\": inlineAction(" + JSON.stringify(ruleName) + ")},");
    } else {
      if (gen.flags) {
        ret.rules.push("{\"name\": " + JSON.stringify(ruleName) + ", \"symbols\": [" + gen.rhs + "], \"postprocess\": defaultAction(" + JSON.stringify(ruleName) + "), "
          + JSON.stringify(gen.flags) + "},");
      }
      else
        ret.rules.push("{\"name\": " + JSON.stringify(ruleName) + ", \"symbols\": [" + gen.rhs + "], \"postprocess\": defaultAction(" + JSON.stringify(ruleName) + ")},");
    }
    ret.rules = ret.rules.concat(gen.rules);
  } else {
    for (var i = 0; i < alts.kids.length; i++) {
      var gen = generateAlt(ruleName + "_A" + i, alts.kids[i], true);
      if (shouldInline) {
        if (gen.flags) {
          ret.rules.push("{\"name\": " + JSON.stringify(ruleName) + ", \"symbols\": [" + gen.rhs + "], \"postprocess\": inlineAction(" + JSON.stringify(ruleName) + "), "
            + JSON.stringify(gen.flags) + "},");
        }
        else
          ret.rules.push("{\"name\": " + JSON.stringify(ruleName) + ", \"symbols\": [" + gen.rhs + "], \"postprocess\": inlineAction(" + JSON.stringify(ruleName) + ")},");
      } else {
        if (gen.flags) {
          ret.rules.push("{\"name\": " + JSON.stringify(ruleName) + ", \"symbols\": [" + gen.rhs + "], \"postprocess\": defaultAction(" + JSON.stringify(ruleName) + "), "
            + JSON.stringify(gen.flags) + "},");
        }
        else
          ret.rules.push("{\"name\": " + JSON.stringify(ruleName) + ", \"symbols\": [" + gen.rhs + "], \"postprocess\": defaultAction(" + JSON.stringify(ruleName) + ")},");
      }
      ret.rules = ret.rules.concat(gen.rules);
    }
  }
  return ret;
}
function generateAlt(ruleName, alt, shouldInline) {
  var ret = { rules: [] }
  var flags = undefined;
  var gen = generateItems(ruleName, alt.kids[0].kids);
  if (shouldInline)
    ret.rhs = gen.rhs;
  else
    ret.rules.push("{\"name\": " + JSON.stringify(ruleName) + ", \"symbols\": [" + gen.rhs + "], \"postprocess\": defaultAction(" + JSON.stringify(ruleName) + ")},");
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
  var ret = { rules: [] }
  var genRhs = [];
  for (var i = 0; i < items.length; i++) {
    var gen = generateItem(ruleName + "_I" + i, items[i]);
    ret.rules = ret.rules.concat(gen.rules);
    genRhs.push(gen.rhs);
  }
  ret.rhs = genRhs.join(", ");
  return ret;
}

function generateItem(ruleName, item) {
  if (item.name === "name") {
    if (item.kids[0].value.toUpperCase() === item.kids[0].value)
      return { rhs: "(tokenizer.has(" + JSON.stringify(item.kids[0].value) + ") ? {type: " + JSON.stringify(item.kids[0].value) + "} : " + JSON.stringify(item.kids[0].value) + ")", rules: [] };
    else
      return { rhs:  JSON.stringify(item.kids[0].value), rules: [] };
  } else if (item.name === "star") {
    var ret = { rules: [] };
    var newNameOne = ruleName;
    var newNameStar = newNameOne + "*";
    var json_newNameOne = JSON.stringify(newNameOne);
    var json_newNameStar = JSON.stringify(newNameStar);
    ret.rules.push("{\"name\": "  + json_newNameStar + ", \"symbols\": [], \"postprocess\": inlineAction(" + json_newNameStar + ")},");
    ret.rules.push("{\"name\": "  + json_newNameStar + ", \"symbols\": [" +
      json_newNameStar + ", " + json_newNameOne + "], " + "\"postprocess\": listSnocAction(" + json_newNameStar + ", " + json_newNameOne + ", true)},");
    if (item.kids[0].name === "alts") {
      var gen = generateAlts(newNameOne, item.kids[0], true);
      ret.rules = ret.rules.concat(gen.rules);
    } else {
      var gen = generateItems(newNameOne, [item.kids[0]]);
      ret.rules = ret.rules.concat(gen.rules);
      ret.rules.push("{\"name\": " + JSON.stringify(newNameOne) + ", \"symbols\": [" + gen.rhs + "], \"postprocess\": inlineAction(" + JSON.stringify(newNameOne) + ")},");
    }
    ret.rhs = json_newNameStar;
    return ret;
  } else if (item.name === "plus") {
    var ret = { rules: [] };
    var newNameOne = ruleName;
    var newNamePlus = newNameOne + "_plus";
    var json_newNameOne = JSON.stringify(newNameOne);
    var json_newNamePlus = JSON.stringify(newNamePlus);
    ret.rules.push("{\"name\": " + json_newNamePlus + ", \"symbols\": " +
      "[" + json_newNameOne + "], \"postprocess\": inlineAction(" + json_newNamePlus + ")},");
    ret.rules.push("{\"name\": " + json_newNamePlus + ", \"symbols\": " +
      "[" + json_newNamePlus + ", " + json_newNameOne + "], " + "\"postprocess\": listSnocAction(" + json_newNamePlus + ", " + json_newNameOne + ", true)},");
    if (item.kids[0].name === "alt") {
      var gen = generateAlts(newNameOne, item.kids[0], true);
      ret.rules = ret.rules.concat(gen.rules);
    } else {
      var gen = generateItems(newNameOne, [item.kids[0]]);
      ret.rules = ret.rules.concat(gen.rules);
      ret.rules.push("{\"name\": " + JSON.stringify(newNameOne) + ", \"symbols\": [" + gen.rhs + "], \"postprocess\": inlineAction(" + JSON.stringify(newNameOne) + ")},");
    }
    ret.rhs = json_newNamePlus;
    return ret;
  } else if (item.name === "opt") {
    var ret = { rules: [] };
    var newNameOne = ruleName;
    var newNameOpt = newNameOne + "?";
    var json_newNameOne = JSON.stringify(newNameOne);
    var json_newNameOpt = JSON.stringify(newNameOpt);
    ret.rules.push("{\"name\": " + json_newNameOpt + ", \"symbols\": [], \"postprocess\": inlineAction(" + json_newNameOpt + ")},");
    ret.rules.push("{\"name\": " + json_newNameOpt + ", \"symbols\": [" + json_newNameOne + "], \"postprocess\": inlineAction(" + json_newNameOpt + ")},");
    var gen = generateAlts(newNameOne, item.kids[0], true);
    ret.rules = ret.rules.concat(gen.rules);
    ret.rhs = json_newNameOpt;
    return ret;
  } else if (item.name === "alts") {
    var ret = generateAlts(ruleName, item, true);
    ret.rhs = JSON.stringify(ruleName);
    return ret;
  } else {
    console.log("Unknown item " + JSON.stringify(item));
    return "UNKNOWN";
  }
}

const parser = new nearley.Parser(nearley.Grammar.fromCompiled(grammar));
const data = fs.readFileSync(process.argv[2], {encoding: "utf8"});
parser.feed(data);

if (parser.results.length === 0) {
  throw "No parses found!";
} else {
  console.log("Found " + parser.results.length + " parses");
  var bnfJS = generateGrammar(parser.results[0]);
  var filename = process.argv[3];
  var out = fs.createWriteStream(filename);
  out.write("(function () {\n");
  out.write("function id(x) { return x[0]; }\n\n");
  out.write("const R = require('requirejs');\n\n");
  out.write("R.config({paths: {'nearley': '.'}});\n\n");
  out.write("const T = R('nearley/nearley-tokenizer');\n");
  out.write("const tokenizer = T.Tokenizer;\n");
  out.write("const E = R('nearley/nearley-helpers');\n");
  out.write("const SrcLoc = E.SrcLoc;\n");
  const toStringFunction = `
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
`;
  out.write(toStringFunction);
  const defaultActionFunction = `
const defaultAction = function (name) {
  const ret = function (kids) {
    var useful_kids = [];
    for (var i = 0; i < kids.length; i++) {
      if (kids[i].shouldInline === true) useful_kids = useful_kids.concat(kids[i].kids);
      else useful_kids.push(kids[i]);
    }
    if (useful_kids.length > 0) {
      const pos = useful_kids[0].pos.combine(useful_kids[useful_kids.length  - 1].pos);
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
`;
  out.write(defaultActionFunction);
  const inlineActionFunction = `
const inlineAction = function (name) {
  const ret = function (kids) {
    const defaultRet = defaultAction(name)(kids);
    defaultRet.shouldInline = true;
    return defaultRet;
  }
  return ret;
};
`;
  out.write(inlineActionFunction);
  const listSnocActionFunction = `
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
    if (useful_kids.length > 0) {
      const pos = useful_kids[0].pos.combine(useful_kids[useful_kids.length  - 1].pos);
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
};\n
`;
  out.write(listSnocActionFunction);
  out.write("var grammar = {\n");
  out.write("\tLexer: tokenizer,\n");
  out.write("  " + bnfJS.output.join("\n  "));
  out.write("\n}\n");
  out.write("if (typeof module !== 'undefined'&& typeof module.exports !== 'undefined') {\n");
  out.write("\tmodule.exports = grammar;\n");
  out.write("} else {\n");
  out.write("\twindow.grammar = grammar;\n");
  out.write("}\n");
  out.write("})();\n");
  out.end();
}