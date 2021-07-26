const R = require('requirejs');

R.config({paths: {'jglr': '../../../jglr'}});

R(['fs', 'jglr/jglr'], function(fs, E) {
  const Grammar = E.Grammar
  const Nonterm = E.Nonterm
  const Token = E.Token
  const Rule = E.Rule

  var g = new Grammar("TestGrammar", "opt-add");
  g.addRule("opt-add", [new Token("NUMBER"), new Token("PLUS"), new Nonterm("opt-add_I2?")])
  g.addRule("opt-add_I2?", [], E.Rule.Inline);
  g.addRule("opt-add_I2?", [new Nonterm("opt-add_I2")], E.Rule.Inline);
  g.addRule("opt-add_I2", [new Token("NUMBER")], E.Rule.Inline)

  g.initializeParser(false);
  var cycles = g.checkForCycles();
  if (cycles === false) {
    console.log("Non-cyclic grammar -- all good!");
  } else {
    console.log("Grammar has " + cycles.length + " cycles!");
    for (var i = 0; i < cycles.length; i++)
      console.log(cycles[i]);
  }
  var g_json = JSON.stringify(g.toSerializable(1), null, '  ');
  var filename = process.argv[2];
  var out = fs.createWriteStream(filename);
  out.write("({\n");
  out.write("provides: {},");
  out.write("requires: [],");
  out.write("nativeRequires: [");
  out.write("\"jglr/jglr\"");
  out.write("],\n");
  out.write("/** @param {{Grammar : {fromSerializable : !Function}, Nonterm : !Object, Token : !Object, Rule : !Object}} E */\n");
  out.write("theModule: function(runtime, ns, uri, E) {\n");
  out.write("  const Grammar = E.Grammar;\n");
  out.write("  const Nonterm = E.Nonterm;\n");
  out.write("  const Token = E.Token;\n");
  out.write("  const Rule = E.Rule;\n\n");
  out.write("  var g_json = " + g_json.replace(/\n/g, "\n  ") + ";\n");
  out.write("  return runtime.makeModuleReturn({ TestGrammar: Grammar.fromSerializable(g_json) }, {});\n");
  out.write("}\n");
  out.write("})\n");
  out.end();
});
