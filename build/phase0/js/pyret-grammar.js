const R = require('requirejs');

R.config({paths: {'jglr': '../../../lib/jglr'}});

R(['fs', 'jglr/jglr'], function(fs, E) {
  const Grammar = E.Grammar
  const Nonterm = E.Nonterm
  const Token = E.Token
  const Rule = E.Rule

  var g = new Grammar("PyretGrammar", "program");
  g.addRule("program", [new Nonterm("prelude"), new Nonterm("block")])
  g.addRule("prelude", [new Nonterm("prelude_I0*")])
  g.addRule("prelude_I0*", [], E.Rule.Inline);
  g.addRule("prelude_I0*", [new Nonterm("prelude_I0*"), new Nonterm("prelude_I0")], E.Rule.ListSnoc("prelude_I0*", "prelude_I0", true));
  g.addRule("prelude_I0", [new Nonterm("provide-stmt")], E.Rule.Inline)
  g.addRule("prelude_I0", [new Nonterm("import-stmt")], E.Rule.Inline)
  g.addRule("import-stmt", [new Token("INCLUDE"), new Nonterm("import-source")])
  g.addRule("import-stmt", [new Token("INCLUDE"), new Token("FROM"), new Nonterm("module-ref"), new Token("COLON"), new Nonterm("import-stmt_A1_I4?"), new Token("END")])
  g.addRule("import-stmt_A1_I4?", [], E.Rule.Inline);
  g.addRule("import-stmt_A1_I4?", [new Nonterm("import-stmt_A1_I4")], E.Rule.Inline);
  g.addRule("import-stmt_A1_I4", [new Nonterm("include-spec"), new Nonterm("import-stmt_A1_I4_I1*"), new Nonterm("import-stmt_A1_I4_I2?")], E.Rule.Inline)
  g.addRule("import-stmt_A1_I4_I1*", [], E.Rule.Inline);
  g.addRule("import-stmt_A1_I4_I1*", [new Nonterm("import-stmt_A1_I4_I1*"), new Nonterm("import-stmt_A1_I4_I1")], E.Rule.ListSnoc("import-stmt_A1_I4_I1*", "import-stmt_A1_I4_I1", true));
  g.addRule("import-stmt_A1_I4_I1", [new Token("COMMA"), new Nonterm("include-spec")], E.Rule.Inline)
  g.addRule("import-stmt_A1_I4_I2?", [], E.Rule.Inline);
  g.addRule("import-stmt_A1_I4_I2?", [new Nonterm("import-stmt_A1_I4_I2")], E.Rule.Inline);
  g.addRule("import-stmt_A1_I4_I2", [new Token("COMMA")], E.Rule.Inline)
  g.addRule("import-stmt", [new Token("IMPORT"), new Nonterm("import-source"), new Token("AS"), new Token("NAME")])
  g.addRule("import-stmt", [new Token("IMPORT"), new Nonterm("comma-names"), new Token("FROM"), new Nonterm("import-source")])
  g.addRule("import-source", [new Nonterm("import-special")])
  g.addRule("import-source", [new Nonterm("import-name")])
  g.addRule("import-special", [new Token("NAME"), new Token("PARENNOSPACE"), new Token("STRING"), new Nonterm("import-special_I3*"), new Token("RPAREN")])
  g.addRule("import-special_I3*", [], E.Rule.Inline);
  g.addRule("import-special_I3*", [new Nonterm("import-special_I3*"), new Nonterm("import-special_I3")], E.Rule.ListSnoc("import-special_I3*", "import-special_I3", true));
  g.addRule("import-special_I3", [new Token("COMMA"), new Token("STRING")], E.Rule.Inline)
  g.addRule("import-name", [new Token("NAME")])
  g.addRule("include-spec", [new Nonterm("include-name-spec")])
  g.addRule("include-spec", [new Nonterm("include-type-spec")])
  g.addRule("include-spec", [new Nonterm("include-data-spec")])
  g.addRule("include-spec", [new Nonterm("include-module-spec")])
  g.addRule("include-name-spec", [new Nonterm("name-spec")])
  g.addRule("include-type-spec", [new Token("TYPE"), new Nonterm("name-spec")])
  g.addRule("include-data-spec", [new Token("DATA"), new Nonterm("data-name-spec"), new Nonterm("include-data-spec_I2?")])
  g.addRule("include-data-spec_I2?", [], E.Rule.Inline);
  g.addRule("include-data-spec_I2?", [new Nonterm("include-data-spec_I2")], E.Rule.Inline);
  g.addRule("include-data-spec_I2", [new Nonterm("hiding-spec")], E.Rule.Inline)
  g.addRule("include-module-spec", [new Token("MODULE"), new Nonterm("name-spec")])
  g.addRule("provide-stmt", [new Nonterm("provide-vals-stmt")])
  g.addRule("provide-stmt", [new Nonterm("provide-types-stmt")])
  g.addRule("provide-stmt", [new Nonterm("provide-block")])
  g.addRule("provide-vals-stmt", [new Token("PROVIDE"), new Nonterm("stmt"), new Token("END")])
  g.addRule("provide-vals-stmt", [new Token("PROVIDE"), new Nonterm("provide-vals-stmt_A1_I1")])
  g.addRule("provide-vals-stmt_A1_I1", [new Token("STAR")], E.Rule.Inline)
  g.addRule("provide-vals-stmt_A1_I1", [new Token("TIMES")], E.Rule.Inline)
  g.addRule("provide-types-stmt", [new Token("PROVIDE-TYPES"), new Nonterm("record-ann")])
  g.addRule("provide-types-stmt", [new Token("PROVIDE-TYPES"), new Nonterm("provide-types-stmt_A1_I1")])
  g.addRule("provide-types-stmt_A1_I1", [new Token("STAR")], E.Rule.Inline)
  g.addRule("provide-types-stmt_A1_I1", [new Token("TIMES")], E.Rule.Inline)
  g.addRule("provide-block", [new Token("PROVIDECOLON"), new Nonterm("provide-block_A0_I1?"), new Token("END")])
  g.addRule("provide-block_A0_I1?", [], E.Rule.Inline);
  g.addRule("provide-block_A0_I1?", [new Nonterm("provide-block_A0_I1")], E.Rule.Inline);
  g.addRule("provide-block_A0_I1", [new Nonterm("provide-spec"), new Nonterm("provide-block_A0_I1_I1*"), new Nonterm("provide-block_A0_I1_I2?")], E.Rule.Inline)
  g.addRule("provide-block_A0_I1_I1*", [], E.Rule.Inline);
  g.addRule("provide-block_A0_I1_I1*", [new Nonterm("provide-block_A0_I1_I1*"), new Nonterm("provide-block_A0_I1_I1")], E.Rule.ListSnoc("provide-block_A0_I1_I1*", "provide-block_A0_I1_I1", true));
  g.addRule("provide-block_A0_I1_I1", [new Token("COMMA"), new Nonterm("provide-spec")], E.Rule.Inline)
  g.addRule("provide-block_A0_I1_I2?", [], E.Rule.Inline);
  g.addRule("provide-block_A0_I1_I2?", [new Nonterm("provide-block_A0_I1_I2")], E.Rule.Inline);
  g.addRule("provide-block_A0_I1_I2", [new Token("COMMA")], E.Rule.Inline)
  g.addRule("provide-block", [new Token("PROVIDE"), new Token("FROM"), new Nonterm("module-ref"), new Token("COLON"), new Nonterm("provide-block_A1_I4?"), new Token("END")])
  g.addRule("provide-block_A1_I4?", [], E.Rule.Inline);
  g.addRule("provide-block_A1_I4?", [new Nonterm("provide-block_A1_I4")], E.Rule.Inline);
  g.addRule("provide-block_A1_I4", [new Nonterm("provide-spec"), new Nonterm("provide-block_A1_I4_I1*"), new Nonterm("provide-block_A1_I4_I2?")], E.Rule.Inline)
  g.addRule("provide-block_A1_I4_I1*", [], E.Rule.Inline);
  g.addRule("provide-block_A1_I4_I1*", [new Nonterm("provide-block_A1_I4_I1*"), new Nonterm("provide-block_A1_I4_I1")], E.Rule.ListSnoc("provide-block_A1_I4_I1*", "provide-block_A1_I4_I1", true));
  g.addRule("provide-block_A1_I4_I1", [new Token("COMMA"), new Nonterm("provide-spec")], E.Rule.Inline)
  g.addRule("provide-block_A1_I4_I2?", [], E.Rule.Inline);
  g.addRule("provide-block_A1_I4_I2?", [new Nonterm("provide-block_A1_I4_I2")], E.Rule.Inline);
  g.addRule("provide-block_A1_I4_I2", [new Token("COMMA")], E.Rule.Inline)
  g.addRule("provide-spec", [new Nonterm("provide-name-spec")])
  g.addRule("provide-spec", [new Nonterm("provide-type-spec")])
  g.addRule("provide-spec", [new Nonterm("provide-data-spec")])
  g.addRule("provide-spec", [new Nonterm("provide-module-spec")])
  g.addRule("name-spec", [new Nonterm("name-spec_A0_I0"), new Nonterm("name-spec_A0_I1?")])
  g.addRule("name-spec_A0_I0", [new Token("STAR")], E.Rule.Inline)
  g.addRule("name-spec_A0_I0", [new Token("TIMES")], E.Rule.Inline)
  g.addRule("name-spec_A0_I1?", [], E.Rule.Inline);
  g.addRule("name-spec_A0_I1?", [new Nonterm("name-spec_A0_I1")], E.Rule.Inline);
  g.addRule("name-spec_A0_I1", [new Nonterm("hiding-spec")], E.Rule.Inline)
  g.addRule("name-spec", [new Nonterm("module-ref")])
  g.addRule("name-spec", [new Nonterm("module-ref"), new Token("AS"), new Token("NAME")])
  g.addRule("data-name-spec", [new Nonterm("data-name-spec_A0_I0")])
  g.addRule("data-name-spec_A0_I0", [new Token("STAR")], E.Rule.Inline)
  g.addRule("data-name-spec_A0_I0", [new Token("TIMES")], E.Rule.Inline)
  g.addRule("data-name-spec", [new Nonterm("module-ref")])
  g.addRule("provide-name-spec", [new Nonterm("name-spec")])
  g.addRule("provide-type-spec", [new Token("TYPE"), new Nonterm("name-spec")])
  g.addRule("provide-data-spec", [new Token("DATA"), new Nonterm("data-name-spec"), new Nonterm("provide-data-spec_I2?")])
  g.addRule("provide-data-spec_I2?", [], E.Rule.Inline);
  g.addRule("provide-data-spec_I2?", [new Nonterm("provide-data-spec_I2")], E.Rule.Inline);
  g.addRule("provide-data-spec_I2", [new Nonterm("hiding-spec")], E.Rule.Inline)
  g.addRule("provide-module-spec", [new Token("MODULE"), new Nonterm("name-spec")])
  g.addRule("hiding-spec", [new Token("HIDING"), new Nonterm("hiding-spec_I1"), new Nonterm("hiding-spec_I2?"), new Token("RPAREN")])
  g.addRule("hiding-spec_I1", [new Token("PARENSPACE")], E.Rule.Inline)
  g.addRule("hiding-spec_I1", [new Token("PARENNOSPACE")], E.Rule.Inline)
  g.addRule("hiding-spec_I2?", [], E.Rule.Inline);
  g.addRule("hiding-spec_I2?", [new Nonterm("hiding-spec_I2")], E.Rule.Inline);
  g.addRule("hiding-spec_I2", [new Nonterm("hiding-spec_I2_I0*"), new Token("NAME")], E.Rule.Inline)
  g.addRule("hiding-spec_I2_I0*", [], E.Rule.Inline);
  g.addRule("hiding-spec_I2_I0*", [new Nonterm("hiding-spec_I2_I0*"), new Nonterm("hiding-spec_I2_I0")], E.Rule.ListSnoc("hiding-spec_I2_I0*", "hiding-spec_I2_I0", true));
  g.addRule("hiding-spec_I2_I0", [new Token("NAME"), new Token("COMMA")], E.Rule.Inline)
  g.addRule("module-ref", [new Nonterm("module-ref_I0*"), new Token("NAME")])
  g.addRule("module-ref_I0*", [], E.Rule.Inline);
  g.addRule("module-ref_I0*", [new Nonterm("module-ref_I0*"), new Nonterm("module-ref_I0")], E.Rule.ListSnoc("module-ref_I0*", "module-ref_I0", true));
  g.addRule("module-ref_I0", [new Token("NAME"), new Token("DOT")], E.Rule.Inline)
  g.addRule("comma-names", [new Token("NAME"), new Nonterm("comma-names_I1*")])
  g.addRule("comma-names_I1*", [], E.Rule.Inline);
  g.addRule("comma-names_I1*", [new Nonterm("comma-names_I1*"), new Nonterm("comma-names_I1")], E.Rule.ListSnoc("comma-names_I1*", "comma-names_I1", true));
  g.addRule("comma-names_I1", [new Token("COMMA"), new Token("NAME")], E.Rule.Inline)
  g.addRule("block", [new Nonterm("block_I0*")])
  g.addRule("block_I0*", [], E.Rule.Inline);
  g.addRule("block_I0*", [new Nonterm("block_I0*"), new Nonterm("block_I0")], E.Rule.ListSnoc("block_I0*", "block_I0", true));
  g.addRule("block_I0", [new Nonterm("stmt")], E.Rule.Inline)
  g.addRule("stmt", [new Nonterm("type-expr")])
  g.addRule("stmt", [new Nonterm("newtype-expr")])
  g.addRule("stmt", [new Nonterm("spy-stmt")])
  g.addRule("stmt", [new Nonterm("let-expr")])
  g.addRule("stmt", [new Nonterm("fun-expr")])
  g.addRule("stmt", [new Nonterm("data-expr")])
  g.addRule("stmt", [new Nonterm("when-expr")])
  g.addRule("stmt", [new Nonterm("var-expr")])
  g.addRule("stmt", [new Nonterm("rec-expr")])
  g.addRule("stmt", [new Nonterm("assign-expr")])
  g.addRule("stmt", [new Nonterm("check-test")])
  g.addRule("stmt", [new Nonterm("check-expr")])
  g.addRule("stmt", [new Nonterm("contract-stmt")])
  g.addRule("spy-stmt", [new Token("SPY"), new Nonterm("spy-stmt_I1?"), new Token("COLON"), new Nonterm("spy-stmt_I3?"), new Token("END")])
  g.addRule("spy-stmt_I1?", [], E.Rule.Inline);
  g.addRule("spy-stmt_I1?", [new Nonterm("spy-stmt_I1")], E.Rule.Inline);
  g.addRule("spy-stmt_I1", [new Nonterm("binop-expr")], E.Rule.Inline)
  g.addRule("spy-stmt_I3?", [], E.Rule.Inline);
  g.addRule("spy-stmt_I3?", [new Nonterm("spy-stmt_I3")], E.Rule.Inline);
  g.addRule("spy-stmt_I3", [new Nonterm("spy-contents")], E.Rule.Inline)
  g.addRule("spy-contents", [new Nonterm("spy-field"), new Nonterm("spy-contents_I1*")])
  g.addRule("spy-contents_I1*", [], E.Rule.Inline);
  g.addRule("spy-contents_I1*", [new Nonterm("spy-contents_I1*"), new Nonterm("spy-contents_I1")], E.Rule.ListSnoc("spy-contents_I1*", "spy-contents_I1", true));
  g.addRule("spy-contents_I1", [new Token("COMMA"), new Nonterm("spy-field")], E.Rule.Inline)
  g.addRule("spy-field", [new Nonterm("id-expr")])
  g.addRule("spy-field", [new Token("NAME"), new Token("COLON"), new Nonterm("binop-expr")])
  g.addRule("type-expr", [new Token("TYPE"), new Token("NAME"), new Nonterm("ty-params"), new Token("EQUALS"), new Nonterm("ann")])
  g.addRule("newtype-expr", [new Token("NEWTYPE"), new Token("NAME"), new Token("AS"), new Token("NAME")])
  g.addRule("let-expr", [new Nonterm("toplevel-binding"), new Token("EQUALS"), new Nonterm("binop-expr")])
  g.addRule("binding", [new Nonterm("name-binding")])
  g.addRule("binding", [new Nonterm("tuple-binding")])
  g.addRule("tuple-binding", [new Token("LBRACE"), new Nonterm("tuple-binding_I1*"), new Nonterm("binding"), new Nonterm("tuple-binding_I3?"), new Token("RBRACE"), new Nonterm("tuple-binding_I5?")])
  g.addRule("tuple-binding_I1*", [], E.Rule.Inline);
  g.addRule("tuple-binding_I1*", [new Nonterm("tuple-binding_I1*"), new Nonterm("tuple-binding_I1")], E.Rule.ListSnoc("tuple-binding_I1*", "tuple-binding_I1", true));
  g.addRule("tuple-binding_I1", [new Nonterm("binding"), new Token("SEMI")], E.Rule.Inline)
  g.addRule("tuple-binding_I3?", [], E.Rule.Inline);
  g.addRule("tuple-binding_I3?", [new Nonterm("tuple-binding_I3")], E.Rule.Inline);
  g.addRule("tuple-binding_I3", [new Token("SEMI")], E.Rule.Inline)
  g.addRule("tuple-binding_I5?", [], E.Rule.Inline);
  g.addRule("tuple-binding_I5?", [new Nonterm("tuple-binding_I5")], E.Rule.Inline);
  g.addRule("tuple-binding_I5", [new Token("AS"), new Nonterm("name-binding")], E.Rule.Inline)
  g.addRule("name-binding", [new Nonterm("name-binding_I0?"), new Token("NAME"), new Nonterm("name-binding_I2?")])
  g.addRule("name-binding_I0?", [], E.Rule.Inline);
  g.addRule("name-binding_I0?", [new Nonterm("name-binding_I0")], E.Rule.Inline);
  g.addRule("name-binding_I0", [new Token("SHADOW")], E.Rule.Inline)
  g.addRule("name-binding_I2?", [], E.Rule.Inline);
  g.addRule("name-binding_I2?", [new Nonterm("name-binding_I2")], E.Rule.Inline);
  g.addRule("name-binding_I2", [new Token("COLONCOLON"), new Nonterm("ann")], E.Rule.Inline)
  g.addRule("toplevel-binding", [new Nonterm("binding")])
  g.addRule("multi-let-expr", [new Token("LET"), new Nonterm("let-binding"), new Nonterm("multi-let-expr_I2*"), new Nonterm("multi-let-expr_I3"), new Nonterm("block"), new Token("END")])
  g.addRule("multi-let-expr_I2*", [], E.Rule.Inline);
  g.addRule("multi-let-expr_I2*", [new Nonterm("multi-let-expr_I2*"), new Nonterm("multi-let-expr_I2")], E.Rule.ListSnoc("multi-let-expr_I2*", "multi-let-expr_I2", true));
  g.addRule("multi-let-expr_I2", [new Token("COMMA"), new Nonterm("let-binding")], E.Rule.Inline)
  g.addRule("multi-let-expr_I3", [new Token("BLOCK")], E.Rule.Inline)
  g.addRule("multi-let-expr_I3", [new Token("COLON")], E.Rule.Inline)
  g.addRule("let-binding", [new Nonterm("let-expr")])
  g.addRule("let-binding", [new Nonterm("var-expr")])
  g.addRule("letrec-expr", [new Token("LETREC"), new Nonterm("let-expr"), new Nonterm("letrec-expr_I2*"), new Nonterm("letrec-expr_I3"), new Nonterm("block"), new Token("END")])
  g.addRule("letrec-expr_I2*", [], E.Rule.Inline);
  g.addRule("letrec-expr_I2*", [new Nonterm("letrec-expr_I2*"), new Nonterm("letrec-expr_I2")], E.Rule.ListSnoc("letrec-expr_I2*", "letrec-expr_I2", true));
  g.addRule("letrec-expr_I2", [new Token("COMMA"), new Nonterm("let-expr")], E.Rule.Inline)
  g.addRule("letrec-expr_I3", [new Token("BLOCK")], E.Rule.Inline)
  g.addRule("letrec-expr_I3", [new Token("COLON")], E.Rule.Inline)
  g.addRule("type-bind", [new Token("NAME"), new Nonterm("ty-params"), new Token("EQUALS"), new Nonterm("ann")])
  g.addRule("newtype-bind", [new Token("NEWTYPE"), new Token("NAME"), new Token("AS"), new Token("NAME")])
  g.addRule("type-let-bind", [new Nonterm("type-bind")])
  g.addRule("type-let-bind", [new Nonterm("newtype-bind")])
  g.addRule("type-let-expr", [new Token("TYPE-LET"), new Nonterm("type-let-bind"), new Nonterm("type-let-expr_I2*"), new Nonterm("type-let-expr_I3"), new Nonterm("block"), new Token("END")])
  g.addRule("type-let-expr_I2*", [], E.Rule.Inline);
  g.addRule("type-let-expr_I2*", [new Nonterm("type-let-expr_I2*"), new Nonterm("type-let-expr_I2")], E.Rule.ListSnoc("type-let-expr_I2*", "type-let-expr_I2", true));
  g.addRule("type-let-expr_I2", [new Token("COMMA"), new Nonterm("type-let-bind")], E.Rule.Inline)
  g.addRule("type-let-expr_I3", [new Token("BLOCK")], E.Rule.Inline)
  g.addRule("type-let-expr_I3", [new Token("COLON")], E.Rule.Inline)
  g.addRule("contract-stmt", [new Token("NAME"), new Token("COLONCOLON"), new Nonterm("ty-params"), new Nonterm("contract-stmt_I3")])
  g.addRule("contract-stmt_I3", [new Nonterm("ann")], E.Rule.Inline)
  g.addRule("contract-stmt_I3", [new Nonterm("noparen-arrow-ann")], E.Rule.Inline)
  g.addRule("fun-expr", [new Token("FUN"), new Token("NAME"), new Nonterm("fun-header"), new Nonterm("fun-expr_I3"), new Nonterm("doc-string"), new Nonterm("block"), new Nonterm("where-clause"), new Token("END")])
  g.addRule("fun-expr_I3", [new Token("BLOCK")], E.Rule.Inline)
  g.addRule("fun-expr_I3", [new Token("COLON")], E.Rule.Inline)
  g.addRule("fun-header", [new Nonterm("ty-params"), new Nonterm("args"), new Nonterm("return-ann")])
  g.addRule("fun-header", [new Nonterm("ty-params"), new Nonterm("bad-args"), new Nonterm("return-ann")])
  g.addRule("ty-params", [new Nonterm("ty-params_I0?")])
  g.addRule("ty-params_I0?", [], E.Rule.Inline);
  g.addRule("ty-params_I0?", [new Nonterm("ty-params_I0")], E.Rule.Inline);
  g.addRule("ty-params_I0", [new Nonterm("ty-params_I0_I0"), new Nonterm("comma-names"), new Nonterm("ty-params_I0_I2")], E.Rule.Inline)
  g.addRule("ty-params_I0_I0", [new Token("LANGLE")], E.Rule.Inline)
  g.addRule("ty-params_I0_I0", [new Token("LT")], E.Rule.Inline)
  g.addRule("ty-params_I0_I2", [new Token("RANGLE")], E.Rule.Inline)
  g.addRule("ty-params_I0_I2", [new Token("GT")], E.Rule.Inline)
  g.addRule("args", [new Nonterm("args_I0"), new Nonterm("args_I1?"), new Token("RPAREN")])
  g.addRule("args_I0", [new Token("PARENNOSPACE")], E.Rule.Inline)
  g.addRule("args_I0", [new Token("PARENAFTERBRACE")], E.Rule.Inline)
  g.addRule("args_I1?", [], E.Rule.Inline);
  g.addRule("args_I1?", [new Nonterm("args_I1")], E.Rule.Inline);
  g.addRule("args_I1", [new Nonterm("binding"), new Nonterm("args_I1_I1*")], E.Rule.Inline)
  g.addRule("args_I1_I1*", [], E.Rule.Inline);
  g.addRule("args_I1_I1*", [new Nonterm("args_I1_I1*"), new Nonterm("args_I1_I1")], E.Rule.ListSnoc("args_I1_I1*", "args_I1_I1", true));
  g.addRule("args_I1_I1", [new Token("COMMA"), new Nonterm("binding")], E.Rule.Inline)
  g.addRule("bad-args", [new Token("PARENSPACE"), new Nonterm("bad-args_I1?"), new Token("RPAREN")])
  g.addRule("bad-args_I1?", [], E.Rule.Inline);
  g.addRule("bad-args_I1?", [new Nonterm("bad-args_I1")], E.Rule.Inline);
  g.addRule("bad-args_I1", [new Nonterm("binding"), new Nonterm("bad-args_I1_I1*")], E.Rule.Inline)
  g.addRule("bad-args_I1_I1*", [], E.Rule.Inline);
  g.addRule("bad-args_I1_I1*", [new Nonterm("bad-args_I1_I1*"), new Nonterm("bad-args_I1_I1")], E.Rule.ListSnoc("bad-args_I1_I1*", "bad-args_I1_I1", true));
  g.addRule("bad-args_I1_I1", [new Token("COMMA"), new Nonterm("binding")], E.Rule.Inline)
  g.addRule("return-ann", [new Nonterm("return-ann_I0?")])
  g.addRule("return-ann_I0?", [], E.Rule.Inline);
  g.addRule("return-ann_I0?", [new Nonterm("return-ann_I0")], E.Rule.Inline);
  g.addRule("return-ann_I0", [new Token("THINARROW"), new Nonterm("ann")], E.Rule.Inline)
  g.addRule("doc-string", [new Nonterm("doc-string_I0?")])
  g.addRule("doc-string_I0?", [], E.Rule.Inline);
  g.addRule("doc-string_I0?", [new Nonterm("doc-string_I0")], E.Rule.Inline);
  g.addRule("doc-string_I0", [new Token("DOC"), new Token("STRING")], E.Rule.Inline)
  g.addRule("where-clause", [new Nonterm("where-clause_I0?")])
  g.addRule("where-clause_I0?", [], E.Rule.Inline);
  g.addRule("where-clause_I0?", [new Nonterm("where-clause_I0")], E.Rule.Inline);
  g.addRule("where-clause_I0", [new Token("WHERE"), new Nonterm("block")], E.Rule.Inline)
  g.addRule("check-expr", [new Nonterm("check-expr_A0_I0"), new Token("STRING"), new Token("COLON"), new Nonterm("block"), new Token("END")])
  g.addRule("check-expr_A0_I0", [new Token("CHECK")], E.Rule.Inline)
  g.addRule("check-expr_A0_I0", [new Token("EXAMPLES")], E.Rule.Inline)
  g.addRule("check-expr", [new Nonterm("check-expr_A1_I0"), new Nonterm("block"), new Token("END")])
  g.addRule("check-expr_A1_I0", [new Token("CHECKCOLON")], E.Rule.Inline)
  g.addRule("check-expr_A1_I0", [new Token("EXAMPLESCOLON")], E.Rule.Inline)
  g.addRule("check-test", [new Nonterm("binop-expr"), new Nonterm("check-op"), new Nonterm("check-test_A0_I2?"), new Nonterm("binop-expr"), new Nonterm("check-test_A0_I4?")])
  g.addRule("check-test_A0_I2?", [], E.Rule.Inline);
  g.addRule("check-test_A0_I2?", [new Nonterm("check-test_A0_I2")], E.Rule.Inline);
  g.addRule("check-test_A0_I2", [new Token("PERCENT"), new Nonterm("check-test_A0_I2_I1"), new Nonterm("binop-expr"), new Token("RPAREN")], E.Rule.Inline)
  g.addRule("check-test_A0_I2_I1", [new Token("PARENSPACE")], E.Rule.Inline)
  g.addRule("check-test_A0_I2_I1", [new Token("PARENNOSPACE")], E.Rule.Inline)
  g.addRule("check-test_A0_I4?", [], E.Rule.Inline);
  g.addRule("check-test_A0_I4?", [new Nonterm("check-test_A0_I4")], E.Rule.Inline);
  g.addRule("check-test_A0_I4", [new Token("BECAUSE"), new Nonterm("binop-expr")], E.Rule.Inline)
  g.addRule("check-test", [new Nonterm("binop-expr"), new Nonterm("check-op-postfix"), new Nonterm("check-test_A1_I2?")])
  g.addRule("check-test_A1_I2?", [], E.Rule.Inline);
  g.addRule("check-test_A1_I2?", [new Nonterm("check-test_A1_I2")], E.Rule.Inline);
  g.addRule("check-test_A1_I2", [new Token("BECAUSE"), new Nonterm("binop-expr")], E.Rule.Inline)
  g.addRule("check-test", [new Nonterm("binop-expr")])
  g.addRule("data-expr", [new Token("DATA"), new Token("NAME"), new Nonterm("ty-params"), new Token("COLON"), new Nonterm("data-expr_I4?"), new Nonterm("data-expr_I5*"), new Nonterm("data-sharing"), new Nonterm("where-clause"), new Token("END")])
  g.addRule("data-expr_I4?", [], E.Rule.Inline);
  g.addRule("data-expr_I4?", [new Nonterm("data-expr_I4")], E.Rule.Inline);
  g.addRule("data-expr_I4", [new Nonterm("first-data-variant")], E.Rule.Inline)
  g.addRule("data-expr_I5*", [], E.Rule.Inline);
  g.addRule("data-expr_I5*", [new Nonterm("data-expr_I5*"), new Nonterm("data-expr_I5")], E.Rule.ListSnoc("data-expr_I5*", "data-expr_I5", true));
  g.addRule("data-expr_I5", [new Nonterm("data-variant")], E.Rule.Inline)
  g.addRule("variant-constructor", [new Token("NAME"), new Nonterm("variant-members")])
  g.addRule("first-data-variant", [new Nonterm("variant-constructor"), new Nonterm("data-with")])
  g.addRule("first-data-variant", [new Token("NAME"), new Nonterm("data-with")])
  g.addRule("data-variant", [new Token("BAR"), new Nonterm("variant-constructor"), new Nonterm("data-with")])
  g.addRule("data-variant", [new Token("BAR"), new Token("NAME"), new Nonterm("data-with")])
  g.addRule("variant-members", [new Token("PARENNOSPACE"), new Nonterm("variant-members_I1?"), new Token("RPAREN")])
  g.addRule("variant-members_I1?", [], E.Rule.Inline);
  g.addRule("variant-members_I1?", [new Nonterm("variant-members_I1")], E.Rule.Inline);
  g.addRule("variant-members_I1", [new Nonterm("variant-member"), new Nonterm("variant-members_I1_I1*")], E.Rule.Inline)
  g.addRule("variant-members_I1_I1*", [], E.Rule.Inline);
  g.addRule("variant-members_I1_I1*", [new Nonterm("variant-members_I1_I1*"), new Nonterm("variant-members_I1_I1")], E.Rule.ListSnoc("variant-members_I1_I1*", "variant-members_I1_I1", true));
  g.addRule("variant-members_I1_I1", [new Token("COMMA"), new Nonterm("variant-member")], E.Rule.Inline)
  g.addRule("variant-member", [new Nonterm("variant-member_I0?"), new Nonterm("binding")])
  g.addRule("variant-member_I0?", [], E.Rule.Inline);
  g.addRule("variant-member_I0?", [new Nonterm("variant-member_I0")], E.Rule.Inline);
  g.addRule("variant-member_I0", [new Token("REF")], E.Rule.Inline)
  g.addRule("data-with", [new Nonterm("data-with_I0?")])
  g.addRule("data-with_I0?", [], E.Rule.Inline);
  g.addRule("data-with_I0?", [new Nonterm("data-with_I0")], E.Rule.Inline);
  g.addRule("data-with_I0", [new Token("WITH"), new Nonterm("fields")], E.Rule.Inline)
  g.addRule("data-sharing", [new Nonterm("data-sharing_I0?")])
  g.addRule("data-sharing_I0?", [], E.Rule.Inline);
  g.addRule("data-sharing_I0?", [new Nonterm("data-sharing_I0")], E.Rule.Inline);
  g.addRule("data-sharing_I0", [new Token("SHARING"), new Nonterm("fields")], E.Rule.Inline)
  g.addRule("var-expr", [new Token("VAR"), new Nonterm("toplevel-binding"), new Token("EQUALS"), new Nonterm("binop-expr")])
  g.addRule("rec-expr", [new Token("REC"), new Nonterm("toplevel-binding"), new Token("EQUALS"), new Nonterm("binop-expr")])
  g.addRule("assign-expr", [new Token("NAME"), new Token("COLONEQUALS"), new Nonterm("binop-expr")])
  g.addRule("when-expr", [new Token("WHEN"), new Nonterm("binop-expr"), new Nonterm("when-expr_I2"), new Nonterm("block"), new Token("END")])
  g.addRule("when-expr_I2", [new Token("BLOCK")], E.Rule.Inline)
  g.addRule("when-expr_I2", [new Token("COLON")], E.Rule.Inline)
  g.addRule("binop-expr", [new Nonterm("expr"), new Nonterm("binop-expr_I1*")])
  g.addRule("binop-expr_I1*", [], E.Rule.Inline);
  g.addRule("binop-expr_I1*", [new Nonterm("binop-expr_I1*"), new Nonterm("binop-expr_I1")], E.Rule.ListSnoc("binop-expr_I1*", "binop-expr_I1", true));
  g.addRule("binop-expr_I1", [new Nonterm("binop"), new Nonterm("expr")], E.Rule.Inline)
  g.addRule("binop", [new Token("PLUS")])
  g.addRule("binop", [new Token("DASH")])
  g.addRule("binop", [new Token("TIMES")])
  g.addRule("binop", [new Token("SLASH")])
  g.addRule("binop", [new Token("LEQ")])
  g.addRule("binop", [new Token("GEQ")])
  g.addRule("binop", [new Token("EQUALEQUAL")])
  g.addRule("binop", [new Token("SPACESHIP")])
  g.addRule("binop", [new Token("EQUALTILDE")])
  g.addRule("binop", [new Token("NEQ")])
  g.addRule("binop", [new Token("LT")])
  g.addRule("binop", [new Token("GT")])
  g.addRule("binop", [new Token("AND")])
  g.addRule("binop", [new Token("OR")])
  g.addRule("binop", [new Token("CARET")])
  g.addRule("check-op", [new Token("IS")])
  g.addRule("check-op", [new Token("ISEQUALEQUAL")])
  g.addRule("check-op", [new Token("ISEQUALTILDE")])
  g.addRule("check-op", [new Token("ISSPACESHIP")])
  g.addRule("check-op", [new Token("ISROUGHLY")])
  g.addRule("check-op", [new Token("ISNOT")])
  g.addRule("check-op", [new Token("ISNOTEQUALEQUAL")])
  g.addRule("check-op", [new Token("ISNOTEQUALTILDE")])
  g.addRule("check-op", [new Token("ISNOTSPACESHIP")])
  g.addRule("check-op", [new Token("RAISES")])
  g.addRule("check-op", [new Token("RAISESOTHER")])
  g.addRule("check-op", [new Token("SATISFIES")])
  g.addRule("check-op", [new Token("SATISFIESNOT")])
  g.addRule("check-op", [new Token("RAISESSATISFIES")])
  g.addRule("check-op", [new Token("RAISESVIOLATES")])
  g.addRule("check-op-postfix", [new Token("RAISESNOT")])
  g.addRule("expr", [new Nonterm("paren-expr")])
  g.addRule("expr", [new Nonterm("id-expr")])
  g.addRule("expr", [new Nonterm("prim-expr")])
  g.addRule("expr", [new Nonterm("lambda-expr")])
  g.addRule("expr", [new Nonterm("method-expr")])
  g.addRule("expr", [new Nonterm("app-expr")])
  g.addRule("expr", [new Nonterm("obj-expr")])
  g.addRule("expr", [new Nonterm("tuple-expr")])
  g.addRule("expr", [new Nonterm("tuple-get")])
  g.addRule("expr", [new Nonterm("dot-expr")])
  g.addRule("expr", [new Nonterm("template-expr")])
  g.addRule("expr", [new Nonterm("bracket-expr")])
  g.addRule("expr", [new Nonterm("get-bang-expr")])
  g.addRule("expr", [new Nonterm("update-expr")])
  g.addRule("expr", [new Nonterm("extend-expr")])
  g.addRule("expr", [new Nonterm("if-expr")])
  g.addRule("expr", [new Nonterm("if-pipe-expr")])
  g.addRule("expr", [new Nonterm("cases-expr")])
  g.addRule("expr", [new Nonterm("for-expr")])
  g.addRule("expr", [new Nonterm("user-block-expr")])
  g.addRule("expr", [new Nonterm("inst-expr")])
  g.addRule("expr", [new Nonterm("multi-let-expr")])
  g.addRule("expr", [new Nonterm("letrec-expr")])
  g.addRule("expr", [new Nonterm("type-let-expr")])
  g.addRule("expr", [new Nonterm("construct-expr")])
  g.addRule("expr", [new Nonterm("table-select")])
  g.addRule("expr", [new Nonterm("table-extend")])
  g.addRule("expr", [new Nonterm("table-filter")])
  g.addRule("expr", [new Nonterm("table-order")])
  g.addRule("expr", [new Nonterm("table-extract")])
  g.addRule("expr", [new Nonterm("table-update")])
  g.addRule("expr", [new Nonterm("table-expr")])
  g.addRule("expr", [new Nonterm("load-table-expr")])
  g.addRule("expr", [new Nonterm("reactor-expr")])
  g.addRule("template-expr", [new Token("DOTDOTDOT")])
  g.addRule("bad-expr", [new Token("UNTERMINATED-STRING")])
  g.addRule("bad-expr", [new Token("UNTERMINATED-BLOCK-COMMENT")])
  g.addRule("bad-expr", [new Token("BAD-OPER")])
  g.addRule("bad-expr", [new Token("BAD-NUMBER")])
  g.addRule("bad-expr", [new Token("UNKNOWN")])
  g.addRule("paren-expr", [new Nonterm("paren-expr_I0"), new Nonterm("binop-expr"), new Token("RPAREN")])
  g.addRule("paren-expr_I0", [new Token("PARENSPACE")], E.Rule.Inline)
  g.addRule("paren-expr_I0", [new Token("PARENAFTERBRACE")], E.Rule.Inline)
  g.addRule("id-expr", [new Token("NAME")])
  g.addRule("prim-expr", [new Nonterm("num-expr")])
  g.addRule("prim-expr", [new Nonterm("frac-expr")])
  g.addRule("prim-expr", [new Nonterm("rfrac-expr")])
  g.addRule("prim-expr", [new Nonterm("bool-expr")])
  g.addRule("prim-expr", [new Nonterm("string-expr")])
  g.addRule("num-expr", [new Token("NUMBER")])
  g.addRule("frac-expr", [new Token("RATIONAL")])
  g.addRule("rfrac-expr", [new Token("ROUGHRATIONAL")])
  g.addRule("bool-expr", [new Token("TRUE")])
  g.addRule("bool-expr", [new Token("FALSE")])
  g.addRule("string-expr", [new Token("STRING")])
  g.addRule("lambda-expr", [new Token("LAM"), new Nonterm("fun-header"), new Nonterm("lambda-expr_A0_I2"), new Nonterm("doc-string"), new Nonterm("block"), new Nonterm("where-clause"), new Token("END")])
  g.addRule("lambda-expr_A0_I2", [new Token("BLOCK")], E.Rule.Inline)
  g.addRule("lambda-expr_A0_I2", [new Token("COLON")], E.Rule.Inline)
  g.addRule("lambda-expr", [new Token("LBRACE"), new Nonterm("fun-header"), new Nonterm("lambda-expr_A1_I2"), new Nonterm("doc-string"), new Nonterm("block"), new Nonterm("where-clause"), new Token("RBRACE")])
  g.addRule("lambda-expr_A1_I2", [new Token("BLOCK")], E.Rule.Inline)
  g.addRule("lambda-expr_A1_I2", [new Token("COLON")], E.Rule.Inline)
  g.addRule("method-expr", [new Token("METHOD"), new Nonterm("fun-header"), new Nonterm("method-expr_I2"), new Nonterm("doc-string"), new Nonterm("block"), new Nonterm("where-clause"), new Token("END")])
  g.addRule("method-expr_I2", [new Token("BLOCK")], E.Rule.Inline)
  g.addRule("method-expr_I2", [new Token("COLON")], E.Rule.Inline)
  g.addRule("app-expr", [new Nonterm("expr"), new Nonterm("app-args")])
  g.addRule("app-expr", [new Nonterm("expr"), new Token("PARENSPACE"), new Token("RPAREN")])
  g.addRule("app-expr", [new Nonterm("expr"), new Token("PARENSPACE"), new Nonterm("binop-expr"), new Token("COMMA"), new Nonterm("binop-expr"), new Nonterm("app-expr_A2_I5*"), new Token("RPAREN")])
  g.addRule("app-expr_A2_I5*", [], E.Rule.Inline);
  g.addRule("app-expr_A2_I5*", [new Nonterm("app-expr_A2_I5*"), new Nonterm("app-expr_A2_I5")], E.Rule.ListSnoc("app-expr_A2_I5*", "app-expr_A2_I5", true));
  g.addRule("app-expr_A2_I5", [new Token("COMMA"), new Nonterm("binop-expr")], E.Rule.Inline)
  g.addRule("app-args", [new Token("PARENNOSPACE"), new Nonterm("opt-comma-binops"), new Token("RPAREN")])
  g.addRule("opt-comma-binops", [new Nonterm("opt-comma-binops_I0?")])
  g.addRule("opt-comma-binops_I0?", [], E.Rule.Inline);
  g.addRule("opt-comma-binops_I0?", [new Nonterm("opt-comma-binops_I0")], E.Rule.Inline);
  g.addRule("opt-comma-binops_I0", [new Nonterm("comma-binops")], E.Rule.Inline)
  g.addRule("comma-binops", [new Nonterm("binop-expr"), new Nonterm("comma-binops_I1*")])
  g.addRule("comma-binops_I1*", [], E.Rule.Inline);
  g.addRule("comma-binops_I1*", [new Nonterm("comma-binops_I1*"), new Nonterm("comma-binops_I1")], E.Rule.ListSnoc("comma-binops_I1*", "comma-binops_I1", true));
  g.addRule("comma-binops_I1", [new Token("COMMA"), new Nonterm("binop-expr")], E.Rule.Inline)
  g.addRule("trailing-opt-comma-binops", [new Nonterm("trailing-opt-comma-binops_I0")])
  g.addRule("trailing-opt-comma-binops_I0", [new Nonterm("comma-binops"), new Nonterm("trailing-opt-comma-binops_I0_A0_I1?")], E.Rule.Inline)
  g.addRule("trailing-opt-comma-binops_I0_A0_I1?", [], E.Rule.Inline);
  g.addRule("trailing-opt-comma-binops_I0_A0_I1?", [new Nonterm("trailing-opt-comma-binops_I0_A0_I1")], E.Rule.Inline);
  g.addRule("trailing-opt-comma-binops_I0_A0_I1", [new Token("COMMA")], E.Rule.Inline)
  g.addRule("trailing-opt-comma-binops_I0", [], E.Rule.Inline)
  g.addRule("inst-expr", [new Nonterm("expr"), new Token("LANGLE"), new Nonterm("ann"), new Nonterm("inst-expr_I3*"), new Nonterm("inst-expr_I4")])
  g.addRule("inst-expr_I3*", [], E.Rule.Inline);
  g.addRule("inst-expr_I3*", [new Nonterm("inst-expr_I3*"), new Nonterm("inst-expr_I3")], E.Rule.ListSnoc("inst-expr_I3*", "inst-expr_I3", true));
  g.addRule("inst-expr_I3", [new Token("COMMA"), new Nonterm("ann")], E.Rule.Inline)
  g.addRule("inst-expr_I4", [new Token("RANGLE")], E.Rule.Inline)
  g.addRule("inst-expr_I4", [new Token("GT")], E.Rule.Inline)
  g.addRule("tuple-expr", [new Token("LBRACE"), new Nonterm("tuple-fields"), new Token("RBRACE")])
  g.addRule("tuple-fields", [new Nonterm("binop-expr"), new Nonterm("tuple-fields_I1*"), new Nonterm("tuple-fields_I2?")])
  g.addRule("tuple-fields_I1*", [], E.Rule.Inline);
  g.addRule("tuple-fields_I1*", [new Nonterm("tuple-fields_I1*"), new Nonterm("tuple-fields_I1")], E.Rule.ListSnoc("tuple-fields_I1*", "tuple-fields_I1", true));
  g.addRule("tuple-fields_I1", [new Token("SEMI"), new Nonterm("binop-expr")], E.Rule.Inline)
  g.addRule("tuple-fields_I2?", [], E.Rule.Inline);
  g.addRule("tuple-fields_I2?", [new Nonterm("tuple-fields_I2")], E.Rule.Inline);
  g.addRule("tuple-fields_I2", [new Token("SEMI")], E.Rule.Inline)
  g.addRule("tuple-get", [new Nonterm("expr"), new Token("DOT"), new Token("LBRACE"), new Token("NUMBER"), new Token("RBRACE")])
  g.addRule("obj-expr", [new Token("LBRACE"), new Nonterm("obj-fields"), new Token("RBRACE")])
  g.addRule("obj-expr", [new Token("LBRACE"), new Token("RBRACE")])
  g.addRule("obj-fields", [new Nonterm("obj-field"), new Nonterm("obj-fields_I1*"), new Nonterm("obj-fields_I2?")])
  g.addRule("obj-fields_I1*", [], E.Rule.Inline);
  g.addRule("obj-fields_I1*", [new Nonterm("obj-fields_I1*"), new Nonterm("obj-fields_I1")], E.Rule.ListSnoc("obj-fields_I1*", "obj-fields_I1", true));
  g.addRule("obj-fields_I1", [new Token("COMMA"), new Nonterm("obj-field")], E.Rule.Inline)
  g.addRule("obj-fields_I2?", [], E.Rule.Inline);
  g.addRule("obj-fields_I2?", [new Nonterm("obj-fields_I2")], E.Rule.Inline);
  g.addRule("obj-fields_I2", [new Token("COMMA")], E.Rule.Inline)
  g.addRule("obj-field", [new Nonterm("key"), new Token("COLON"), new Nonterm("binop-expr")])
  g.addRule("obj-field", [new Token("REF"), new Nonterm("key"), new Nonterm("obj-field_A1_I2?"), new Token("COLON"), new Nonterm("binop-expr")])
  g.addRule("obj-field_A1_I2?", [], E.Rule.Inline);
  g.addRule("obj-field_A1_I2?", [new Nonterm("obj-field_A1_I2")], E.Rule.Inline);
  g.addRule("obj-field_A1_I2", [new Token("COLONCOLON"), new Nonterm("ann")], E.Rule.Inline)
  g.addRule("obj-field", [new Token("METHOD"), new Nonterm("key"), new Nonterm("fun-header"), new Nonterm("obj-field_A2_I3"), new Nonterm("doc-string"), new Nonterm("block"), new Nonterm("where-clause"), new Token("END")])
  g.addRule("obj-field_A2_I3", [new Token("BLOCK")], E.Rule.Inline)
  g.addRule("obj-field_A2_I3", [new Token("COLON")], E.Rule.Inline)
  g.addRule("fields", [new Nonterm("field"), new Nonterm("fields_I1*"), new Nonterm("fields_I2?")])
  g.addRule("fields_I1*", [], E.Rule.Inline);
  g.addRule("fields_I1*", [new Nonterm("fields_I1*"), new Nonterm("fields_I1")], E.Rule.ListSnoc("fields_I1*", "fields_I1", true));
  g.addRule("fields_I1", [new Token("COMMA"), new Nonterm("field")], E.Rule.Inline)
  g.addRule("fields_I2?", [], E.Rule.Inline);
  g.addRule("fields_I2?", [new Nonterm("fields_I2")], E.Rule.Inline);
  g.addRule("fields_I2", [new Token("COMMA")], E.Rule.Inline)
  g.addRule("field", [new Nonterm("key"), new Token("COLON"), new Nonterm("binop-expr")])
  g.addRule("field", [new Token("METHOD"), new Nonterm("key"), new Nonterm("fun-header"), new Nonterm("field_A1_I3"), new Nonterm("doc-string"), new Nonterm("block"), new Nonterm("where-clause"), new Token("END")])
  g.addRule("field_A1_I3", [new Token("BLOCK")], E.Rule.Inline)
  g.addRule("field_A1_I3", [new Token("COLON")], E.Rule.Inline)
  g.addRule("key", [new Token("NAME")])
  g.addRule("construct-expr", [new Token("LBRACK"), new Nonterm("construct-modifier"), new Nonterm("binop-expr"), new Token("COLON"), new Nonterm("trailing-opt-comma-binops"), new Token("RBRACK")])
  g.addRule("construct-modifier", [])
  g.addRule("construct-modifier", [new Token("LAZY")])
  g.addRule("table-expr", [new Token("TABLE"), new Nonterm("table-headers"), new Nonterm("table-rows"), new Token("END")])
  g.addRule("table-headers", [new Nonterm("table-headers_I0?")])
  g.addRule("table-headers_I0?", [], E.Rule.Inline);
  g.addRule("table-headers_I0?", [new Nonterm("table-headers_I0")], E.Rule.Inline);
  g.addRule("table-headers_I0", [new Nonterm("table-headers_I0_I0*"), new Nonterm("table-header")], E.Rule.Inline)
  g.addRule("table-headers_I0_I0*", [], E.Rule.Inline);
  g.addRule("table-headers_I0_I0*", [new Nonterm("table-headers_I0_I0*"), new Nonterm("table-headers_I0_I0")], E.Rule.ListSnoc("table-headers_I0_I0*", "table-headers_I0_I0", true));
  g.addRule("table-headers_I0_I0", [new Nonterm("list-table-header")], E.Rule.Inline)
  g.addRule("list-table-header", [new Nonterm("table-header"), new Token("COMMA")])
  g.addRule("table-header", [new Token("NAME"), new Nonterm("table-header_I1?")])
  g.addRule("table-header_I1?", [], E.Rule.Inline);
  g.addRule("table-header_I1?", [new Nonterm("table-header_I1")], E.Rule.Inline);
  g.addRule("table-header_I1", [new Token("COLONCOLON"), new Nonterm("ann")], E.Rule.Inline)
  g.addRule("table-rows", [new Nonterm("table-rows_I0?")])
  g.addRule("table-rows_I0?", [], E.Rule.Inline);
  g.addRule("table-rows_I0?", [new Nonterm("table-rows_I0")], E.Rule.Inline);
  g.addRule("table-rows_I0", [new Nonterm("table-rows_I0_I0*"), new Nonterm("table-row")], E.Rule.Inline)
  g.addRule("table-rows_I0_I0*", [], E.Rule.Inline);
  g.addRule("table-rows_I0_I0*", [new Nonterm("table-rows_I0_I0*"), new Nonterm("table-rows_I0_I0")], E.Rule.ListSnoc("table-rows_I0_I0*", "table-rows_I0_I0", true));
  g.addRule("table-rows_I0_I0", [new Nonterm("table-row")], E.Rule.Inline)
  g.addRule("table-row", [new Token("ROW"), new Nonterm("table-items")])
  g.addRule("table-items", [new Nonterm("table-items_I0?")])
  g.addRule("table-items_I0?", [], E.Rule.Inline);
  g.addRule("table-items_I0?", [new Nonterm("table-items_I0")], E.Rule.Inline);
  g.addRule("table-items_I0", [new Nonterm("table-items_I0_I0*"), new Nonterm("binop-expr")], E.Rule.Inline)
  g.addRule("table-items_I0_I0*", [], E.Rule.Inline);
  g.addRule("table-items_I0_I0*", [new Nonterm("table-items_I0_I0*"), new Nonterm("table-items_I0_I0")], E.Rule.ListSnoc("table-items_I0_I0*", "table-items_I0_I0", true));
  g.addRule("table-items_I0_I0", [new Nonterm("list-table-item")], E.Rule.Inline)
  g.addRule("list-table-item", [new Nonterm("binop-expr"), new Token("COMMA")])
  g.addRule("reactor-expr", [new Token("REACTOR"), new Token("COLON"), new Nonterm("fields"), new Token("END")])
  g.addRule("dot-expr", [new Nonterm("expr"), new Token("DOT"), new Token("NAME")])
  g.addRule("bracket-expr", [new Nonterm("expr"), new Token("LBRACK"), new Nonterm("binop-expr"), new Token("RBRACK")])
  g.addRule("get-bang-expr", [new Nonterm("expr"), new Token("BANG"), new Token("NAME")])
  g.addRule("extend-expr", [new Nonterm("expr"), new Token("DOT"), new Token("LBRACE"), new Nonterm("fields"), new Token("RBRACE")])
  g.addRule("update-expr", [new Nonterm("expr"), new Token("BANG"), new Token("LBRACE"), new Nonterm("fields"), new Token("RBRACE")])
  g.addRule("if-expr", [new Token("IF"), new Nonterm("binop-expr"), new Nonterm("if-expr_I2"), new Nonterm("block"), new Nonterm("if-expr_I4*"), new Nonterm("if-expr_I5?"), new Token("END")])
  g.addRule("if-expr_I2", [new Token("BLOCK")], E.Rule.Inline)
  g.addRule("if-expr_I2", [new Token("COLON")], E.Rule.Inline)
  g.addRule("if-expr_I4*", [], E.Rule.Inline);
  g.addRule("if-expr_I4*", [new Nonterm("if-expr_I4*"), new Nonterm("if-expr_I4")], E.Rule.ListSnoc("if-expr_I4*", "if-expr_I4", true));
  g.addRule("if-expr_I4", [new Nonterm("else-if")], E.Rule.Inline)
  g.addRule("if-expr_I5?", [], E.Rule.Inline);
  g.addRule("if-expr_I5?", [new Nonterm("if-expr_I5")], E.Rule.Inline);
  g.addRule("if-expr_I5", [new Token("ELSECOLON"), new Nonterm("block")], E.Rule.Inline)
  g.addRule("else-if", [new Token("ELSEIF"), new Nonterm("binop-expr"), new Token("COLON"), new Nonterm("block")])
  g.addRule("if-pipe-expr", [new Token("ASK"), new Nonterm("if-pipe-expr_I1"), new Nonterm("if-pipe-expr_I2*"), new Nonterm("if-pipe-expr_I3?"), new Token("END")])
  g.addRule("if-pipe-expr_I1", [new Token("BLOCK")], E.Rule.Inline)
  g.addRule("if-pipe-expr_I1", [new Token("COLON")], E.Rule.Inline)
  g.addRule("if-pipe-expr_I2*", [], E.Rule.Inline);
  g.addRule("if-pipe-expr_I2*", [new Nonterm("if-pipe-expr_I2*"), new Nonterm("if-pipe-expr_I2")], E.Rule.ListSnoc("if-pipe-expr_I2*", "if-pipe-expr_I2", true));
  g.addRule("if-pipe-expr_I2", [new Nonterm("if-pipe-branch")], E.Rule.Inline)
  g.addRule("if-pipe-expr_I3?", [], E.Rule.Inline);
  g.addRule("if-pipe-expr_I3?", [new Nonterm("if-pipe-expr_I3")], E.Rule.Inline);
  g.addRule("if-pipe-expr_I3", [new Token("BAR"), new Token("OTHERWISECOLON"), new Nonterm("block")], E.Rule.Inline)
  g.addRule("if-pipe-branch", [new Token("BAR"), new Nonterm("binop-expr"), new Token("THENCOLON"), new Nonterm("block")])
  g.addRule("cases-binding", [new Nonterm("cases-binding_I0?"), new Nonterm("binding")])
  g.addRule("cases-binding_I0?", [], E.Rule.Inline);
  g.addRule("cases-binding_I0?", [new Nonterm("cases-binding_I0")], E.Rule.Inline);
  g.addRule("cases-binding_I0", [new Token("REF")], E.Rule.Inline)
  g.addRule("cases-args", [new Token("PARENNOSPACE"), new Nonterm("cases-args_I1?"), new Token("RPAREN")])
  g.addRule("cases-args_I1?", [], E.Rule.Inline);
  g.addRule("cases-args_I1?", [new Nonterm("cases-args_I1")], E.Rule.Inline);
  g.addRule("cases-args_I1", [new Nonterm("cases-binding"), new Nonterm("cases-args_I1_I1*")], E.Rule.Inline)
  g.addRule("cases-args_I1_I1*", [], E.Rule.Inline);
  g.addRule("cases-args_I1_I1*", [new Nonterm("cases-args_I1_I1*"), new Nonterm("cases-args_I1_I1")], E.Rule.ListSnoc("cases-args_I1_I1*", "cases-args_I1_I1", true));
  g.addRule("cases-args_I1_I1", [new Token("COMMA"), new Nonterm("cases-binding")], E.Rule.Inline)
  g.addRule("cases-expr", [new Token("CASES"), new Nonterm("cases-expr_I1"), new Nonterm("ann"), new Token("RPAREN"), new Nonterm("binop-expr"), new Nonterm("cases-expr_I5"), new Nonterm("cases-expr_I6*"), new Nonterm("cases-expr_I7?"), new Token("END")])
  g.addRule("cases-expr_I1", [new Token("PARENSPACE")], E.Rule.Inline)
  g.addRule("cases-expr_I1", [new Token("PARENNOSPACE")], E.Rule.Inline)
  g.addRule("cases-expr_I5", [new Token("BLOCK")], E.Rule.Inline)
  g.addRule("cases-expr_I5", [new Token("COLON")], E.Rule.Inline)
  g.addRule("cases-expr_I6*", [], E.Rule.Inline);
  g.addRule("cases-expr_I6*", [new Nonterm("cases-expr_I6*"), new Nonterm("cases-expr_I6")], E.Rule.ListSnoc("cases-expr_I6*", "cases-expr_I6", true));
  g.addRule("cases-expr_I6", [new Nonterm("cases-branch")], E.Rule.Inline)
  g.addRule("cases-expr_I7?", [], E.Rule.Inline);
  g.addRule("cases-expr_I7?", [new Nonterm("cases-expr_I7")], E.Rule.Inline);
  g.addRule("cases-expr_I7", [new Token("BAR"), new Token("ELSE"), new Token("THICKARROW"), new Nonterm("block")], E.Rule.Inline)
  g.addRule("cases-branch", [new Token("BAR"), new Token("NAME"), new Nonterm("cases-branch_I2?"), new Token("THICKARROW"), new Nonterm("block")])
  g.addRule("cases-branch_I2?", [], E.Rule.Inline);
  g.addRule("cases-branch_I2?", [new Nonterm("cases-branch_I2")], E.Rule.Inline);
  g.addRule("cases-branch_I2", [new Nonterm("cases-args")], E.Rule.Inline)
  g.addRule("for-bind", [new Nonterm("binding"), new Token("FROM"), new Nonterm("binop-expr")])
  g.addRule("for-expr", [new Token("FOR"), new Nonterm("expr"), new Token("PARENNOSPACE"), new Nonterm("for-expr_I3?"), new Token("RPAREN"), new Nonterm("return-ann"), new Nonterm("for-expr_I6"), new Nonterm("block"), new Token("END")])
  g.addRule("for-expr_I3?", [], E.Rule.Inline);
  g.addRule("for-expr_I3?", [new Nonterm("for-expr_I3")], E.Rule.Inline);
  g.addRule("for-expr_I3", [new Nonterm("for-bind"), new Nonterm("for-expr_I3_I1*")], E.Rule.Inline)
  g.addRule("for-expr_I3_I1*", [], E.Rule.Inline);
  g.addRule("for-expr_I3_I1*", [new Nonterm("for-expr_I3_I1*"), new Nonterm("for-expr_I3_I1")], E.Rule.ListSnoc("for-expr_I3_I1*", "for-expr_I3_I1", true));
  g.addRule("for-expr_I3_I1", [new Token("COMMA"), new Nonterm("for-bind")], E.Rule.Inline)
  g.addRule("for-expr_I6", [new Token("BLOCK")], E.Rule.Inline)
  g.addRule("for-expr_I6", [new Token("COLON")], E.Rule.Inline)
  g.addRule("column-order", [new Token("NAME"), new Nonterm("column-order_I1")])
  g.addRule("column-order_I1", [new Token("ASCENDING")], E.Rule.Inline)
  g.addRule("column-order_I1", [new Token("DESCENDING")], E.Rule.Inline)
  g.addRule("table-select", [new Token("TABLE-SELECT"), new Token("NAME"), new Nonterm("table-select_I2*"), new Token("FROM"), new Nonterm("expr"), new Token("END")])
  g.addRule("table-select_I2*", [], E.Rule.Inline);
  g.addRule("table-select_I2*", [new Nonterm("table-select_I2*"), new Nonterm("table-select_I2")], E.Rule.ListSnoc("table-select_I2*", "table-select_I2", true));
  g.addRule("table-select_I2", [new Token("COMMA"), new Token("NAME")], E.Rule.Inline)
  g.addRule("table-filter", [new Token("TABLE-FILTER"), new Nonterm("expr"), new Nonterm("table-filter_I2?"), new Token("COLON"), new Nonterm("binop-expr"), new Token("END")])
  g.addRule("table-filter_I2?", [], E.Rule.Inline);
  g.addRule("table-filter_I2?", [new Nonterm("table-filter_I2")], E.Rule.Inline);
  g.addRule("table-filter_I2", [new Token("USING"), new Nonterm("binding"), new Nonterm("table-filter_I2_I2*")], E.Rule.Inline)
  g.addRule("table-filter_I2_I2*", [], E.Rule.Inline);
  g.addRule("table-filter_I2_I2*", [new Nonterm("table-filter_I2_I2*"), new Nonterm("table-filter_I2_I2")], E.Rule.ListSnoc("table-filter_I2_I2*", "table-filter_I2_I2", true));
  g.addRule("table-filter_I2_I2", [new Token("COMMA"), new Nonterm("binding")], E.Rule.Inline)
  g.addRule("table-order", [new Token("TABLE-ORDER"), new Nonterm("expr"), new Token("COLON"), new Nonterm("column-order"), new Nonterm("table-order_I4*"), new Token("END")])
  g.addRule("table-order_I4*", [], E.Rule.Inline);
  g.addRule("table-order_I4*", [new Nonterm("table-order_I4*"), new Nonterm("table-order_I4")], E.Rule.ListSnoc("table-order_I4*", "table-order_I4", true));
  g.addRule("table-order_I4", [new Token("COMMA"), new Nonterm("column-order")], E.Rule.Inline)
  g.addRule("table-extract", [new Token("TABLE-EXTRACT"), new Token("NAME"), new Token("FROM"), new Nonterm("expr"), new Token("END")])
  g.addRule("table-update", [new Token("TABLE-UPDATE"), new Nonterm("expr"), new Nonterm("table-update_I2?"), new Token("COLON"), new Nonterm("obj-fields"), new Token("END")])
  g.addRule("table-update_I2?", [], E.Rule.Inline);
  g.addRule("table-update_I2?", [new Nonterm("table-update_I2")], E.Rule.Inline);
  g.addRule("table-update_I2", [new Token("USING"), new Nonterm("binding"), new Nonterm("table-update_I2_I2*")], E.Rule.Inline)
  g.addRule("table-update_I2_I2*", [], E.Rule.Inline);
  g.addRule("table-update_I2_I2*", [new Nonterm("table-update_I2_I2*"), new Nonterm("table-update_I2_I2")], E.Rule.ListSnoc("table-update_I2_I2*", "table-update_I2_I2", true));
  g.addRule("table-update_I2_I2", [new Token("COMMA"), new Nonterm("binding")], E.Rule.Inline)
  g.addRule("table-extend", [new Token("TABLE-EXTEND"), new Nonterm("expr"), new Nonterm("table-extend_I2?"), new Token("COLON"), new Nonterm("table-extend-fields"), new Token("END")])
  g.addRule("table-extend_I2?", [], E.Rule.Inline);
  g.addRule("table-extend_I2?", [new Nonterm("table-extend_I2")], E.Rule.Inline);
  g.addRule("table-extend_I2", [new Token("USING"), new Nonterm("binding"), new Nonterm("table-extend_I2_I2*")], E.Rule.Inline)
  g.addRule("table-extend_I2_I2*", [], E.Rule.Inline);
  g.addRule("table-extend_I2_I2*", [new Nonterm("table-extend_I2_I2*"), new Nonterm("table-extend_I2_I2")], E.Rule.ListSnoc("table-extend_I2_I2*", "table-extend_I2_I2", true));
  g.addRule("table-extend_I2_I2", [new Token("COMMA"), new Nonterm("binding")], E.Rule.Inline)
  g.addRule("table-extend-fields", [new Nonterm("table-extend-fields_I0*"), new Nonterm("table-extend-field"), new Nonterm("table-extend-fields_I2?")])
  g.addRule("table-extend-fields_I0*", [], E.Rule.Inline);
  g.addRule("table-extend-fields_I0*", [new Nonterm("table-extend-fields_I0*"), new Nonterm("table-extend-fields_I0")], E.Rule.ListSnoc("table-extend-fields_I0*", "table-extend-fields_I0", true));
  g.addRule("table-extend-fields_I0", [new Nonterm("list-table-extend-field")], E.Rule.Inline)
  g.addRule("table-extend-fields_I2?", [], E.Rule.Inline);
  g.addRule("table-extend-fields_I2?", [new Nonterm("table-extend-fields_I2")], E.Rule.Inline);
  g.addRule("table-extend-fields_I2", [new Token("COMMA")], E.Rule.Inline)
  g.addRule("list-table-extend-field", [new Nonterm("table-extend-field"), new Token("COMMA")])
  g.addRule("table-extend-field", [new Nonterm("key"), new Nonterm("table-extend-field_A0_I1?"), new Token("COLON"), new Nonterm("binop-expr")])
  g.addRule("table-extend-field_A0_I1?", [], E.Rule.Inline);
  g.addRule("table-extend-field_A0_I1?", [new Nonterm("table-extend-field_A0_I1")], E.Rule.Inline);
  g.addRule("table-extend-field_A0_I1", [new Token("COLONCOLON"), new Nonterm("ann")], E.Rule.Inline)
  g.addRule("table-extend-field", [new Nonterm("key"), new Nonterm("table-extend-field_A1_I1?"), new Token("COLON"), new Nonterm("expr"), new Token("OF"), new Token("NAME")])
  g.addRule("table-extend-field_A1_I1?", [], E.Rule.Inline);
  g.addRule("table-extend-field_A1_I1?", [new Nonterm("table-extend-field_A1_I1")], E.Rule.Inline);
  g.addRule("table-extend-field_A1_I1", [new Token("COLONCOLON"), new Nonterm("ann")], E.Rule.Inline)
  g.addRule("load-table-expr", [new Token("LOAD-TABLE"), new Token("COLON"), new Nonterm("table-headers"), new Nonterm("load-table-expr_I3?"), new Token("END")])
  g.addRule("load-table-expr_I3?", [], E.Rule.Inline);
  g.addRule("load-table-expr_I3?", [new Nonterm("load-table-expr_I3")], E.Rule.Inline);
  g.addRule("load-table-expr_I3", [new Nonterm("load-table-specs")], E.Rule.Inline)
  g.addRule("load-table-specs", [new Nonterm("load-table-specs_I0*"), new Nonterm("load-table-spec")])
  g.addRule("load-table-specs_I0*", [], E.Rule.Inline);
  g.addRule("load-table-specs_I0*", [new Nonterm("load-table-specs_I0*"), new Nonterm("load-table-specs_I0")], E.Rule.ListSnoc("load-table-specs_I0*", "load-table-specs_I0", true));
  g.addRule("load-table-specs_I0", [new Nonterm("load-table-spec")], E.Rule.Inline)
  g.addRule("load-table-spec", [new Token("SOURCECOLON"), new Nonterm("expr")])
  g.addRule("load-table-spec", [new Token("SANITIZE"), new Token("NAME"), new Token("USING"), new Nonterm("expr")])
  g.addRule("user-block-expr", [new Token("BLOCK"), new Nonterm("block"), new Token("END")])
  g.addRule("ann", [new Nonterm("name-ann")])
  g.addRule("ann", [new Nonterm("record-ann")])
  g.addRule("ann", [new Nonterm("arrow-ann")])
  g.addRule("ann", [new Nonterm("app-ann")])
  g.addRule("ann", [new Nonterm("pred-ann")])
  g.addRule("ann", [new Nonterm("dot-ann")])
  g.addRule("ann", [new Nonterm("tuple-ann")])
  g.addRule("name-ann", [new Token("NAME")])
  g.addRule("comma-ann-field", [new Nonterm("ann-field"), new Nonterm("comma-ann-field_I1*")])
  g.addRule("comma-ann-field_I1*", [], E.Rule.Inline);
  g.addRule("comma-ann-field_I1*", [new Nonterm("comma-ann-field_I1*"), new Nonterm("comma-ann-field_I1")], E.Rule.ListSnoc("comma-ann-field_I1*", "comma-ann-field_I1", true));
  g.addRule("comma-ann-field_I1", [new Token("COMMA"), new Nonterm("ann-field")], E.Rule.Inline)
  g.addRule("trailing-opt-comma-ann-field", [new Nonterm("trailing-opt-comma-ann-field_I0")])
  g.addRule("trailing-opt-comma-ann-field_I0", [new Nonterm("comma-ann-field"), new Nonterm("trailing-opt-comma-ann-field_I0_A0_I1?")], E.Rule.Inline)
  g.addRule("trailing-opt-comma-ann-field_I0_A0_I1?", [], E.Rule.Inline);
  g.addRule("trailing-opt-comma-ann-field_I0_A0_I1?", [new Nonterm("trailing-opt-comma-ann-field_I0_A0_I1")], E.Rule.Inline);
  g.addRule("trailing-opt-comma-ann-field_I0_A0_I1", [new Token("COMMA")], E.Rule.Inline)
  g.addRule("trailing-opt-comma-ann-field_I0", [], E.Rule.Inline)
  g.addRule("record-ann", [new Token("LBRACE"), new Nonterm("trailing-opt-comma-ann-field"), new Token("RBRACE")])
  g.addRule("ann-field", [new Token("NAME"), new Token("COLONCOLON"), new Nonterm("ann")])
  g.addRule("tuple-ann", [new Token("LBRACE"), new Nonterm("ann"), new Nonterm("tuple-ann_I2*"), new Nonterm("tuple-ann_I3?"), new Token("RBRACE")])
  g.addRule("tuple-ann_I2*", [], E.Rule.Inline);
  g.addRule("tuple-ann_I2*", [new Nonterm("tuple-ann_I2*"), new Nonterm("tuple-ann_I2")], E.Rule.ListSnoc("tuple-ann_I2*", "tuple-ann_I2", true));
  g.addRule("tuple-ann_I2", [new Token("SEMI"), new Nonterm("ann")], E.Rule.Inline)
  g.addRule("tuple-ann_I3?", [], E.Rule.Inline);
  g.addRule("tuple-ann_I3?", [new Nonterm("tuple-ann_I3")], E.Rule.Inline);
  g.addRule("tuple-ann_I3", [new Token("SEMI")], E.Rule.Inline)
  g.addRule("noparen-arrow-ann", [new Nonterm("noparen-arrow-ann_I0?"), new Token("THINARROW"), new Nonterm("ann")])
  g.addRule("noparen-arrow-ann_I0?", [], E.Rule.Inline);
  g.addRule("noparen-arrow-ann_I0?", [new Nonterm("noparen-arrow-ann_I0")], E.Rule.Inline);
  g.addRule("noparen-arrow-ann_I0", [new Nonterm("arrow-ann-args")], E.Rule.Inline)
  g.addRule("arrow-ann-args", [new Nonterm("comma-anns")])
  g.addRule("arrow-ann-args", [new Nonterm("arrow-ann-args_A1_I0"), new Nonterm("comma-ann-field"), new Token("RPAREN")])
  g.addRule("arrow-ann-args_A1_I0", [new Token("PARENSPACE")], E.Rule.Inline)
  g.addRule("arrow-ann-args_A1_I0", [new Token("PARENNOSPACE")], E.Rule.Inline)
  g.addRule("arrow-ann-args_A1_I0", [new Token("PARENAFTERBRACE")], E.Rule.Inline)
  g.addRule("arrow-ann", [new Nonterm("arrow-ann_I0"), new Nonterm("arrow-ann_I1?"), new Token("THINARROW"), new Nonterm("ann"), new Token("RPAREN")])
  g.addRule("arrow-ann_I0", [new Token("PARENSPACE")], E.Rule.Inline)
  g.addRule("arrow-ann_I0", [new Token("PARENNOSPACE")], E.Rule.Inline)
  g.addRule("arrow-ann_I0", [new Token("PARENAFTERBRACE")], E.Rule.Inline)
  g.addRule("arrow-ann_I1?", [], E.Rule.Inline);
  g.addRule("arrow-ann_I1?", [new Nonterm("arrow-ann_I1")], E.Rule.Inline);
  g.addRule("arrow-ann_I1", [new Nonterm("arrow-ann-args")], E.Rule.Inline)
  g.addRule("app-ann", [new Nonterm("app-ann_I0"), new Token("LANGLE"), new Nonterm("comma-anns"), new Nonterm("app-ann_I3")])
  g.addRule("app-ann_I0", [new Nonterm("name-ann")], E.Rule.Inline)
  g.addRule("app-ann_I0", [new Nonterm("dot-ann")], E.Rule.Inline)
  g.addRule("app-ann_I3", [new Token("RANGLE")], E.Rule.Inline)
  g.addRule("app-ann_I3", [new Token("GT")], E.Rule.Inline)
  g.addRule("comma-anns", [new Nonterm("ann"), new Nonterm("comma-anns_I1*")])
  g.addRule("comma-anns_I1*", [], E.Rule.Inline);
  g.addRule("comma-anns_I1*", [new Nonterm("comma-anns_I1*"), new Nonterm("comma-anns_I1")], E.Rule.ListSnoc("comma-anns_I1*", "comma-anns_I1", true));
  g.addRule("comma-anns_I1", [new Token("COMMA"), new Nonterm("ann")], E.Rule.Inline)
  g.addRule("pred-ann", [new Nonterm("ann"), new Token("PERCENT"), new Nonterm("pred-ann_I2"), new Nonterm("id-expr"), new Token("RPAREN")])
  g.addRule("pred-ann_I2", [new Token("PARENSPACE")], E.Rule.Inline)
  g.addRule("pred-ann_I2", [new Token("PARENNOSPACE")], E.Rule.Inline)
  g.addRule("dot-ann", [new Token("NAME"), new Token("DOT"), new Token("NAME")])

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
  out.write("define('pyret-base/js/pyret-parser', ['jglr/jglr'],\n");
  out.write("/** @param {{Grammar : {fromSerializable : !Function}, Nonterm : !Object, Token : !Object, Rule : !Object}} E */\n");
  out.write("function(E) {\n");
  out.write("  const Grammar = E.Grammar;\n");
  out.write("  const Nonterm = E.Nonterm;\n");
  out.write("  const Token = E.Token;\n");
  out.write("  const Rule = E.Rule;\n\n");
  out.write("  var g_json = " + g_json.replace(/\n/g, "\n  ") + ";\n");
  out.write("  return { PyretGrammar: Grammar.fromSerializable(g_json) };\n");
  out.write("});\n");
  out.end();
});
