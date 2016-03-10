const R = require('requirejs');

R(['fs', '../../../lib/jglr/jglr'], function(fs, E) {
  const Grammar = E.Grammar
  const Nonterm = E.Nonterm
  const Token = E.Token
  const Rule = E.Rule

  var g = new Grammar("PyretGrammar", "program");
  g.addRule("program", [new Nonterm("prelude"), new Nonterm("block")])
  g.addRule("end", [new Token("END")])
  g.addRule("end", [new Token("SEMI")])
  g.addRule("prelude", [new Nonterm("prelude_I0_opt"), new Nonterm("prelude_I1_opt"), new Nonterm("prelude_I2_star")])
  g.addRule("prelude_I0_opt", [], E.Rule.Inline);
  g.addRule("prelude_I0_opt", [new Nonterm("prelude_I0")], E.Rule.Inline);
  g.addRule("prelude_I0", [new Nonterm("provide-stmt")], E.Rule.Inline)
  g.addRule("prelude_I1_opt", [], E.Rule.Inline);
  g.addRule("prelude_I1_opt", [new Nonterm("prelude_I1")], E.Rule.Inline);
  g.addRule("prelude_I1", [new Nonterm("provide-types-stmt")], E.Rule.Inline)
  g.addRule("prelude_I2_star", [], E.Rule.Inline);
  g.addRule("prelude_I2_star", [new Nonterm("prelude_I2_star"), new Nonterm("prelude_I2")], E.Rule.ListSnoc("prelude_I2_star", "prelude_I2", true));
  g.addRule("prelude_I2", [new Nonterm("import-stmt")], E.Rule.Inline)
  g.addRule("import-stmt", [new Token("INCLUDE"), new Nonterm("import-source")])
  g.addRule("import-stmt", [new Token("IMPORT"), new Nonterm("import-source"), new Token("AS"), new Token("NAME")])
  g.addRule("import-stmt", [new Token("IMPORT"), new Token("NAME"), new Nonterm("import-stmt_I2_star"), new Token("FROM"), new Nonterm("import-source")])
  g.addRule("import-stmt_I2_star", [], E.Rule.Inline);
  g.addRule("import-stmt_I2_star", [new Nonterm("import-stmt_I2_star"), new Nonterm("import-stmt_I2")], E.Rule.ListSnoc("import-stmt_I2_star", "import-stmt_I2", true));
  g.addRule("import-stmt_I2", [new Token("COMMA"), new Token("NAME")], E.Rule.Inline)
  g.addRule("import-source", [new Nonterm("import-special")])
  g.addRule("import-source", [new Nonterm("import-name")])
  g.addRule("import-source", [new Nonterm("import-string")])
  g.addRule("import-special", [new Token("NAME"), new Token("PARENNOSPACE"), new Token("STRING"), new Nonterm("import-special_I3_star"), new Token("RPAREN")])
  g.addRule("import-special_I3_star", [], E.Rule.Inline);
  g.addRule("import-special_I3_star", [new Nonterm("import-special_I3_star"), new Nonterm("import-special_I3")], E.Rule.ListSnoc("import-special_I3_star", "import-special_I3", true));
  g.addRule("import-special_I3", [new Token("COMMA"), new Token("STRING")], E.Rule.Inline)
  g.addRule("import-name", [new Token("NAME")])
  g.addRule("import-string", [new Token("STRING")])
  g.addRule("provide-stmt", [new Token("PROVIDE"), new Nonterm("stmt"), new Nonterm("end")])
  g.addRule("provide-stmt", [new Token("PROVIDE"), new Token("STAR")])
  g.addRule("provide-types-stmt", [new Token("PROVIDE-TYPES"), new Nonterm("record-ann")])
  g.addRule("provide-types-stmt", [new Token("PROVIDE-TYPES"), new Token("STAR")])
  g.addRule("block", [new Nonterm("block_I0_star")])
  g.addRule("block_I0_star", [], E.Rule.Inline);
  g.addRule("block_I0_star", [new Nonterm("block_I0_star"), new Nonterm("block_I0")], E.Rule.ListSnoc("block_I0_star", "block_I0", true));
  g.addRule("block_I0", [new Nonterm("stmt")], E.Rule.Inline)
  g.addRule("stmt", [new Nonterm("type-expr")])
  g.addRule("stmt", [new Nonterm("newtype-expr")])
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
  g.addRule("type-expr", [new Token("TYPE"), new Token("NAME"), new Token("EQUALS"), new Nonterm("ann")])
  g.addRule("newtype-expr", [new Token("NEWTYPE"), new Token("NAME"), new Token("AS"), new Token("NAME")])
  g.addRule("let-expr", [new Nonterm("toplevel-binding"), new Token("EQUALS"), new Nonterm("binop-expr")])
  g.addRule("binding", [new Nonterm("binding_I0_opt"), new Token("NAME"), new Nonterm("binding_I2_opt")])
  g.addRule("binding_I0_opt", [], E.Rule.Inline);
  g.addRule("binding_I0_opt", [new Nonterm("binding_I0")], E.Rule.Inline);
  g.addRule("binding_I0", [new Token("SHADOW")], E.Rule.Inline)
  g.addRule("binding_I2_opt", [], E.Rule.Inline);
  g.addRule("binding_I2_opt", [new Nonterm("binding_I2")], E.Rule.Inline);
  g.addRule("binding_I2", [new Token("COLONCOLON"), new Nonterm("ann")], E.Rule.Inline)
  g.addRule("toplevel-binding", [new Nonterm("binding")])
  g.addRule("multi-let-expr", [new Token("LET"), new Nonterm("multi-let-expr_I1_star"), new Nonterm("let-binding"), new Token("COLON"), new Nonterm("block"), new Nonterm("end")])
  g.addRule("multi-let-expr_I1_star", [], E.Rule.Inline);
  g.addRule("multi-let-expr_I1_star", [new Nonterm("multi-let-expr_I1_star"), new Nonterm("multi-let-expr_I1")], E.Rule.ListSnoc("multi-let-expr_I1_star", "multi-let-expr_I1", true));
  g.addRule("multi-let-expr_I1", [new Nonterm("let-binding-elt")], E.Rule.Inline)
  g.addRule("let-binding-elt", [new Nonterm("let-binding"), new Token("COMMA")])
  g.addRule("let-binding", [new Nonterm("let-expr")])
  g.addRule("let-binding", [new Nonterm("var-expr")])
  g.addRule("letrec-expr", [new Token("LETREC"), new Nonterm("letrec-expr_I1_star"), new Nonterm("let-expr"), new Token("COLON"), new Nonterm("block"), new Nonterm("end")])
  g.addRule("letrec-expr_I1_star", [], E.Rule.Inline);
  g.addRule("letrec-expr_I1_star", [new Nonterm("letrec-expr_I1_star"), new Nonterm("letrec-expr_I1")], E.Rule.ListSnoc("letrec-expr_I1_star", "letrec-expr_I1", true));
  g.addRule("letrec-expr_I1", [new Nonterm("letrec-binding")], E.Rule.Inline)
  g.addRule("letrec-binding", [new Nonterm("let-expr"), new Token("COMMA")])
  g.addRule("type-bind", [new Token("NAME"), new Token("EQUALS"), new Nonterm("ann")])
  g.addRule("newtype-bind", [new Token("NEWTYPE"), new Token("NAME"), new Token("AS"), new Token("NAME")])
  g.addRule("type-let-bind", [new Nonterm("type-bind")])
  g.addRule("type-let-bind", [new Nonterm("newtype-bind")])
  g.addRule("type-let-bind-elt", [new Nonterm("type-let-bind"), new Token("COMMA")])
  g.addRule("type-let-expr", [new Token("TYPE-LET"), new Nonterm("type-let-expr_I1_star"), new Nonterm("type-let-bind"), new Token("COLON"), new Nonterm("block"), new Nonterm("end")])
  g.addRule("type-let-expr_I1_star", [], E.Rule.Inline);
  g.addRule("type-let-expr_I1_star", [new Nonterm("type-let-expr_I1_star"), new Nonterm("type-let-expr_I1")], E.Rule.ListSnoc("type-let-expr_I1_star", "type-let-expr_I1", true));
  g.addRule("type-let-expr_I1", [new Nonterm("type-let-bind-elt")], E.Rule.Inline)
  g.addRule("contract-stmt", [new Token("NAME"), new Token("COLONCOLON"), new Nonterm("contract-stmt_I2")])
  g.addRule("contract-stmt_I2", [new Nonterm("ann")], E.Rule.Inline)
  g.addRule("contract-stmt_I2", [new Nonterm("noparen-arrow-ann")], E.Rule.Inline)
  g.addRule("fun-expr", [new Token("FUN"), new Token("NAME"), new Nonterm("fun-header"), new Token("COLON"), new Nonterm("doc-string"), new Nonterm("block"), new Nonterm("where-clause"), new Nonterm("end")])
  g.addRule("fun-header", [new Nonterm("ty-params"), new Nonterm("args"), new Nonterm("return-ann")])
  g.addRule("ty-params", [new Nonterm("ty-params_I0_opt")])
  g.addRule("ty-params_I0_opt", [], E.Rule.Inline);
  g.addRule("ty-params_I0_opt", [new Nonterm("ty-params_I0")], E.Rule.Inline);
  g.addRule("ty-params_I0", [new Nonterm("ty-params_I0_I0"), new Nonterm("ty-params_I0_I1_star"), new Token("NAME"), new Nonterm("ty-params_I0_I3")], E.Rule.Inline)
  g.addRule("ty-params_I0_I0", [new Token("LANGLE")], E.Rule.Inline)
  g.addRule("ty-params_I0_I0", [new Token("LT")], E.Rule.Inline)
  g.addRule("ty-params_I0_I1_star", [], E.Rule.Inline);
  g.addRule("ty-params_I0_I1_star", [new Nonterm("ty-params_I0_I1_star"), new Nonterm("ty-params_I0_I1")], E.Rule.ListSnoc("ty-params_I0_I1_star", "ty-params_I0_I1", true));
  g.addRule("ty-params_I0_I1", [new Nonterm("list-ty-param")], E.Rule.Inline)
  g.addRule("ty-params_I0_I3", [new Token("RANGLE")], E.Rule.Inline)
  g.addRule("ty-params_I0_I3", [new Token("GT")], E.Rule.Inline)
  g.addRule("list-ty-param", [new Token("NAME"), new Token("COMMA")])
  g.addRule("args", [new Token("PARENNOSPACE"), new Nonterm("args_I1_opt"), new Token("RPAREN")])
  g.addRule("args_I1_opt", [], E.Rule.Inline);
  g.addRule("args_I1_opt", [new Nonterm("args_I1")], E.Rule.Inline);
  g.addRule("args_I1", [new Nonterm("args_I1_I0_star"), new Nonterm("binding")], E.Rule.Inline)
  g.addRule("args_I1_I0_star", [], E.Rule.Inline);
  g.addRule("args_I1_I0_star", [new Nonterm("args_I1_I0_star"), new Nonterm("args_I1_I0")], E.Rule.ListSnoc("args_I1_I0_star", "args_I1_I0", true));
  g.addRule("args_I1_I0", [new Nonterm("list-arg-elt")], E.Rule.Inline)
  g.addRule("list-arg-elt", [new Nonterm("binding"), new Token("COMMA")])
  g.addRule("return-ann", [new Nonterm("return-ann_I0_opt")])
  g.addRule("return-ann_I0_opt", [], E.Rule.Inline);
  g.addRule("return-ann_I0_opt", [new Nonterm("return-ann_I0")], E.Rule.Inline);
  g.addRule("return-ann_I0", [new Token("THINARROW"), new Nonterm("ann")], E.Rule.Inline)
  g.addRule("doc-string", [new Nonterm("doc-string_I0_opt")])
  g.addRule("doc-string_I0_opt", [], E.Rule.Inline);
  g.addRule("doc-string_I0_opt", [new Nonterm("doc-string_I0")], E.Rule.Inline);
  g.addRule("doc-string_I0", [new Token("DOC"), new Token("STRING")], E.Rule.Inline)
  g.addRule("where-clause", [new Nonterm("where-clause_I0_opt")])
  g.addRule("where-clause_I0_opt", [], E.Rule.Inline);
  g.addRule("where-clause_I0_opt", [new Nonterm("where-clause_I0")], E.Rule.Inline);
  g.addRule("where-clause_I0", [new Token("WHERE"), new Nonterm("block")], E.Rule.Inline)
  g.addRule("check-expr", [new Token("CHECK"), new Token("STRING"), new Token("COLON"), new Nonterm("block"), new Nonterm("end")])
  g.addRule("check-expr", [new Nonterm("check-expr_I0"), new Nonterm("block"), new Nonterm("end")])
  g.addRule("check-expr_I0", [new Token("CHECKCOLON")], E.Rule.Inline)
  g.addRule("check-expr_I0", [new Token("EXAMPLESCOLON")], E.Rule.Inline)
  g.addRule("check-test", [new Nonterm("binop-expr"), new Nonterm("check-op"), new Nonterm("check-test_A0_I2_opt"), new Nonterm("binop-expr")])
  g.addRule("check-test_A0_I2_opt", [], E.Rule.Inline);
  g.addRule("check-test_A0_I2_opt", [new Nonterm("check-test_A0_I2")], E.Rule.Inline);
  g.addRule("check-test_A0_I2", [new Token("PERCENT"), new Nonterm("check-test_A0_I2_I1"), new Nonterm("binop-expr"), new Token("RPAREN")], E.Rule.Inline)
  g.addRule("check-test_A0_I2_I1", [new Token("PARENSPACE")], E.Rule.Inline)
  g.addRule("check-test_A0_I2_I1", [new Token("PARENNOSPACE")], E.Rule.Inline)
  g.addRule("check-test", [new Nonterm("binop-expr"), new Nonterm("check-op-postfix")])
  g.addRule("check-test", [new Nonterm("binop-expr")])
  g.addRule("data-expr", [new Token("DATA"), new Token("NAME"), new Nonterm("ty-params"), new Token("COLON"), new Nonterm("data-expr_I4_opt"), new Nonterm("data-expr_I5_star"), new Nonterm("data-sharing"), new Nonterm("where-clause"), new Nonterm("end")])
  g.addRule("data-expr_I4_opt", [], E.Rule.Inline);
  g.addRule("data-expr_I4_opt", [new Nonterm("data-expr_I4")], E.Rule.Inline);
  g.addRule("data-expr_I4", [new Nonterm("first-data-variant")], E.Rule.Inline)
  g.addRule("data-expr_I5_star", [], E.Rule.Inline);
  g.addRule("data-expr_I5_star", [new Nonterm("data-expr_I5_star"), new Nonterm("data-expr_I5")], E.Rule.ListSnoc("data-expr_I5_star", "data-expr_I5", true));
  g.addRule("data-expr_I5", [new Nonterm("data-variant")], E.Rule.Inline)
  g.addRule("variant-constructor", [new Token("NAME"), new Nonterm("variant-members")])
  g.addRule("first-data-variant", [new Nonterm("variant-constructor"), new Nonterm("data-with")])
  g.addRule("first-data-variant", [new Token("NAME"), new Nonterm("data-with")])
  g.addRule("data-variant", [new Token("BAR"), new Nonterm("variant-constructor"), new Nonterm("data-with")])
  g.addRule("data-variant", [new Token("BAR"), new Token("NAME"), new Nonterm("data-with")])
  g.addRule("variant-members", [new Token("PARENNOSPACE"), new Nonterm("variant-members_I1_opt"), new Token("RPAREN")])
  g.addRule("variant-members_I1_opt", [], E.Rule.Inline);
  g.addRule("variant-members_I1_opt", [new Nonterm("variant-members_I1")], E.Rule.Inline);
  g.addRule("variant-members_I1", [new Nonterm("variant-members_I1_I0_star"), new Nonterm("variant-member")], E.Rule.Inline)
  g.addRule("variant-members_I1_I0_star", [], E.Rule.Inline);
  g.addRule("variant-members_I1_I0_star", [new Nonterm("variant-members_I1_I0_star"), new Nonterm("variant-members_I1_I0")], E.Rule.ListSnoc("variant-members_I1_I0_star", "variant-members_I1_I0", true));
  g.addRule("variant-members_I1_I0", [new Nonterm("list-variant-member")], E.Rule.Inline)
  g.addRule("list-variant-member", [new Nonterm("variant-member"), new Token("COMMA")])
  g.addRule("variant-member", [new Nonterm("variant-member_I0_opt"), new Nonterm("binding")])
  g.addRule("variant-member_I0_opt", [], E.Rule.Inline);
  g.addRule("variant-member_I0_opt", [new Nonterm("variant-member_I0")], E.Rule.Inline);
  g.addRule("variant-member_I0", [new Token("REF")], E.Rule.Inline)
  g.addRule("data-with", [new Nonterm("data-with_I0_opt")])
  g.addRule("data-with_I0_opt", [], E.Rule.Inline);
  g.addRule("data-with_I0_opt", [new Nonterm("data-with_I0")], E.Rule.Inline);
  g.addRule("data-with_I0", [new Token("WITH"), new Nonterm("fields")], E.Rule.Inline)
  g.addRule("data-sharing", [new Nonterm("data-sharing_I0_opt")])
  g.addRule("data-sharing_I0_opt", [], E.Rule.Inline);
  g.addRule("data-sharing_I0_opt", [new Nonterm("data-sharing_I0")], E.Rule.Inline);
  g.addRule("data-sharing_I0", [new Token("SHARING"), new Nonterm("fields")], E.Rule.Inline)
  g.addRule("var-expr", [new Token("VAR"), new Nonterm("toplevel-binding"), new Token("EQUALS"), new Nonterm("binop-expr")])
  g.addRule("rec-expr", [new Token("REC"), new Nonterm("toplevel-binding"), new Token("EQUALS"), new Nonterm("binop-expr")])
  g.addRule("assign-expr", [new Token("NAME"), new Token("COLONEQUALS"), new Nonterm("binop-expr")])
  g.addRule("when-expr", [new Token("WHEN"), new Nonterm("binop-expr"), new Token("COLON"), new Nonterm("block"), new Nonterm("end")])
  g.addRule("binop-expr", [new Nonterm("expr"), new Nonterm("binop-expr_I1_star")])
  g.addRule("binop-expr_I1_star", [], E.Rule.Inline);
  g.addRule("binop-expr_I1_star", [new Nonterm("binop-expr_I1_star"), new Nonterm("binop-expr_I1")], E.Rule.ListSnoc("binop-expr_I1_star", "binop-expr_I1", true));
  g.addRule("binop-expr_I1", [new Nonterm("binop"), new Nonterm("expr")], E.Rule.Inline)
  g.addRule("binop", [new Token("PLUS")])
  g.addRule("binop", [new Token("DASH")])
  g.addRule("binop", [new Token("STAR")])
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
  g.addRule("expr", [new Nonterm("dot-expr")])
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
  g.addRule("bad-expr", [new Token("UNTERMINATED-STRING")])
  g.addRule("bad-expr", [new Token("UNTERMINATED-BLOCK-COMMENT")])
  g.addRule("bad-expr", [new Token("BAD-OPER")])
  g.addRule("bad-expr", [new Token("BAD-NUMBER")])
  g.addRule("bad-expr", [new Token("UNKNOWN")])
  g.addRule("paren-expr", [new Token("PARENSPACE"), new Nonterm("binop-expr"), new Token("RPAREN")])
  g.addRule("id-expr", [new Token("NAME")])
  g.addRule("prim-expr", [new Nonterm("num-expr")])
  g.addRule("prim-expr", [new Nonterm("frac-expr")])
  g.addRule("prim-expr", [new Nonterm("bool-expr")])
  g.addRule("prim-expr", [new Nonterm("string-expr")])
  g.addRule("num-expr", [new Token("NUMBER")])
  g.addRule("frac-expr", [new Token("RATIONAL")])
  g.addRule("bool-expr", [new Token("TRUE")])
  g.addRule("bool-expr", [new Token("FALSE")])
  g.addRule("string-expr", [new Token("STRING")])
  g.addRule("lambda-expr", [new Token("LAM"), new Nonterm("fun-header"), new Token("COLON"), new Nonterm("doc-string"), new Nonterm("block"), new Nonterm("where-clause"), new Nonterm("end")])
  g.addRule("method-expr", [new Token("METHOD"), new Nonterm("fun-header"), new Token("COLON"), new Nonterm("doc-string"), new Nonterm("block"), new Nonterm("where-clause"), new Nonterm("end")])
  g.addRule("binop-expr-commas", [new Nonterm("binop-expr"), new Token("COMMA")])
  g.addRule("app-expr", [new Nonterm("expr"), new Nonterm("app-args")])
  g.addRule("app-args", [new Token("PARENNOSPACE"), new Nonterm("app-args_I1_opt"), new Token("RPAREN")])
  g.addRule("app-args_I1_opt", [], E.Rule.Inline);
  g.addRule("app-args_I1_opt", [new Nonterm("app-args_I1")], E.Rule.Inline);
  g.addRule("app-args_I1", [new Nonterm("app-args_I1_I0_star"), new Nonterm("binop-expr")], E.Rule.Inline)
  g.addRule("app-args_I1_I0_star", [], E.Rule.Inline);
  g.addRule("app-args_I1_I0_star", [new Nonterm("app-args_I1_I0_star"), new Nonterm("app-args_I1_I0")], E.Rule.ListSnoc("app-args_I1_I0_star", "app-args_I1_I0", true));
  g.addRule("app-args_I1_I0", [new Nonterm("binop-expr-commas")], E.Rule.Inline)
  g.addRule("inst-expr", [new Nonterm("expr"), new Token("LANGLE"), new Nonterm("inst-expr_I2_star"), new Nonterm("ann"), new Token("RANGLE")])
  g.addRule("inst-expr_I2_star", [], E.Rule.Inline);
  g.addRule("inst-expr_I2_star", [new Nonterm("inst-expr_I2_star"), new Nonterm("inst-expr_I2")], E.Rule.ListSnoc("inst-expr_I2_star", "inst-expr_I2", true));
  g.addRule("inst-expr_I2", [new Nonterm("inst-elt")], E.Rule.Inline)
  g.addRule("inst-elt", [new Nonterm("ann"), new Token("COMMA")])
  g.addRule("obj-expr", [new Token("LBRACE"), new Nonterm("obj-fields"), new Token("RBRACE")])
  g.addRule("obj-expr", [new Token("LBRACE"), new Token("RBRACE")])
  g.addRule("obj-fields", [new Nonterm("obj-fields_I0_star"), new Nonterm("obj-field"), new Nonterm("obj-fields_I2_opt")])
  g.addRule("obj-fields_I0_star", [], E.Rule.Inline);
  g.addRule("obj-fields_I0_star", [new Nonterm("obj-fields_I0_star"), new Nonterm("obj-fields_I0")], E.Rule.ListSnoc("obj-fields_I0_star", "obj-fields_I0", true));
  g.addRule("obj-fields_I0", [new Nonterm("list-obj-field")], E.Rule.Inline)
  g.addRule("obj-fields_I2_opt", [], E.Rule.Inline);
  g.addRule("obj-fields_I2_opt", [new Nonterm("obj-fields_I2")], E.Rule.Inline);
  g.addRule("obj-fields_I2", [new Token("COMMA")], E.Rule.Inline)
  g.addRule("list-obj-field", [new Nonterm("obj-field"), new Token("COMMA")])
  g.addRule("obj-field", [new Nonterm("key"), new Token("COLON"), new Nonterm("binop-expr")])
  g.addRule("obj-field", [new Token("REF"), new Nonterm("key"), new Nonterm("obj-field_A1_I2_opt"), new Token("COLON"), new Nonterm("binop-expr")])
  g.addRule("obj-field_A1_I2_opt", [], E.Rule.Inline);
  g.addRule("obj-field_A1_I2_opt", [new Nonterm("obj-field_A1_I2")], E.Rule.Inline);
  g.addRule("obj-field_A1_I2", [new Token("COLONCOLON"), new Nonterm("ann")], E.Rule.Inline)
  g.addRule("obj-field", [new Nonterm("key"), new Nonterm("fun-header"), new Token("COLON"), new Nonterm("doc-string"), new Nonterm("block"), new Nonterm("where-clause"), new Nonterm("end")])
  g.addRule("fields", [new Nonterm("fields_I0_star"), new Nonterm("field"), new Nonterm("fields_I2_opt")])
  g.addRule("fields_I0_star", [], E.Rule.Inline);
  g.addRule("fields_I0_star", [new Nonterm("fields_I0_star"), new Nonterm("fields_I0")], E.Rule.ListSnoc("fields_I0_star", "fields_I0", true));
  g.addRule("fields_I0", [new Nonterm("list-field")], E.Rule.Inline)
  g.addRule("fields_I2_opt", [], E.Rule.Inline);
  g.addRule("fields_I2_opt", [new Nonterm("fields_I2")], E.Rule.Inline);
  g.addRule("fields_I2", [new Token("COMMA")], E.Rule.Inline)
  g.addRule("list-field", [new Nonterm("field"), new Token("COMMA")])
  g.addRule("field", [new Nonterm("key"), new Token("COLON"), new Nonterm("binop-expr")])
  g.addRule("field", [new Nonterm("key"), new Nonterm("fun-header"), new Token("COLON"), new Nonterm("doc-string"), new Nonterm("block"), new Nonterm("where-clause"), new Nonterm("end")])
  g.addRule("key", [new Token("NAME")])
  g.addRule("construct-expr", [new Token("LBRACK"), new Nonterm("construct-modifier"), new Nonterm("binop-expr"), new Token("COLON"), new Nonterm("construct-expr_I4_opt"), new Token("RBRACK")])
  g.addRule("construct-expr_I4_opt", [], E.Rule.Inline);
  g.addRule("construct-expr_I4_opt", [new Nonterm("construct-expr_I4")], E.Rule.Inline);
  g.addRule("construct-expr_I4", [new Nonterm("construct-expr_I4_I0_star"), new Nonterm("binop-expr")], E.Rule.Inline)
  g.addRule("construct-expr_I4_I0_star", [], E.Rule.Inline);
  g.addRule("construct-expr_I4_I0_star", [new Nonterm("construct-expr_I4_I0_star"), new Nonterm("construct-expr_I4_I0")], E.Rule.ListSnoc("construct-expr_I4_I0_star", "construct-expr_I4_I0", true));
  g.addRule("construct-expr_I4_I0", [new Nonterm("binop-expr-commas")], E.Rule.Inline)
  g.addRule("construct-modifier", [])
  g.addRule("construct-modifier", [new Token("LAZY")])
  g.addRule("dot-expr", [new Nonterm("expr"), new Token("DOT"), new Token("NAME")])
  g.addRule("bracket-expr", [new Nonterm("expr"), new Token("DOT"), new Token("LBRACK"), new Nonterm("binop-expr"), new Token("RBRACK")])
  g.addRule("get-bang-expr", [new Nonterm("expr"), new Token("BANG"), new Token("NAME")])
  g.addRule("extend-expr", [new Nonterm("expr"), new Token("DOT"), new Token("LBRACE"), new Nonterm("fields"), new Token("RBRACE")])
  g.addRule("update-expr", [new Nonterm("expr"), new Token("BANG"), new Token("LBRACE"), new Nonterm("fields"), new Token("RBRACE")])
  g.addRule("if-expr", [new Token("IF"), new Nonterm("binop-expr"), new Token("COLON"), new Nonterm("block"), new Nonterm("if-expr_I4_star"), new Nonterm("if-expr_I5_opt"), new Nonterm("end")])
  g.addRule("if-expr_I4_star", [], E.Rule.Inline);
  g.addRule("if-expr_I4_star", [new Nonterm("if-expr_I4_star"), new Nonterm("if-expr_I4")], E.Rule.ListSnoc("if-expr_I4_star", "if-expr_I4", true));
  g.addRule("if-expr_I4", [new Nonterm("else-if")], E.Rule.Inline)
  g.addRule("if-expr_I5_opt", [], E.Rule.Inline);
  g.addRule("if-expr_I5_opt", [new Nonterm("if-expr_I5")], E.Rule.Inline);
  g.addRule("if-expr_I5", [new Token("ELSECOLON"), new Nonterm("block")], E.Rule.Inline)
  g.addRule("else-if", [new Token("ELSEIF"), new Nonterm("binop-expr"), new Token("COLON"), new Nonterm("block")])
  g.addRule("if-pipe-expr", [new Token("ASKCOLON"), new Nonterm("if-pipe-expr_I1_star"), new Nonterm("if-pipe-expr_I2_opt"), new Nonterm("end")])
  g.addRule("if-pipe-expr_I1_star", [], E.Rule.Inline);
  g.addRule("if-pipe-expr_I1_star", [new Nonterm("if-pipe-expr_I1_star"), new Nonterm("if-pipe-expr_I1")], E.Rule.ListSnoc("if-pipe-expr_I1_star", "if-pipe-expr_I1", true));
  g.addRule("if-pipe-expr_I1", [new Nonterm("if-pipe-branch")], E.Rule.Inline)
  g.addRule("if-pipe-expr_I2_opt", [], E.Rule.Inline);
  g.addRule("if-pipe-expr_I2_opt", [new Nonterm("if-pipe-expr_I2")], E.Rule.Inline);
  g.addRule("if-pipe-expr_I2", [new Token("BAR"), new Token("OTHERWISECOLON"), new Nonterm("block")], E.Rule.Inline)
  g.addRule("if-pipe-branch", [new Token("BAR"), new Nonterm("binop-expr"), new Token("THENCOLON"), new Nonterm("block")])
  g.addRule("cases-binding", [new Nonterm("cases-binding_I0_opt"), new Nonterm("binding")])
  g.addRule("cases-binding_I0_opt", [], E.Rule.Inline);
  g.addRule("cases-binding_I0_opt", [new Nonterm("cases-binding_I0")], E.Rule.Inline);
  g.addRule("cases-binding_I0", [new Token("REF")], E.Rule.Inline)
  g.addRule("list-cases-arg-elt", [new Nonterm("cases-binding"), new Token("COMMA")])
  g.addRule("cases-args", [new Token("PARENNOSPACE"), new Nonterm("cases-args_I1_opt"), new Token("RPAREN")])
  g.addRule("cases-args_I1_opt", [], E.Rule.Inline);
  g.addRule("cases-args_I1_opt", [new Nonterm("cases-args_I1")], E.Rule.Inline);
  g.addRule("cases-args_I1", [new Nonterm("cases-args_I1_I0_star"), new Nonterm("cases-binding")], E.Rule.Inline)
  g.addRule("cases-args_I1_I0_star", [], E.Rule.Inline);
  g.addRule("cases-args_I1_I0_star", [new Nonterm("cases-args_I1_I0_star"), new Nonterm("cases-args_I1_I0")], E.Rule.ListSnoc("cases-args_I1_I0_star", "cases-args_I1_I0", true));
  g.addRule("cases-args_I1_I0", [new Nonterm("list-cases-arg-elt")], E.Rule.Inline)
  g.addRule("cases-expr", [new Token("CASES"), new Nonterm("cases-expr_I1"), new Nonterm("ann"), new Token("RPAREN"), new Nonterm("binop-expr"), new Token("COLON"), new Nonterm("cases-expr_I6_star"), new Nonterm("cases-expr_I7_opt"), new Nonterm("end")])
  g.addRule("cases-expr_I1", [new Token("PARENSPACE")], E.Rule.Inline)
  g.addRule("cases-expr_I1", [new Token("PARENNOSPACE")], E.Rule.Inline)
  g.addRule("cases-expr_I6_star", [], E.Rule.Inline);
  g.addRule("cases-expr_I6_star", [new Nonterm("cases-expr_I6_star"), new Nonterm("cases-expr_I6")], E.Rule.ListSnoc("cases-expr_I6_star", "cases-expr_I6", true));
  g.addRule("cases-expr_I6", [new Nonterm("cases-branch")], E.Rule.Inline)
  g.addRule("cases-expr_I7_opt", [], E.Rule.Inline);
  g.addRule("cases-expr_I7_opt", [new Nonterm("cases-expr_I7")], E.Rule.Inline);
  g.addRule("cases-expr_I7", [new Token("BAR"), new Token("ELSE"), new Token("THICKARROW"), new Nonterm("block")], E.Rule.Inline)
  g.addRule("cases-branch", [new Token("BAR"), new Token("NAME"), new Nonterm("cases-branch_I2_opt"), new Token("THICKARROW"), new Nonterm("block")])
  g.addRule("cases-branch_I2_opt", [], E.Rule.Inline);
  g.addRule("cases-branch_I2_opt", [new Nonterm("cases-branch_I2")], E.Rule.Inline);
  g.addRule("cases-branch_I2", [new Nonterm("cases-args")], E.Rule.Inline)
  g.addRule("for-bind", [new Nonterm("binding"), new Token("FROM"), new Nonterm("binop-expr")])
  g.addRule("for-bind-elt", [new Nonterm("for-bind"), new Token("COMMA")])
  g.addRule("for-expr", [new Token("FOR"), new Nonterm("expr"), new Token("PARENNOSPACE"), new Nonterm("for-expr_I3_opt"), new Token("RPAREN"), new Nonterm("return-ann"), new Token("COLON"), new Nonterm("block"), new Nonterm("end")])
  g.addRule("for-expr_I3_opt", [], E.Rule.Inline);
  g.addRule("for-expr_I3_opt", [new Nonterm("for-expr_I3")], E.Rule.Inline);
  g.addRule("for-expr_I3", [new Nonterm("for-expr_I3_I0_star"), new Nonterm("for-bind")], E.Rule.Inline)
  g.addRule("for-expr_I3_I0_star", [], E.Rule.Inline);
  g.addRule("for-expr_I3_I0_star", [new Nonterm("for-expr_I3_I0_star"), new Nonterm("for-expr_I3_I0")], E.Rule.ListSnoc("for-expr_I3_I0_star", "for-expr_I3_I0", true));
  g.addRule("for-expr_I3_I0", [new Nonterm("for-bind-elt")], E.Rule.Inline)
  g.addRule("user-block-expr", [new Token("BLOCK"), new Nonterm("block"), new Nonterm("end")])
  g.addRule("ann", [new Nonterm("name-ann")])
  g.addRule("ann", [new Nonterm("record-ann")])
  g.addRule("ann", [new Nonterm("arrow-ann")])
  g.addRule("ann", [new Nonterm("app-ann")])
  g.addRule("ann", [new Nonterm("pred-ann")])
  g.addRule("ann", [new Nonterm("dot-ann")])
  g.addRule("name-ann", [new Token("NAME")])
  g.addRule("record-ann", [new Token("LBRACE"), new Nonterm("record-ann_I1_opt"), new Token("RBRACE")])
  g.addRule("record-ann_I1_opt", [], E.Rule.Inline);
  g.addRule("record-ann_I1_opt", [new Nonterm("record-ann_I1")], E.Rule.Inline);
  g.addRule("record-ann_I1", [new Nonterm("record-ann_I1_I0_star"), new Nonterm("ann-field")], E.Rule.Inline)
  g.addRule("record-ann_I1_I0_star", [], E.Rule.Inline);
  g.addRule("record-ann_I1_I0_star", [new Nonterm("record-ann_I1_I0_star"), new Nonterm("record-ann_I1_I0")], E.Rule.ListSnoc("record-ann_I1_I0_star", "record-ann_I1_I0", true));
  g.addRule("record-ann_I1_I0", [new Nonterm("list-ann-field")], E.Rule.Inline)
  g.addRule("list-ann-field", [new Nonterm("ann-field"), new Token("COMMA")])
  g.addRule("ann-field", [new Token("NAME"), new Token("COLONCOLON"), new Nonterm("ann")])
  g.addRule("noparen-arrow-ann", [new Nonterm("noparen-arrow-ann_I0_opt"), new Token("THINARROW"), new Nonterm("ann")])
  g.addRule("noparen-arrow-ann_I0_opt", [], E.Rule.Inline);
  g.addRule("noparen-arrow-ann_I0_opt", [new Nonterm("noparen-arrow-ann_I0")], E.Rule.Inline);
  g.addRule("noparen-arrow-ann_I0", [new Nonterm("noparen-arrow-ann_I0_I0_star"), new Nonterm("ann")], E.Rule.Inline)
  g.addRule("noparen-arrow-ann_I0_I0_star", [], E.Rule.Inline);
  g.addRule("noparen-arrow-ann_I0_I0_star", [new Nonterm("noparen-arrow-ann_I0_I0_star"), new Nonterm("noparen-arrow-ann_I0_I0")], E.Rule.ListSnoc("noparen-arrow-ann_I0_I0_star", "noparen-arrow-ann_I0_I0", true));
  g.addRule("noparen-arrow-ann_I0_I0", [new Nonterm("arrow-ann-elt")], E.Rule.Inline)
  g.addRule("arrow-ann", [new Nonterm("arrow-ann_I0"), new Nonterm("arrow-ann_I1_opt"), new Token("THINARROW"), new Nonterm("ann"), new Token("RPAREN")])
  g.addRule("arrow-ann_I0", [new Token("PARENSPACE")], E.Rule.Inline)
  g.addRule("arrow-ann_I0", [new Token("PARENNOSPACE")], E.Rule.Inline)
  g.addRule("arrow-ann_I1_opt", [], E.Rule.Inline);
  g.addRule("arrow-ann_I1_opt", [new Nonterm("arrow-ann_I1")], E.Rule.Inline);
  g.addRule("arrow-ann_I1", [new Nonterm("arrow-ann_I1_I0_star"), new Nonterm("ann")], E.Rule.Inline)
  g.addRule("arrow-ann_I1_I0_star", [], E.Rule.Inline);
  g.addRule("arrow-ann_I1_I0_star", [new Nonterm("arrow-ann_I1_I0_star"), new Nonterm("arrow-ann_I1_I0")], E.Rule.ListSnoc("arrow-ann_I1_I0_star", "arrow-ann_I1_I0", true));
  g.addRule("arrow-ann_I1_I0", [new Nonterm("arrow-ann-elt")], E.Rule.Inline)
  g.addRule("arrow-ann-elt", [new Nonterm("ann"), new Token("COMMA")])
  g.addRule("app-ann", [new Nonterm("app-ann_I0"), new Token("LANGLE"), new Nonterm("app-ann_I2_star"), new Nonterm("ann"), new Token("RANGLE")])
  g.addRule("app-ann_I0", [new Nonterm("name-ann")], E.Rule.Inline)
  g.addRule("app-ann_I0", [new Nonterm("dot-ann")], E.Rule.Inline)
  g.addRule("app-ann_I2_star", [], E.Rule.Inline);
  g.addRule("app-ann_I2_star", [new Nonterm("app-ann_I2_star"), new Nonterm("app-ann_I2")], E.Rule.ListSnoc("app-ann_I2_star", "app-ann_I2", true));
  g.addRule("app-ann_I2", [new Nonterm("app-ann-elt")], E.Rule.Inline)
  g.addRule("app-ann-elt", [new Nonterm("ann"), new Token("COMMA")])
  g.addRule("pred-ann", [new Nonterm("ann"), new Token("PERCENT"), new Nonterm("pred-ann_I2"), new Nonterm("id-expr"), new Token("RPAREN")])
  g.addRule("pred-ann_I2", [new Token("PARENSPACE")], E.Rule.Inline)
  g.addRule("pred-ann_I2", [new Token("PARENNOSPACE")], E.Rule.Inline)
  g.addRule("dot-ann", [new Token("NAME"), new Token("DOT"), new Token("NAME")])

  g.initializeParser(true);
  var cycles = g.checkForCycles();
  if (cycles === false) {
    console.log("Non-cyclic grammar -- all good!");
  } else {
    console.log("Grammar has " + cycles.length + " cycles!");
    for (var i = 0; i < cycles.length; i++)
      console.log(cycles[i]);
  }
  var g_json = JSON.stringify(g.toSerializable(), null, '  ');
  var filename = process.argv[2];
  var out = fs.createWriteStream(filename);
  out.write("define(['../../../lib/jglr/jglr'],\n");
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
