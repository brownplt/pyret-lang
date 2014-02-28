#lang pyret

provide *
import ast as A

data CompileEnvironment:
  | compile-env(bindings :: List<CompileBinding>)
end

data CompileResult<C>:
  | ok(code :: C)
  | err(problems :: List<CompileError>)
end

data CompileError:
  | wf-err(msg :: String, loc :: A.Loc) with:
    tostring(self): "well-formedness: " + self.msg + " at " + tostring(self.loc) end
  | wf-err-split(msg :: String, loc :: List<A.Loc>) with:
    tostring(self): "well-formedness: " + self.msg + " at " + self.loc.map(tostring).join-str(", ") end
  | unbound-id(id :: A.Expr) with:
    tostring(self):
      "Identifier " + self.id.id.tostring() + " is used at " + self.id.l.tostring() + ", but is not defined"
    end
  | unbound-var(id :: String, loc :: Loc) with:
    tostring(self):
      "Assigning to unbound variable " + self.id + " at " + self.loc.tostring()
    end
  | pointless-var(loc :: Loc) with:
    tostring(self):
      "The anonymous mutable variable at " + self.loc.tostring() + " can never be re-used"
    end
  | pointless-shadow(loc :: Loc) with:
    tostring(self):
      "The anonymous identifier at " + self.loc.tostring() + " can't actually shadow anything"
    end
  | bad-assignment(id :: String, loc :: Loc, prev-loc :: Loc) with:
    tostring(self):
      "Identifier " + self.id + " is assigned at " + self.loc.tostring()
        + ", but its definition at " + self.prev-loc.tostring() + " is not assignable."
        + "  (Only names declared with var are assignable.)"
    end
  | mixed-id-var(id :: String, var-loc :: Loc, id-loc :: Loc) with:
    tostring(self):
      self.id + " is declared as both a variable (at " + self.var-loc.tostring() + ")"
        + " and an identifier (at " + self.id-loc.tostring() + ")"
    end
  | shadow-id(id :: String, new-loc :: Loc, old-loc :: Loc) with:
    tostring(self):
      "Identifier " + self.id + " is declared at " + self.new-loc.tostring()
        + ", but is already declared at " + self.old-loc.tostring()
    end
  | duplicate-id(id :: String, new-loc :: Loc, old-loc :: Loc) with:
    tostring(self):
      "Identifier " + self.id + " is declared twice, at " + self.new-loc.tostring()
        + " and at " + self.old-loc.tostring()
    end
end

data CompileBinding:
  | builtin-id(id :: String)
  | module-bindings(name :: String, bindings :: List<String>)
end

runtime-builtins = [
    builtin-id("test-print"),
    builtin-id("print"),
    builtin-id("display"),
    builtin-id("print-error"),
    builtin-id("display-error"),
    builtin-id("tostring"),
    builtin-id("torepr"),
    builtin-id("brander"),
    builtin-id("raise"),
    builtin-id("nothing"),
    builtin-id("builtins"),
    builtin-id("is-nothing"),
    builtin-id("is-number"),
    builtin-id("is-string"),
    builtin-id("is-boolean"),
    builtin-id("is-object"),
    builtin-id("is-function"),
    builtin-id("gensym")
  ]

no-builtins = compile-env([])

minimal-builtins = compile-env(runtime-builtins)

standard-builtins = compile-env(
    runtime-builtins + [
      module-bindings("list", [
          "is-empty",
          "is-link",
          "empty",
          "link",
          "range",
          "repeat",
          "filter",
          "partition",
          "split-at",
          "any",
          "find",
          "map",
          "map2",
          "map3",
          "map4",
          "map_n",
          "map2_n",
          "map3_n",
          "map4_n",
          "each",
          "each2",
          "each3",
          "each4",
          "each_n",
          "each2_n",
          "each3_n",
          "each4_n",
          "fold",
          "fold2",
          "fold3",
          "fold4",
          "index"
          ]),
      module-bindings("option", [
          "Option",
          "is-none",
          "is-some",
          "none",
          "some"
          ]),
      module-bindings("error", []),
      module-bindings("sets", [
          "set",
          "tree-set",
          "list-set"
        ])
    ])
