#lang pyret

provide *
provide-types *
import ast as A
import srcloc as SL
import error-display as ED
import string-dict as SD

type StringDict = SD.StringDict
string-dict = SD.string-dict

type Loc = SL.Srcloc

data PyretDialect:
  | Pyret
  | Bootstrap
end

data Dependency:
  | dependency(protocol :: String, arguments :: List<String>)
    with:
    key(self): self.protocol + "(" + self.arguments.join-str(", ") + ")" end
  | builtin(modname :: String)
    with:
    key(self): "builtin(" + self.modname + ")" end
end

# Used to describe when additional module imports should be added to a
# program.  See wrap-extra-imports
data ExtraImports:
  | extra-imports(imports :: List<ExtraImport>)
end

# Import this module, and bind the given value and type bindings from it
data ExtraImport:
  | extra-import(dependency :: Dependency, as-name :: String, values :: List<String>, types :: List<String>)
end

data CompileEnvironment:
  | compile-env(
        globals :: Globals,
        mods :: StringDict<Provides> # map from dependency key to info provided from module
      )
end

data Globals:
  | globals(values :: StringDict<ValInfo>, types :: StringDict<TypeInfo>)
end

data ValInfo:
  | v-just-there
end

data TypeInfo:
  | t-just-there
end

data Provides:
  | provides(
      values :: StringDict<ValInfo>,
      types :: StringDict<TypeInfo>
      )
end

data CompileResult<C>:
  | ok(code :: C)
  | err(problems :: List<CompileError>)
end

fun draw-and-highlight(l):
  ED.loc-display(l, "error-highlight", ED.loc(l))
end

data CompileError:
  | wf-err(msg :: String, loc :: A.Loc) with:
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Well-formedness:"),
          ED.text(self.msg),
          ED.text("at")],
        draw-and-highlight(self.loc)]
    end
  | wf-err-split(msg :: String, loc :: List<A.Loc>) with:
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Well-formedness:"),
          ED.text(self.msg),
          ED.text("at")],
        ED.v-sequence(self.loc.map(lam(l): [ED.para: draw-and-highlight(l)] end))]
    end
  | reserved-name(loc :: Loc, id :: String) with:
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Well-formedness: Pyret disallows the use of"),
          ED.code(ED.text(self.id)),
          ED.text("as an identifier")],
        draw-and-highlight(self.loc)]
    end
  | zero-fraction(loc, numerator) with:
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Well-formedness: fraction literal with zero denominator (numerator was"),
          ED.val(self.numerator),
          ED.text(") at")],
        draw-and-highlight(self.loc)]
    end
  | underscore-as-expr(l :: Loc) with:
    render-reason(self):
      [ED.error: 
        [ED.para: ED.text("Underscore used as an expression, which is not allowed, at ")],
        draw-and-highlight(self.l)]
    end
  | underscore-as-ann(l :: Loc) with:
    render-reason(self):
      [ED.error:
        [ED.para: ED.text("Underscore used as an annotation, which is not allowed at ")],
        draw-and-highlight(self.l)]
    end
  | unbound-id(id :: A.Expr) with:
    render-reason(self):
      cases(SL.Srcloc) self.id.l:
        | builtin(_) =>
          [ED.para:
            ED.text("ERROR: should not be allowed to have a builtin that's unbound:"),
            ED.text(self.id.id.toname()),
            draw-and-highlight(self.id.l)]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("The name"), ED.code(ED.text(self.id.id.toname())), ED.text("is used but not defined at")],
            draw-and-highlight(self.id.l)]
      end
    end
  | unbound-var(id :: String, loc :: Loc) with:
    render-reason(self):
      cases(SL.Srcloc) self.loc:
        | builtin(_) =>
          [ED.para:
            ED.text("ERROR: should not be allowed to have a builtin that's unbound:"),
            ED.text(self.id),
            draw-and-highlight(self.id.l)]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("The variable"), ED.code(ED.text(self.id)), ED.text("is assigned to, but not defined, at")],
            draw-and-highlight(self.loc)]
      end
    end
  | unbound-type-id(ann :: A.Ann) with:
    render-reason(self):
      cases(SL.Srcloc) self.ann.l:
        | builtin(_) =>
          [ED.para:
            ED.text("ERROR: should not be allowed to have a builtin that's unbound:"),
            ED.text(self.ann.tosource().pretty(1000)),
            draw-and-highlight(self.id.l)]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("The name"), ED.code(ED.text(self.ann.id.toname())),
              ED.text("is used as a type but not defined as one, at")],
            draw-and-highlight(self.ann.l)]
      end
    end
  | unexpected-type-var(loc :: Loc, name :: A.Name) with:
    render-reason(self):
      #### TODO ###
      ED.text("Identifier " + tostring(self.name) + " is used in a dot-annotation at " + tostring(self.loc) + ", but is bound as a type variable")
    end
  | pointless-var(loc :: Loc) with:
    render-reason(self):
      cases(SL.Srcloc) self.loc:
        | builtin(_) =>
          [ED.para:
            ED.text("ERROR: should not be allowed to have a builtin that's anonymous:"),
            draw-and-highlight(self.loc)]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("Defining an anonymous variable is pointless: there is no name to modify."),
              ED.text("Either give this expression a name, or bind it to an identifier rather than a variable.")],
            draw-and-highlight(self.loc)]
      end
    end
  | pointless-rec(loc :: Loc) with:
    render-reason(self):
      cases(SL.Srcloc) self.loc:
        | builtin(_) =>
          [ED.para:
            ED.text("ERROR: should not be allowed to have a builtin that's anonymous:"),
            draw-and-highlight(self.loc)]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("Defining an anonymous recursive identifier is pointless: there is no name to call recursively."),
              ED.text("Either give this expression a name, or remove the rec annotation.")],
            draw-and-highlight(self.loc)]
      end
    end
  | pointless-shadow(loc :: Loc) with:
    render-reason(self):
      cases(SL.Srcloc) self.loc:
        | builtin(_) =>
          [ED.para:
            ED.text("ERROR: should not be allowed to have a builtin that's anonymous:"),
            draw-and-highlight(self.loc)]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("Anonymous identifier cannot shadow anything: there is no name to shadow."),
              ED.text("Either give this expression a name, or remove the shadow annotation.")],
            draw-and-highlight(self.loc)]
      end
    end
  | bad-assignment(id :: String, loc :: Loc, prev-loc :: Loc) with:
    render-reason(self):
      cases(SL.Srcloc) self.prev-loc:
        | builtin(_) =>
          [ED.error:
            [ED.para:
              ED.text("The name"), ED.code(ED.text(self.id)), ED.text("is defined as an identifier,"),
              ED.text("but it is assigned as if it were a variable at"),
              draw-and-highlight(self.loc)]]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("The name"), ED.code(ED.text(self.id)), ED.text("is defined as an identifier,"),
              ED.text("but it is assigned as if it were a variable at"),
              draw-and-highlight(self.loc)],
            [ED.para:
              ED.text("One possible fix is to change the declaration of"), ED.code(ED.text(self.id)),
              ED.text("to use"), ED.code(ED.text("var")), ED.text("at"), draw-and-highlight(self.prev-loc)]]
      end
    end
  | mixed-id-var(id :: String, var-loc :: Loc, id-loc :: Loc) with:
    #### TODO ###
    render-reason(self):
      ED.text(self.id + " is declared as both a variable (at " + tostring(self.var-loc) + ")"
          + " and an identifier (at " + self.id-loc.format(not(self.var-loc.same-file(self.id-loc))) + ")")
    end
  | shadow-id(id :: String, new-loc :: Loc, old-loc :: Loc) with:
    render-reason(self):
      cases(SL.Srcloc) self.old-loc:
        | builtin(_) =>
          [ED.error:
            [ED.para:
              ED.text("The name"), ED.code(ED.text(self.id)), ED.text("is already defined."),
              ED.text("You need to pick a different name for"), ED.code(ED.text(self.id)), ED.text("at"),
              draw-and-highlight(self.new-loc)]]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("It looks like you've defined the name"), ED.code(ED.text(self.id)),
              ED.text("twice, at")],
            draw-and-highlight(self.old-loc),
            draw-and-highlight(self.new-loc),
            [ED.para: ED.text("You need to pick a different name for one of them.")]]
      end
    end
  | duplicate-id(id :: String, new-loc :: Loc, old-loc :: Loc) with:
    render-reason(self):
      cases(SL.Srcloc) self.old-loc:
        | builtin(_) =>
          [ED.error:
            [ED.para:
              ED.text("The name"), ED.code(ED.text(self.id)), ED.text("is already defined."),
              ED.text("You need to pick a different name for"), ED.code(ED.text(self.id)), ED.text("at"),
              draw-and-highlight(self.new-loc)]]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("It looks like you've defined the name"), ED.code(ED.text(self.id)),
              ED.text("twice, at")],
            draw-and-highlight(self.old-loc),
            draw-and-highlight(self.new-loc),
            [ED.para: ED.text("You need to pick a different name for one of them.")]]
      end
    end
  | duplicate-field(id :: String, new-loc :: Loc, old-loc :: Loc) with:
    render-reason(self):
      cases(SL.Srcloc) self.old-loc:
        | builtin(_) =>
          [ED.error:
            [ED.para:
              ED.text("The field name"), ED.code(ED.text(self.id)), ED.text("is already defined."),
              ED.text("You need to pick a different name for"), ED.code(ED.text(self.id)), ED.text("at"),
              draw-and-highlight(self.new-loc)]]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("It looks like you've defined the field name"), ED.code(ED.text(self.id)),
              ED.text("twice, at")],
            draw-and-highlight(self.old-loc),
            draw-and-highlight(self.new-loc),
            [ED.para: ED.text("You need to pick a different name for one of them.")]]
      end
    end
  | incorrect-type(bad-name :: String, bad-loc :: A.Loc, expected-name :: String, expected-loc :: A.Loc) with:
    #### TODO ###
    render-reason(self):
      ED.text("Expected to find " + self.expected-name + " (declared at " + tostring(self.expected-loc)
        + ") on line " + tostring(self.bad-loc) + ", but instead found " + self.bad-name + ".")
    end
  | bad-type-instantiation(wanted :: Number, given :: Number, loc :: A.Loc) with:
    #### TODO ###
    render-reason(self):
      ED.text("Expected to receive " + tostring(self.wanted) + " arguments for type instantiation "
        + " on line " + tostring(self.loc) + ", but instead received " + tostring(self.given) + ".")
    end
  | incorrect-number-of-args(loc :: A.Loc) with:
    #### TODO ###
    render-reason(self):
      ED.text("Incorrect number of arguments given to function at line " + tostring(self.loc) + ".")
    end
  | apply-non-function(loc :: A.Loc) with:
    #### TODO ###
    render-reason(self):
      ED.text("The program tried to apply something that is not a function at line " + tostring(self.loc) + ".")
    end
  | object-missing-field(field-name :: String, obj :: String, obj-loc :: A.Loc, access-loc :: A.Loc) with:
    #### TODO ###
    render-reason(self):
      ED.text("The object type " + self.obj
        + " (defined at " + tostring(self.obj-loc)
        + ") does not have the field \"" + self.field-name
        + "\" (accessed at line " + tostring(self.access-loc) + ").")
    end
  | unneccesary-branch(branch-name :: String, branch-loc :: A.Loc, type-name :: String, type-loc :: A.Loc) with:
    #### TODO ###
    render-reason(self):
      ED.text("The branch " + self.branch-name
        + " (defined at " + tostring(self.branch-loc)
        + ") is not a variant of " + self.type-name
        + " (declared at " + tostring(self.type-loc) + ")")
    end
  | unneccesary-else-branch(type-name :: String, loc :: A.Loc) with:
    #### TODO ###
    render-reason(self):
      ED.text("The else branch for the cases expression at " + tostring(self.loc)
        + " is not needed since all variants of " + self.type-name + " have been exhausted.")
    end
  | non-exhaustive-pattern(missing :: List<String>, type-name :: String, loc :: A.Loc) with:
    #### TODO ###
    render-reason(self):
      ED.text("The cases expression at " + tostring(self.loc)
        + " does not exhaust all variants of " + self.type-name
        + ". It is missing: " + self.missing.join-str(", ") + ".")
    end
  | cant-match-on(type-name :: String, loc :: A.Loc) with:
    #### TODO ###
    render-reason(self):
      ED.text("The type specified " + self.type-name
        + " at " + tostring(self.loc)
        + " cannot be used in a cases expression.")
    end
  | incorrect-number-of-bindings(variant-name :: String, loc :: A.Loc, given :: Number, expected :: Number) with:
    #### TODO ###
    render-reason(self):
      ED.text("Incorrect number of bindings given to "
        + "the variant " + self.variant-name
        + " at " + tostring(self.loc) + ". "
        + "Given " + num-tostring(self.given)
        + ", but expected " + num-tostring(self.expected)
        + ".")
    end
  | cases-singleton-mismatch(name :: String, branch-loc :: A.Loc, should-be-singleton :: Boolean) with:
    render-reason(self):
      if self.should-be-singleton:
        [ED.error:
          [ED.para:
            ED.text("The cases branch named"), ED.code(ED.text(self.name)),
            ED.text("at"), draw-and-highlight(self.branch-loc),
            ED.text("has an argument list, but the variant is a singleton.")]]
      else:
        [ED.error:
          [ED.para:
            ED.text("The cases branch named"), ED.code(ED.text(self.name)),
            ED.text("at"), draw-and-highlight(self.branch-loc),
            ED.text("doesn't have an argument list, but the variant is not a singleton.")]]
      end
    end
  | given-parameters(data-type :: String, loc :: A.Loc) with:
    #### TODO ###
    render-reason(self):
      ED.text("The data type " + self.data-type
        + " does not take any parameters,"
        + " but is given some at " + tostring(self.loc)
        + ".")
    end
  | unable-to-instantiate(loc :: A.Loc) with:
    #### TODO ###
    render-reason(self):
      ED.text("There is not enough information to instantiate the type at " + tostring(self.loc)
         + ", or the arguments are incompatible. Please provide more information or do the type instantiation directly.")
    end
  | cant-typecheck(reason :: String) with:
    #### TODO ###
    render-reason(self):
      ED.text("This program cannot be type-checked. Please send it to the developers. " +
        "The reason that it cannot be type-checked is: " + self.reason)
    end
  | unsupported(message :: String, blame-loc :: A.Loc) with:
    #### TODO ###
    render-reason(self):
      ED.text(self.message + " (found at " + tostring(self.blame-loc) + ")")
    end
  | no-module(loc :: A.Loc, mod-name :: String) with:
    #### TODO ###
    render-reason(self):
      ED.text("There is no module imported with the name " + self.mod-name
        + " (used at " + tostring(self.loc) + ")")
    end
end

default-compile-options = {
  check-mode : true,
  type-check : false,
  allow-shadowed : false,
  collect-all: false,
  ignore-unbound: false
}

runtime-types = [string-dict:
  "Number", t-just-there,
  "String", t-just-there,
  "Function", t-just-there,
  "Boolean", t-just-there,
  "Object", t-just-there,
  "Method", t-just-there,
  "Nothing", t-just-there,
  "RawArray", t-just-there
]

runtime-builtins = [string-dict: 
  "test-print", v-just-there,
  "print", v-just-there,
  "display", v-just-there,
  "print-error", v-just-there,
  "display-error", v-just-there,
  "tostring", v-just-there,
  "torepr", v-just-there,
  "brander", v-just-there,
  "raise", v-just-there,
  "nothing", v-just-there,
  "builtins", v-just-there,
  "not", v-just-there,
  "is-nothing", v-just-there,
  "is-number", v-just-there,
  "is-string", v-just-there,
  "is-boolean", v-just-there,
  "is-object", v-just-there,
  "is-function", v-just-there,
  "is-raw-array", v-just-there,
  "gensym", v-just-there,
  "random", v-just-there,
  "run-task", v-just-there,
  "_plus", v-just-there,
  "_minus", v-just-there,
  "_times", v-just-there,
  "_divide", v-just-there,
  "_lessthan", v-just-there,
  "_lessequal", v-just-there,
  "_greaterthan", v-just-there,
  "_greaterequal", v-just-there,
  "string-equal", v-just-there,
  "string-contains", v-just-there,
  "string-append", v-just-there,
  "string-length", v-just-there,
  "string-tonumber", v-just-there,
  "string-to-number", v-just-there,
  "string-repeat", v-just-there,
  "string-substring", v-just-there,
  "string-replace", v-just-there,
  "string-split", v-just-there,
  "string-split-all", v-just-there,
  "string-char-at", v-just-there,
  "string-toupper", v-just-there,
  "string-tolower", v-just-there,
  "string-explode", v-just-there,
  "string-index-of", v-just-there,
  "string-to-code-point", v-just-there,
  "string-from-code-point", v-just-there,
  "string-to-code-points", v-just-there,
  "string-from-code-points", v-just-there,
  "num-random", v-just-there,
  "num-random-seed", v-just-there,
  "num-max", v-just-there,
  "num-min", v-just-there,
  "num-equal", v-just-there,
  "num-within", v-just-there,
  "num-abs", v-just-there,
  "num-sin", v-just-there,
  "num-cos", v-just-there,
  "num-tan", v-just-there,
  "num-asin", v-just-there,
  "num-acos", v-just-there,
  "num-atan", v-just-there,
  "num-modulo", v-just-there,
  "num-truncate", v-just-there,
  "num-sqrt", v-just-there,
  "num-sqr", v-just-there,
  "num-ceiling", v-just-there,
  "num-floor", v-just-there,
  "num-log", v-just-there,
  "num-exp", v-just-there,
  "num-exact", v-just-there,
  "num-is-integer", v-just-there,
  "num-is-fixnum", v-just-there,
  "num-expt", v-just-there,
  "num-tostring", v-just-there,
  "num-to-string", v-just-there,
  "num-to-string-digits", v-just-there,
  "raw-array-get", v-just-there,
  "raw-array-set", v-just-there,
  "raw-array-of", v-just-there,
  "raw-array-length", v-just-there,
  "raw-array-to-list", v-just-there,
  "raw-array-fold", v-just-there,
  "raw-array", v-just-there,
  "ref-get", v-just-there,
  "ref-set", v-just-there,
  "ref-freeze", v-just-there,
  "equal-always", v-just-there,
  "equal-always3", v-just-there,
  "equal-now", v-just-there,
  "equal-now3", v-just-there,
  "identical", v-just-there,
  "identical3", v-just-there,
  "exn-unwrap", v-just-there,
  "_empty", v-just-there,
  "_link", v-just-there
]

no-builtins = compile-env(globals([string-dict: ], [string-dict: ]), [string-dict:])

minimal-builtins = compile-env(globals(runtime-builtins, runtime-types), [string-dict:])

standard-globals = globals(runtime-builtins, runtime-types)
standard-builtins = compile-env(globals(runtime-builtins, runtime-types), [string-dict:])

minimal-imports = extra-imports(empty)

standard-imports = extra-imports(
   [list: 
      extra-import(builtin("arrays"), "arrays", [list: 
          "array",
          "build-array",
          "array-from-list",
          "is-array",
          "array-of",
          "array-set-now",
          "array-get-now",
          "array-length",
          "array-to-list-now"
        ],
        [list: "Array"]),
      extra-import(builtin("lists"), "lists", [list: 
          "list",
          "is-empty",
          "is-link",
          "empty",
          "link",
          "range",
          "range-by",
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
        ],
        [list: "List"]),
      extra-import(builtin("option"), "option", [list: 
          "Option",
          "is-none",
          "is-some",
          "none",
          "some"
        ],
        [list: "Option"]),
      extra-import(builtin("error"), "error", [list: ], [list:]),
      extra-import(builtin("sets"), "sets", [list: 
          "set",
          "tree-set",
          "list-set"
        ],
        [list: "Set"])
    ])

