#lang pyret

provide *
provide-types *
import ast as A
import srcloc as SL

type Loc = SL.Srcloc

data PyretDialect:
  | Pyret
  | Bootstrap
end

data CompileEnvironment:
  | compile-env(bindings :: List<CompileBinding>, types :: List<CompileTypeBinding>)
end

data CompileResult<C>:
  | ok(code :: C)
  | err(problems :: List<CompileError>)
end

data CompileError:
  | wf-err(msg :: String, loc :: A.Loc) with:
    tostring(self, shadow tostring): "well-formedness: " + self.msg + " at " + tostring(self.loc) end
  | wf-err-split(msg :: String, loc :: List<A.Loc>) with:
    tostring(self, shadow tostring): "well-formedness: " + self.msg + " at " + self.loc.map(tostring).join-str(", ") end
  | reserved-name(loc :: Loc, id :: String) with:
    tostring(self, shadow tostring):
      "well-formedness: cannot use " + self.id + " as an identifier at " + tostring(self.loc) end
  | zero-fraction(loc, numerator) with:
    tostring(self, shadow tostring):
      "well-formedness: fraction literal with zero denominator (numerator was " + tostring(self.numerator) + " at " + tostring(self.loc)
    end
  | underscore-as-expr(l :: Loc) with:
    tostring(self, shadow tostring):
      "Underscore used as an expression at " + tostring(self.l) + ", which is not allowed."
    end
  | underscore-as-ann(l :: Loc) with:
    tostring(self, shadow tostring):
      "Underscore used as an annotation at " + tostring(self.l) + ", which is not allowed."
    end
  | unbound-id(id :: A.Expr) with:
    tostring(self, shadow tostring):
      "Identifier " + tostring(self.id.id) + " is used at " + tostring(self.id.l) + ", but is not defined"
    end
  | unbound-var(id :: String, loc :: Loc) with:
    tostring(self, shadow tostring):
      "Assigning to unbound variable " + self.id + " at " + tostring(self.loc)
    end
  | unbound-type-id(ann :: A.Ann) with:
    tostring(self, shadow tostring):
      "Identifier " + self.ann.id.toname() + " is used as a type name at " + tostring(self.ann.l) + ", but is not defined as a type."
    end
  | unexpected-type-var(loc :: Loc, name :: A.Name) with:
    tostring(self, shadow tostring):
      "Identifier " + tostring(self.name) + " is used in a dot-annotation at " + tostring(self.loc) + ", but is bound as a type variable"
    end
  | pointless-var(loc :: Loc) with:
    tostring(self, shadow tostring):
      "The anonymous mutable variable at " + tostring(self.loc) + " can never be re-used"
    end
  | pointless-rec(loc :: Loc) with:
    tostring(self, shadow tostring):
      "The anonymous recursive identifier at " + tostring(self.loc) + " can never be re-used"
    end
  | pointless-shadow(loc :: Loc) with:
    tostring(self, shadow tostring):
      "The anonymous identifier at " + tostring(self.loc) + " can't actually shadow anything"
    end
  | bad-assignment(id :: String, loc :: Loc, prev-loc :: Loc) with:
    tostring(self, shadow tostring):
      "Identifier " + self.id + " is assigned at " + tostring(self.loc)
        + ", but its definition at " + self.prev-loc.format(not(self.loc.same-file(self.prev-loc)))
        + " is not assignable.  (Only names declared with var are assignable.)"
    end
  | mixed-id-var(id :: String, var-loc :: Loc, id-loc :: Loc) with:
    tostring(self, shadow tostring):
      self.id + " is declared as both a variable (at " + tostring(self.var-loc) + ")"
        + " and an identifier (at " + self.id-loc.format(not(self.var-loc.same-file(self.id-loc))) + ")"
    end
  | shadow-id(id :: String, new-loc :: Loc, old-loc :: Loc) with:
    tostring(self, shadow tostring):
      "Identifier " + self.id + " is declared at " + tostring(self.new-loc)
        + ", but is already declared at " + self.old-loc.format(not(self.new-loc.same-file(self.old-loc)))
    end
  | duplicate-id(id :: String, new-loc :: Loc, old-loc :: Loc) with:
    tostring(self, shadow tostring):
      "Identifier " + self.id + " is declared twice, at " + tostring(self.new-loc)
        + " and at " + self.old-loc.format(not(self.new-loc.same-file(self.old-loc)))
    end
  | duplicate-field(id :: String, new-loc :: Loc, old-loc :: Loc) with:
    tostring(self, shadow tostring):
      "The field " + self.id + " is declared twice, at " + tostring(self.new-loc)
        + " and at " + self.old-loc.format(not(self.new-loc.same-file(self.old-loc)))
    end
  | incorrect-type(bad-name :: String, bad-loc :: A.Loc, expected-name :: String, expected-loc :: A.Loc) with:
    tostring(self, shadow tostring):
      "Expected to find " + self.expected-name + " (declared at " + tostring(self.expected-loc)
        + ") on line " + tostring(self.bad-loc) + ", but instead found " + self.bad-name + "."
    end
  | bad-type-instantiation(wanted :: Number, given :: Number, loc :: A.Loc) with:
    tostring(self, shadow tostring):
      "Expected to receive " + tostring(self.wanted) + " arguments for type instantiation "
        + " on line " + tostring(self.loc) + ", but instead received " + tostring(self.given) + "."
    end
  | incorrect-number-of-args(loc :: A.Loc) with:
    tostring(self, shadow tostring):
      "Incorrect number of arguments given to function at line " + tostring(self.loc) + "."
    end
  | apply-non-function(loc :: A.Loc) with:
    tostring(self, shadow tostring):
      "You tried to apply something that is not a function at line " + tostring(self.loc) + "."
    end
  | object-missing-field(field-name :: String, obj :: String, obj-loc :: A.Loc, access-loc :: A.Loc) with:
    tostring(self, shadow tostring):
      "The object type " + self.obj
        + " (defined at " + tostring(self.obj-loc)
        + ") does not have the field \"" + self.field-name
        + "\" (accessed at line " + tostring(self.access-loc) + ")."
    end
  | unneccesary-branch(branch-name :: String, branch-loc :: A.Loc, type-name :: String, type-loc :: A.Loc) with:
    tostring(self, shadow tostring):
      "The branch " + self.branch-name
        + " (defined at " + tostring(self.branch-loc)
        + ") is not a variant of " + self.type-name
        + " (declared at " + tostring(self.type-loc) + ")"
    end
  | unneccesary-else-branch(type-name :: String, loc :: A.Loc) with:
    tostring(self, shadow tostring):
      "The else branch for the cases expression at " + tostring(self.loc)
        + " is not needed since all variants of " + self.type-name + " have been exhausted."
    end
  | non-exhaustive-pattern(missing :: List<String>, type-name :: String, loc :: A.Loc) with:
    tostring(self, shadow tostring):
      "The cases expression at " + tostring(self.loc)
        + " does not exhaust all variants of " + self.type-name
        + ". It is missing: " + self.missing.join-str(", ") + "."
    end
  | cant-match-on(type-name :: String, loc :: A.Loc) with:
    tostring(self, shadow tostring):
      "The type specified " + self.type-name
        + " at " + tostring(self.loc)
        + " cannot be used in a cases expression."
    end
  | incorrect-number-of-bindings(variant-name :: String, loc :: A.Loc, given :: Number, expected :: Number) with:
    tostring(self, shadow tostring):
      "Incorrect number of bindings given to "
        + "the variant " + self.variant-name
        + " at " + tostring(self.loc) + ". "
        + "Given " + num-tostring(self.given)
        + ", but expected " + num-tostring(self.expected)
        + "."
    end
  | cases-singleton-mismatch(branch-name :: String, branch-loc :: A.Loc, should-be-singleton :: Boolean) with:
    tostring(self, shadow tostring):
      if self.should-be-singleton:
        "The cases branch " + self.branch-name
          + " at " + self.branch-loc.format(true) + " expects to receive parameters, but the value being examined is a singleton"
      else:
        "The cases branch at " + self.branch-loc.format(true) + " expects the value being examined to be a singleton, but it actually has fields"
      end
    end
  | given-parameters(data-type :: String, loc :: A.Loc) with:
    tostring(self, shadow tostring):
      "The data type " + self.data-type
        + " does not take any parameters,"
        + " but is given some at " + tostring(self.loc)
        + "."
    end
  | unable-to-instantiate(loc :: A.Loc) with:
    tostring(self, shadow tostring):
      "There is not enough information to instantiate the type at " + tostring(self.loc)
         + ", or the arguments are incompatible. Please provide more information or do the type instantiation directly."
    end
  | cant-typecheck(reason :: String) with:
    tostring(self, shadow tostring):
      "This program cannot be type-checked. Please send it to the developers. " +
        "The reason that it cannot be type-checked is: " + self.reason
    end
  | unsupported(message :: String, blame-loc :: A.Loc) with:
    tostring(self, shadow tostring):
      self.message + " (found at " + tostring(self.blame-loc) + ")"
    end
  | no-module(loc :: A.Loc, mod-name :: String) with:
    tostring(self, shadow tostring):
      "There is no module imported with the name " + self.mod-name
        + " (used at " + tostring(self.loc) + ")"
    end
end

data CompileTypeBinding:
  | type-id(id :: String)
  | type-module-bindings(name :: String, bindings :: List<String>)
end

runtime-types = lists.map(type-id, [list:
  "Number",
  "String",
  "Function",
  "Boolean",
  "Object",
  "Method",
  "Nothing",
  "RawArray"
])

standard-types = runtime-types +
  [list:
    type-module-bindings("lists", [list: "List" ]),
    type-module-bindings("option", [list: "Option" ]),
    type-module-bindings("arrays", [list: "Array" ]),
    type-module-bindings("sets", [list: "Set"])
    #...
    ]

data CompileBinding:
  | builtin-id(id :: String)
  | module-bindings(name :: String, bindings :: List<String>)
end

runtime-builtins = lists.map(builtin-id, [list: 
  "test-print",
  "print",
  "display",
  "print-error",
  "display-error",
  "tostring",
  "torepr",
  "brander",
  "raise",
  "nothing",
  "builtins",
  "not",
  "is-nothing",
  "is-number",
  "is-string",
  "is-boolean",
  "is-object",
  "is-function",
  "is-raw-array",
  "gensym",
  "random",
  "run-task",
  "_plus",
  "_minus",
  "_times",
  "_divide",
  "_lessthan",
  "_lessequal",
  "_greaterthan",
  "_greaterequal",
  "strings-equal",
  "string-contains",
  "string-append",
  "string-length",
  "string-tonumber",
  "string-to-number",
  "string-repeat",
  "string-substring",
  "string-replace",
  "string-split",
  "string-split-all",
  "string-char-at",
  "string-toupper",
  "string-tolower",
  "string-explode",
  "string-index-of",
  "string-to-code-point",
  "string-from-code-point",
  "string-to-code-points",
  "string-from-code-points",
  "num-random",
  "num-random-seed",
  "num-max",
  "num-min",
  "nums-equal",
  "num-abs",
  "num-sin",
  "num-cos",
  "num-tan",
  "num-asin",
  "num-acos",
  "num-atan",
  "num-modulo",
  "num-truncate",
  "num-sqrt",
  "num-sqr",
  "num-ceiling",
  "num-floor",
  "num-log",
  "num-exp",
  "num-exact",
  "num-is-integer",
  "num-is-fixnum",
  "num-expt",
  "num-tostring",
  "num-to-string",
  "num-to-string-digits",
  "raw-array-get",
  "raw-array-set",
  "raw-array-of",
  "raw-array-length",
  "raw-array-to-list",
  "raw-array-fold",
  "raw-array",
  "ref-get",
  "ref-set",
  "ref-freeze",
  "equal-always",
  "equal-always3",
  "equal-now",
  "equal-now3",
  "identical",
  "identical3",
  "exn-unwrap"
])

no-builtins = compile-env([list: ], [list: ])

minimal-builtins = compile-env(runtime-builtins, runtime-types)

bootstrap-builtins = compile-env(
  [list: module-bindings("lists", [list: 
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
      "all",
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
      "fold-while",
      "fold",
      "fold2",
      "fold3",
      "fold4",
      "index"
  ])] +
  runtime-builtins + lists.map(builtin-id, [list: 
  

  "_link",
  "_empty",

  # new arithmetic aliases  
  "add",
  "sub",
  "div",
  "mul",
  "less",
  "greater",
  "greaterequal",
  "lessequal",

  "both",
  "either",
  "not",

  "max",
  "min",
  "abs",
  "sin",
  "cos",
  "tan",
  "asin",
  "acos",
  "atan",
  "modulo",
  "truncate",
  "sqrt",
  "sqr",
  "ceiling",
  "floor",
  "log",
  "exp",
  "exact",
  "is-integer",
  "is-fixnum",
  "expt",

  # from js/trove/image.js
  "circle",
  "is-image-color",
  "is-mode",
  "is-x-place",
  "is-y-place",
  "is-angle",
  "is-side-count",
  "is-step-count",
  "is-image",
  "bitmap-url",
  "open-image-url",
  "image-url",
  "images-equal",
  "text",
  "normal",
  "text-font",
  "overlay",
  "middle",
  "overlay-xy",
  "overlay-align",
  "underlay",
  "middle",
  "underlay-xy",
  "underlay-align",
  "beside-align",
  "beside",
 "above",
  "middle",
  "above-align",
  "empty-scene",
  "put-image",
  "place-image",
  "place-image-align",
  "rotate",
  "scale",
  "scale-xy",
  "flip-horizontal",
  "flip-vertical",
  "frame",
  "crop",
  "line",
  "add-line",
  "scene-line",
  "square",
  "rectangle",
  "regular-polygon",
  "ellipse",
  "triangle",
  "triangle-sas",
  "triangle-sss",
  "triangle-ass",
  "triangle-ssa",
  "triangle-aas",
  "triangle-asa",
  "triangle-saa",
  "right-triangle",
  "isosceles-triangle",
  "star",
  "star-sized",
  "radial-star",
  "star-polygon",
  "rhombus",
  "image-to-color-list",
  "color-list-to-image",
  "color-list-to-bitmap",
  "image-width",
  "image-height",
  "image-baseline",
  "name-to-color",

  # from js/trove/world.js
  "big-bang",
  "on-tick",
  "on-tick-n",
  "to-draw",
  "on-mouse",
  "on-key",
  "stop-when",
  "is-key-equal"
  ]),
  runtime-types
)

standard-builtins = compile-env(
    runtime-builtins + [list: 
      builtin-id("_link"),
      builtin-id("_empty"),
      module-bindings("arrays", [list: 
          "array",
          "build-array",
          "array-from-list",
          "is-array",
          "array-of",
          "array-set-now",
          "array-get-now",
          "array-length",
          "array-to-list-now"
        ]),
      module-bindings("lists", [list: 
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
        ]),
      module-bindings("option", [list: 
          "Option",
          "is-none",
          "is-some",
          "none",
          "some"
        ]),
      module-bindings("error", [list: ]),
      module-bindings("sets", [list: 
          "set",
          "tree-set",
          "list-set"
        ])
    ],
    standard-types
    )

