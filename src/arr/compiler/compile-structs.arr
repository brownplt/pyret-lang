#lang pyret

provide *
provide-types *
import ast as A
import srcloc as SL
import error-display as ED

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
        ED.v-sequence(self.loc.map(draw-and-highlight), ", ")]
    end
  | reserved-name(loc :: Loc, id :: String) with:
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Well-formedness: Pyret disallows the use of"),
          ED.text(self.id),
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
    #### TODO ###
    render-reason(self):
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
    render-reason(self):
      ED.text("Expected to find " + self.expected-name + " (declared at " + tostring(self.expected-loc)
        + ") on line " + tostring(self.bad-loc) + ", but instead found " + self.bad-name + ".")
    end
  | bad-type-instantiation(wanted :: Number, given :: Number, loc :: A.Loc) with:
    render-reason(self):
      ED.text("Expected to receive " + tostring(self.wanted) + " arguments for type instantiation "
        + " on line " + tostring(self.loc) + ", but instead received " + tostring(self.given) + ".")
    end
  | incorrect-number-of-args(loc :: A.Loc) with:
    render-reason(self):
      ED.text("Incorrect number of arguments given to function at line " + tostring(self.loc) + ".")
    end
  | apply-non-function(loc :: A.Loc) with:
    render-reason(self):
      ED.text("The program tried to apply something that is not a function at line " + tostring(self.loc) + ".")
    end
  | object-missing-field(field-name :: String, obj :: String, obj-loc :: A.Loc, access-loc :: A.Loc) with:
    render-reason(self):
      ED.text("The object type " + self.obj
        + " (defined at " + tostring(self.obj-loc)
        + ") does not have the field \"" + self.field-name
        + "\" (accessed at line " + tostring(self.access-loc) + ").")
    end
  | unneccesary-branch(branch-name :: String, branch-loc :: A.Loc, type-name :: String, type-loc :: A.Loc) with:
    render-reason(self):
      ED.text("The branch " + self.branch-name
        + " (defined at " + tostring(self.branch-loc)
        + ") is not a variant of " + self.type-name
        + " (declared at " + tostring(self.type-loc) + ")")
    end
  | unneccesary-else-branch(type-name :: String, loc :: A.Loc) with:
    render-reason(self):
      ED.text("The else branch for the cases expression at " + tostring(self.loc)
        + " is not needed since all variants of " + self.type-name + " have been exhausted.")
    end
  | non-exhaustive-pattern(missing :: List<String>, type-name :: String, loc :: A.Loc) with:
    render-reason(self):
      ED.text("The cases expression at " + tostring(self.loc)
        + " does not exhaust all variants of " + self.type-name
        + ". It is missing: " + self.missing.join-str(", ") + ".")
    end
  | cant-match-on(type-name :: String, loc :: A.Loc) with:
    render-reason(self):
      ED.text("The type specified " + self.type-name
        + " at " + tostring(self.loc)
        + " cannot be used in a cases expression.")
    end
  | incorrect-number-of-bindings(variant-name :: String, loc :: A.Loc, given :: Number, expected :: Number) with:
    render-reason(self):
      ED.text("Incorrect number of bindings given to "
        + "the variant " + self.variant-name
        + " at " + tostring(self.loc) + ". "
        + "Given " + num-tostring(self.given)
        + ", but expected " + num-tostring(self.expected)
        + ".")
    end
  | cases-singleton-mismatch(branch-loc :: A.Loc, should-be-singleton :: Boolean) with:
    render-reason(self):
      if self.should-be-singleton:
        [ED.error:
          ED.text("The cases branch at"), draw-and-highlight(self.branch-loc),
          ED.text("has an argument list, but the variant is a singleton.")]
      else:
        [ED.error:
          ED.text("The cases branch at"), draw-and-highlight(self.branch-loc),
          ED.text("doesn't have an argument list, but the variant is not a singleton.")]
      end
    end
  | given-parameters(data-type :: String, loc :: A.Loc) with:
    render-reason(self):
      ED.text("The data type " + self.data-type
        + " does not take any parameters,"
        + " but is given some at " + tostring(self.loc)
        + ".")
    end
  | unable-to-instantiate(loc :: A.Loc) with:
    render-reason(self):
      ED.text("There is not enough information to instantiate the type at " + tostring(self.loc)
         + ", or the arguments are incompatible. Please provide more information or do the type instantiation directly.")
    end
  | cant-typecheck(reason :: String) with:
    render-reason(self):
      ED.text("This program cannot be type-checked. Please send it to the developers. " +
        "The reason that it cannot be type-checked is: " + self.reason)
    end
  | unsupported(message :: String, blame-loc :: A.Loc) with:
    render-reason(self):
      ED.text(self.message + " (found at " + tostring(self.blame-loc) + ")")
    end
  | no-module(loc :: A.Loc, mod-name :: String) with:
    render-reason(self):
      ED.text("There is no module imported with the name " + self.mod-name
        + " (used at " + tostring(self.loc) + ")")
    end
end

data CompileTypeBinding:
  | type-id(id :: String)
  | type-module-bindings(name :: String, bindings :: List<String>)
end

runtime-types = lists.map(type-id, [list:
  "Number",
  "Exactnum",
  "Roughnum",
  "NumInteger",
  "NumRational",
  "NumPositive",
  "NumNegative",
  "NumNonPositive",
  "NumNonNegative",
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
  "string-equal",
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
  "num-equal",
  "num-within",
  "num-within-abs",
  "num-within-rel",
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
  "num-round",
  "num-round-even",
  "num-log",
  "num-exp",
  "num-exact",
  "num-to-rational",
  "num-to-roughnum",
  "num-to-fixnum",
  "num-is-integer",
  "num-is-rational",
  "num-is-roughnum",
  "num-is-positive",
  "num-is-negative",
  "num-is-non-positive",
  "num-is-non-negative",
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
  "within-abs-now",
  "within-abs",
  "within-now",
  "within-rel-now",
  "within",
  "within-rel",
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
