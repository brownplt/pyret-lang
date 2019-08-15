provide *
provide-types *

import ast as A
import string-dict as SD
import srcloc as SL
import file("type-structs.arr") as TS

module-uri                = TS.module-uri
dependency                = TS.dependency
local                     = TS.local

type Type                 = TS.Type
t-name                    = TS.t-name(_, _, A.dummy-loc, false)
t-var                     = TS.t-var(_, A.dummy-loc, false)
t-arrow                   = TS.t-arrow(_, _, A.dummy-loc, false)
t-top                     = TS.t-top(A.dummy-loc, false)
t-bot                     = TS.t-bot(A.dummy-loc, false)
t-app                     = TS.t-app(_, _, A.dummy-loc, false)
t-record                  = TS.t-record(_, A.dummy-loc, false)
t-forall                  = TS.t-forall(_, _, A.dummy-loc, false)
t-data-refinement         = TS.t-data-refinement(_, _, A.dummy-loc, false)
t-tuple                   = TS.t-tuple(_, A.dummy-loc, false)

t-data                    = TS.t-data(_, A.d-all, _, _, _, A.dummy-loc)

t-number                  = TS.t-number(A.dummy-loc)
t-string                  = TS.t-string(A.dummy-loc)
t-boolean                 = TS.t-boolean(A.dummy-loc)
t-array                   = TS.t-array(_, A.dummy-loc)
t-nothing                 = TS.t-nothing(A.dummy-loc)
t-srcloc                  = TS.t-srcloc(A.dummy-loc)
t-table                   = TS.t-table(A.dummy-loc)
t-array-name              = TS.t-array-name

type ModuleType           = TS.ModuleType
t-module                  = TS.t-module

type TypeVariant          = TS.TypeVariant
t-variant                 = TS.t-variant(_, _, _, A.dummy-loc)
t-singleton-variant       = TS.t-singleton-variant(_, _, A.dummy-loc)

string-dict = SD.string-dict

s-atom                    = A.s-atom

tva = t-var(A.global-names.make-atom("A"))
tvb = t-var(A.global-names.make-atom("B"))
tvc = t-var(A.global-names.make-atom("C"))
tvd = t-var(A.global-names.make-atom("D"))
tve = t-var(A.global-names.make-atom("E"))
tvf = t-var(A.global-names.make-atom("F"))
tvg = t-var(A.global-names.make-atom("G"))
tvh = t-var(A.global-names.make-atom("H"))

t-image = t-name(module-uri("builtin://image"), A.s-type-global("Image"))
t-option = t-name(module-uri("builtin://option"), A.s-type-global("Option"))
t-reactor = t-name(module-uri("builtin://reactors"), A.s-type-global("Reactor"))
t-equality-result = t-name(module-uri("builtin://equality"), A.s-type-global("EqualityResult"))
t-value-skeleton = t-name(module-uri("builtin://valueskeleton"), A.s-type-global("ValueSkeleton"))
t-list = t-name(module-uri("builtin://lists"), A.s-type-global("List"))
t-big-array = t-name(module-uri("builtin://arrays"), A.s-type-global("Array"))
t-set = t-name(module-uri("builtin://sets"), A.s-type-global("Set"))
t-runtime-error = t-name(module-uri("builtin://error"), A.s-type-global("RuntimeError"))
t-parse-error = t-name(module-uri("builtin://error"), A.s-type-global("ParseError"))
t-either = t-name(module-uri("builtin://either"), A.s-type-global("Either"))
t-s-exp = t-name(module-uri("builtin://s-exp-structs"), A.s-type-global("S-Exp"))
t-pick = t-name(module-uri("builtin://pick"), A.s-type-global("Pick"))
t-json = t-name(module-uri("builtin://json-structs"), A.s-type-global("JSON"))
t-string-dict = t-name(module-uri("builtin://string-dict"), A.s-type-global("StringDict"))

fun t-either-app(typ-1 :: Type, typ-2 :: Type):
  t-app(t-either, [list: typ-1, typ-2])
end

t-option-app = lam(param :: Type):
  t-app(t-option, [list: param])
end

t-reactor-app = lam(param :: Type):
  t-app(t-reactor, [list: param])
end

fun t-list-app(a :: Type) -> Type:
  t-app(t-list, [list: a])
end

fun t-big-array-app(typ :: Type):
  t-app(t-big-array, [list: typ])
end

fun t-set-app(typ :: Type):
  t-app(t-set, [list: typ])
end

fun t-pick-app(typ1 :: Type, typ2 :: Type):
  t-app(t-pick, [list: typ1, typ2])
end

fun t-string-dict-app(typ :: Type):
  t-app(t-string-dict, [list: typ])
end

t-output = t-arrow([list: ], t-value-skeleton)
t-number-binop = t-arrow([list: t-number, t-number], t-number)


fun make-default-aliases():
  default-aliases = [SD.string-dict: ]
  default-aliases
end

fun make-default-types() block:
  default-typs = SD.make-mutable-string-dict()

  # Need to be fixed to correct type:
  default-typs.set-now("makeSome", t-forall([list: tva], t-arrow([list: tva], t-option-app(tva))))
  default-typs.set-now("makeNone", t-forall([list: tva], t-arrow([list: ], t-option-app(tva))))
  default-typs.set-now("checkWrapBoolean", t-arrow([list: t-boolean], t-boolean))
  default-typs.set-now("checkTupleBind", t-arrow([list: t-top, t-number, t-srcloc], t-bot))
  default-typs.set-now("throwNonBooleanCondition", t-arrow([list: t-srcloc, t-string, t-top], t-bot))
  default-typs.set-now("throwNoBranchesMatched", t-arrow([list: t-srcloc, t-string], t-bot))
  default-typs.set-now("throwUnfinishedTemplate", t-arrow([list: t-srcloc], t-bot))
  default-typs.set-now("makeReactor", t-forall([list: tva], t-arrow([list:
      tva,
      t-record([string-dict:
        "on-tick", t-option-app(t-arrow([list: tva], tva)),
        "on-mouse", t-option-app(t-arrow([list: tva, t-number, t-number, t-string], tva)),
        "on-key", t-option-app(t-arrow([list: tva, t-string], tva)),
        "to-draw", t-option-app(t-arrow([list: tva], t-image)),
        "stop-when", t-option-app(t-arrow([list: tva], t-boolean)),
        "seconds-per-tick", t-option-app(t-number),
        "close-when-stop", t-option-app(t-boolean),
        "title", t-option-app(t-string)])],
    t-reactor-app(tva))))
  default-typs.set-now("hasField", t-arrow([list: t-record([string-dict: ]), t-string], t-boolean))
  default-typs.set-now("makeSrcloc", t-arrow([list: t-srcloc], t-bot))

  default-typs.set-now("not", t-arrow([list: t-boolean], t-boolean))
  default-typs.set-now("equal-always", t-arrow([list: t-top, t-top], t-boolean))
  default-typs.set-now("equal-now", t-arrow([list: t-top, t-top], t-boolean))
  default-typs.set-now("identical", t-arrow([list: t-top, t-top], t-boolean))
  default-typs.set-now("equal-always3", t-arrow([list: t-top, t-top], t-equality-result))
  default-typs.set-now("equal-now3", t-arrow([list: t-top, t-top], t-equality-result))
  default-typs.set-now("identical3", t-arrow([list: t-top, t-top], t-equality-result))

  default-typs.set-now("getMaker", t-forall([list: tva, tvb], t-arrow([list: t-record([string-dict: "make", t-arrow([list: t-array(tvb)], tva)]), t-string, t-srcloc, t-srcloc], t-arrow([list: t-array(tvb)], tva))))
  default-typs.set-now("getLazyMaker", t-forall([list: tva, tvb], t-arrow([list: t-record([string-dict: "lazy-make", t-arrow([list: t-array(t-arrow([list: ], tvb))], tva)]), t-string, t-srcloc, t-srcloc], t-arrow([list: t-array(t-arrow([list: ], tvb))], tva))))
  default-typs.set-now("getMaker0", t-forall([list: tva], t-arrow([list: t-record([string-dict: "make0", t-arrow([list: ], tva)]), t-string, t-srcloc, t-srcloc], t-arrow([list: ], tva))))
  default-typs.set-now("getMaker1", t-forall([list: tva, tvb], t-arrow([list: t-record([string-dict: "make1", t-arrow([list: tvb], tva)]), t-string, t-srcloc, t-srcloc], t-arrow([list: tvb], tva))))
  default-typs.set-now("getMaker2", t-forall([list: tva, tvb, tvc], t-arrow([list: t-record([string-dict: "make2", t-arrow([list: tvb, tvc], tva)]), t-string, t-srcloc, t-srcloc], t-arrow([list: tvb, tvc], tva))))
  default-typs.set-now("getMaker3", t-forall([list: tva, tvb, tvc, tvd], t-arrow([list: t-record([string-dict: "make3", t-arrow([list: tvb, tvc, tvd], tva)]), t-string, t-srcloc, t-srcloc], t-arrow([list: tvb, tvc, tvd], tva))))
  default-typs.set-now("getMaker4", t-forall([list: tva, tvb, tvc, tvd, tve], t-arrow([list: t-record([string-dict: "make4", t-arrow([list: tvb, tvc, tvd, tve], tva)]), t-string, t-srcloc, t-srcloc], t-arrow([list: tvb, tvc, tvd, tve], tva))))
  default-typs.set-now("getMaker5", t-forall([list: tva, tvb, tvc, tvd, tve, tvf], t-arrow([list: t-record([string-dict: "make5", t-arrow([list: tvb, tvc, tvd, tve, tvf], tva)]), t-string, t-srcloc, t-srcloc], t-arrow([list: tvb, tvc, tvd, tve, tvf], tva))))

  default-typs.set-now("makeTable", t-arrow([list: t-array(t-top), t-array(t-array(t-top))], t-table))

  default-typs.freeze()
end

fun make-default-data-exprs() block:
  default-data-exprs = SD.make-string-dict()
  default-data-exprs
end

# Functions for adding hard-coded modules
module-const-equality = t-module("builtin://equality",
  t-record([string-dict:
    "EqualityResult", t-arrow([list: t-top], t-boolean),
    "is-EqualityResult", t-arrow([list: t-top], t-boolean),
    "Equal", t-data-refinement(t-equality-result, "Equal"),
    "is-Equal", t-arrow([list: t-top], t-boolean),
    "NotEqual", t-arrow([list: t-string, t-top, t-top], t-data-refinement(t-equality-result, "NotEqual")),
    "is-NotEqual", t-arrow([list: t-top], t-boolean),
    "Unknown", t-arrow([list: t-string, t-top, t-top], t-data-refinement(t-equality-result, "Unknown")),
    "is-Unknown", t-arrow([list: t-top], t-boolean),
    "equal-and", t-arrow([list: t-equality-result, t-equality-result], t-equality-result),
    "equal-or", t-arrow([list: t-equality-result, t-equality-result], t-equality-result),
    "to-boolean", t-arrow([list: t-equality-result], t-boolean)
  ]),
  SD.make-string-dict()
    .set("EqualityResult", t-data(
      "EqualityResult",
      [list: ],
      [list:
        t-singleton-variant("Equal", [string-dict: ]),
        t-variant("NotEqual", [list: {"reason"; t-string}, {"value1"; t-top}, {"value2"; t-top}], [string-dict: ]),
        t-variant("Unknown", [list: {"reason"; t-string}, {"value1"; t-top}, {"value2"; t-top}], [string-dict: ])],
      [string-dict: ])
    ),
  SD.make-string-dict()
    .set("EqualityResult", t-equality-result)
)

module-const-arrays = t-module("builtin://arrays",
  t-record([string-dict:
    "array", t-record([string-dict:
      "make", t-forall([list: tva], t-arrow([list: t-array(tva)], t-big-array-app(tva))),
      "make0", t-forall([list: tva], t-arrow([list: ], t-big-array-app(tva))),
      "make1", t-forall([list: tva], t-arrow([list: tva], t-big-array-app(tva))),
      "make2", t-forall([list: tva], t-arrow([list: tva, tva], t-big-array-app(tva))),
      "make3", t-forall([list: tva], t-arrow([list: tva, tva, tva], t-big-array-app(tva))),
      "make4", t-forall([list: tva], t-arrow([list: tva, tva, tva, tva], t-big-array-app(tva))),
      "make5", t-forall([list: tva], t-arrow([list: tva, tva, tva, tva, tva], t-big-array-app(tva)))
    ]),
    "build-array", t-forall([list: tva], t-arrow([list: t-arrow([list: t-number], tva), t-number], t-big-array-app(tva))),
    "array-from-list", t-forall([list: tva], t-arrow([list: t-list-app(tva)], t-big-array-app(tva))),
    "is-array", t-forall([list: tva], t-arrow([list: t-top], t-boolean)),
    "array-of", t-forall([list: tva], t-arrow([list: tva, t-number], t-big-array-app(tva))),
    "array-set-now", t-forall([list: tva], t-arrow([list: t-big-array-app(tva), t-number, tva], t-nothing)),
    "array-get-now", t-forall([list: tva], t-arrow([list: t-big-array-app(tva), t-number], tva)),
    "array-length", t-forall([list: tva], t-arrow([list: t-big-array-app(tva)], t-number)),
    "array-to-list-now", t-forall([list: tva], t-arrow([list: t-big-array-app(tva)], t-list-app(tva)))
  ]),
  SD.make-string-dict()
    .set("Array", t-data(
        "Array",
        [list: tva],
        [list: ],
        [string-dict:
            "get-now", t-arrow([list: t-number], tva),
            "set-now", t-arrow([list: t-number, tva], t-nothing),
            "to-list-now", t-arrow(empty, t-list-app(tva)),
            "length", t-arrow(empty, t-number),
            "_output", t-output
        ])),
  SD.make-string-dict()
    .set("Array", t-big-array)
    .set("List", t-list)
)

module-const-pick = t-module("builtin://pick",
  t-record([string-dict:
    "Pick", t-arrow([list: t-top], t-boolean),
    "is-Pick", t-arrow([list: t-top], t-boolean),
    "pick-none", t-forall([list: tva, tvb], t-data-refinement(t-pick-app(tva, tvb), "pick-none")),
    "is-pick-none", t-arrow([list: t-top], t-boolean),
    "pick-some", t-forall([list: tva, tvb], t-arrow([list: tva, tvb], t-data-refinement(t-pick-app(tva, tvb), "pick-some"))),
    "is-pick-some", t-arrow([list: t-top], t-boolean)
  ]),
  SD.make-string-dict()
    .set("Pick", t-data(
        "Pick",
        [list: tva, tvb],
        [list:
          t-singleton-variant("pick-none",
            [string-dict:
              "_match", t-top,
            ]
          ),
          t-variant("pick-some",
            [list: {"elt"; tva}, {"rest"; tvb}],
            [string-dict:
              "_match", t-top,
            ]
          )
        ],
        [string-dict:
          "_match", t-top
      ])),
  SD.make-string-dict()
    .set("Pick", t-name(module-uri("builtin://pick"), A.s-type-global("Pick")))
)

set-constructor =
  t-record([string-dict:
      "make", t-forall([list: tva], t-arrow([list: t-array(tva)], t-set-app(tva))),
      "make0", t-forall([list: tva], t-arrow([list: ], t-set-app(tva))),
      "make1", t-forall([list: tva], t-arrow([list: tva], t-set-app(tva))),
      "make2", t-forall([list: tva], t-arrow([list: tva, tva], t-set-app(tva))),
      "make3", t-forall([list: tva], t-arrow([list: tva, tva, tva], t-set-app(tva))),
      "make4", t-forall([list: tva], t-arrow([list: tva, tva, tva, tva], t-set-app(tva))),
      "make5", t-forall([list: tva], t-arrow([list: tva, tva, tva, tva, tva], t-set-app(tva)))
    ])

t-empty-set = t-forall([list: tva], t-set-app(tva))

t-list-to-set = t-forall([list: tva], t-arrow([list: t-list-app(tva)], t-set-app(tva)))

module-const-sets = t-module("builtin://sets",
  t-record([string-dict:
    "set", set-constructor,
    "list-set", set-constructor,
    "tree-set", set-constructor,
    "empty-set", t-empty-set,
    "empty-list-set", t-empty-set,
    "empty-tree-set", t-empty-set,
    "list-to-set", t-list-to-set,
    "list-to-list-set", t-list-to-set,
    "list-to-tree-set", t-list-to-set,
    "fold", t-forall([list: tva, tvb], t-arrow([list: t-arrow([list: tvb, tva], tvb), tvb, t-set-app(tva)], tvb)),
    "all", t-forall([list: tva], t-arrow([list: t-arrow([list: tva], t-boolean), t-set-app(tva)], t-boolean)),
    "any", t-forall([list: tva], t-arrow([list: t-arrow([list: tva], t-boolean), t-set-app(tva)], t-boolean))
  ]),
  SD.make-string-dict()
    .set("Set", t-data(
      "Set",
      [list: tva],
      [list: ],
      [string-dict:
        "add", t-arrow([list: tva], t-set-app(tva)),
        "remove", t-arrow([list: tva], t-set-app(tva)),
        "size", t-arrow([list: ], t-number),
        "member", t-arrow([list: tva], t-boolean),
        "pick", t-arrow([list: ], t-pick-app(tva, t-set-app(tva))),
        "union", t-arrow([list: t-set-app(tva)], t-set-app(tva)),
        "intersect", t-arrow([list: t-set-app(tva)], t-set-app(tva)),
        "difference", t-arrow([list: t-set-app(tva)], t-set-app(tva)),
        "symmetric-difference", t-arrow([list: t-set-app(tva)], t-set-app(tva)),
        "to-list", t-arrow([list: ], t-list-app(tva)),
        "fold", t-forall([list: tvb], t-arrow([list: t-arrow([list: tvb, tva], tvb), tvb], tvb))
      ])),
  SD.make-string-dict()
    .set("Set", t-set)
    .set("List", t-list)
    .set("Pick", t-pick))

module-const-lists = t-module("builtin://lists",
  t-record([string-dict:
    "List", t-arrow([list: t-top], t-boolean),
    "is-List", t-arrow([list: t-top], t-boolean),
    "empty", t-forall([list: tva], t-data-refinement(t-list-app(tva), "empty")),
    "is-empty", t-arrow([list: t-top], t-boolean),
    "link", t-forall([list: tva], t-arrow([list: tva, t-list-app(tva)], t-data-refinement(t-list-app(tva), "link"))),
    "is-link", t-arrow([list: t-top], t-boolean),
    "length", t-forall([list: tva], t-arrow([list: t-list-app(tva)], t-number)),
    "same-length", t-forall([list: tva, tvb], t-arrow([list: t-list-app(tva), t-list-app(tvb)], t-boolean)),
    "longer-than", t-forall([list: tva], t-arrow([list: t-list-app(tva), t-number], t-boolean)),
    "shorter-than", t-forall([list: tva], t-arrow([list: t-list-app(tva), t-number], t-boolean)),
    "get", t-forall([list: tva], t-arrow([list: t-list-app(tva), t-number], tva)),
    "set", t-forall([list: tva], t-arrow([list: t-list-app(tva), t-number, tva], t-list-app(tva))),
    "reverse", t-forall([list: tva], t-arrow([list: t-list-app(tva)], t-list-app(tva))),
    "push", t-forall([list: tva], t-arrow([list: t-list-app(tva), tva], t-list-app(tva))),
    "reverse-help", t-forall([list: tva], t-arrow([list: t-list-app(tva), t-list-app(tva)], t-list-app(tva))),
    "last", t-forall([list: tva], t-arrow([list: t-list-app(tva)], tva)),
    "sort", t-forall([list: tva], t-arrow([list: t-list-app(tva)], t-list-app(tva))),
    "sort-by", t-forall([list: tva], t-arrow([list: t-list-app(tva), t-arrow([list: tva, tva], t-boolean), t-arrow([list: tva, tva], t-boolean)], t-list-app(tva))),
    "range", t-arrow([list: t-number, t-number], t-list-app(t-number)),
    "range-by", t-arrow([list: t-number, t-number, t-number], t-list-app(t-number)),
    "repeat", t-forall([list: tva], t-arrow([list: t-number, tva], t-list-app(tva))),
    "filter", t-forall([list: tva], t-arrow([list: t-arrow([list: tva], t-boolean), t-list-app(tva)], t-list-app(tva))),
    "append", t-forall([list: tva], t-arrow([list: t-list-app(tva), t-list-app(tva)], t-list-app(tva))),
    "partition", t-forall([list: tva], t-arrow([list: t-arrow([list: tva], t-boolean), t-list-app(tva)], t-record([string-dict: "is-true", t-list-app(tva), "is-false", t-list-app(tva)]))),
    "remove", t-forall([list: tva], t-arrow([list: t-list-app(tva), tva], t-list-app(tva))),
    "find", t-forall([list: tva], t-arrow([list: t-arrow([list: tva], t-boolean), t-list-app(tva)], t-app(t-option, [list: tva]))),
    "split-at", t-forall([list: tva], t-arrow([list: t-number, t-list-app(tva)], t-record([string-dict: "prefix", t-list-app(tva), "suffix", t-list-app(tva)]))),
    "take", t-forall([list: tva], t-arrow([list: t-number, t-list-app(tva)], t-list-app(tva))),
    "drop", t-forall([list: tva], t-arrow([list: t-number, t-list-app(tva)], t-list-app(tva))),
    "any", t-forall([list: tva], t-arrow([list: t-arrow([list: tva], t-boolean), t-list-app(tva)], t-boolean)),
    "all", t-forall([list: tva], t-arrow([list: t-arrow([list: tva], t-boolean), t-list-app(tva)], t-boolean)),
    "all2", t-forall([list: tva, tvb], t-arrow([list: t-arrow([list: tva, tvb], t-boolean), t-list-app(tva), t-list-app(tvb)], t-boolean)),
    "map", t-forall([list: tva, tvb], t-arrow([list: t-arrow([list: tva], tvb), t-list-app(tva)], t-list-app(tvb))),
    "map2", t-forall([list: tva, tvb, tvc], t-arrow([list: t-arrow([list: tva, tvb], tvc), t-list-app(tva), t-list-app(tvb)], t-list-app(tvc))),
    "map3", t-forall([list: tva, tvb, tvc, tvd], t-arrow([list: t-arrow([list: tva, tvb, tvc], tvd), t-list-app(tva), t-list-app(tvb), t-list-app(tvc)], t-list-app(tvd))),
    "map4", t-forall([list: tva, tvb, tvc, tvd, tve], t-arrow([list: t-arrow([list: tva, tvb, tvc, tvd], tve), t-list-app(tva), t-list-app(tvb), t-list-app(tvc), t-list-app(tvd)], t-list-app(tve))),
    "map_n", t-forall([list: tva, tvb], t-arrow([list: t-arrow([list: t-number, tva], tvb), t-number, t-list-app(tva)], t-list-app(tvb))),
    "map2_n", t-forall([list: tva, tvb, tvc], t-arrow([list: t-arrow([list: t-number, tva, tvb], tvc), t-number, t-list-app(tva), t-list-app(tvb)], t-list-app(tvc))),
    "map3_n", t-forall([list: tva, tvb, tvc, tvd], t-arrow([list: t-arrow([list: t-number, tva, tvb, tvc], tvd), t-number, t-list-app(tva), t-list-app(tvb), t-list-app(tvc)], t-list-app(tvd))),
    "map4_n", t-forall([list: tva, tvb, tvc, tvd, tve], t-arrow([list: t-arrow([list: t-number, tva, tvb, tvc, tvd], tve), t-number, t-list-app(tva), t-list-app(tvb), t-list-app(tvc), t-list-app(tvd)], t-list-app(tve))),
    "each", t-forall([list: tva], t-arrow([list: t-arrow([list: tva], t-top), t-list-app(tva)], t-nothing)),
    "each2", t-forall([list: tva, tvb], t-arrow([list: t-arrow([list: tva, tvb], t-top), t-list-app(tva), t-list-app(tvb)], t-nothing)),
    "each3", t-forall([list: tva, tvb, tvc], t-arrow([list: t-arrow([list: tva, tvb, tvc], t-top), t-list-app(tva), t-list-app(tvb), t-list-app(tvc)], t-nothing)),
    "each4", t-forall([list: tva, tvb, tvc, tvd], t-arrow([list: t-arrow([list: tva, tvb, tvc, tvd], t-top), t-list-app(tva), t-list-app(tvb), t-list-app(tvc), t-list-app(tvd)], t-nothing)),
    "each_n", t-forall([list: tva], t-arrow([list: t-arrow([list: t-number, tva], t-top), t-number, t-list-app(tva)], t-nothing)),
    "each2_n", t-forall([list: tva, tvb], t-arrow([list: t-arrow([list: t-number, tva, tvb], t-top), t-number, t-list-app(tva), t-list-app(tvb)], t-nothing)),
    "each3_n", t-forall([list: tva, tvb, tvc], t-arrow([list: t-arrow([list: t-number, tva, tvb, tvc], t-top), t-number, t-list-app(tva), t-list-app(tvb), t-list-app(tvc)], t-nothing)),
    "each4_n", t-forall([list: tva, tvb, tvc, tvd], t-arrow([list: t-arrow([list: t-number, tva, tvb, tvc, tvd], t-top), t-number, t-list-app(tva), t-list-app(tvb), t-list-app(tvc), t-list-app(tvd)], t-nothing)),
    "fold-while", t-forall([list: tva, tvb], t-arrow([list: t-arrow([list: tva, tvb], t-either-app(tva, tva)), tva, t-list-app(tvb)], tva)),
    "fold", t-forall([list: tva, tvb], t-arrow([list: t-arrow([list: tva, tvb], tva), tva, t-list-app(tvb)], tva)),
    "foldl", t-forall([list: tva, tvb], t-arrow([list: t-arrow([list: tva, tvb], tva), tva, t-list-app(tvb)], tva)),
    "foldr", t-forall([list: tva, tvb], t-arrow([list: t-arrow([list: tva, tvb], tva), tva, t-list-app(tvb)], tva)),
    "fold2", t-forall([list: tva, tvb, tvc], t-arrow([list: t-arrow([list: tva, tvb, tvc], tva), tva, t-list-app(tvb), t-list-app(tvc)], tva)),
    "fold3", t-forall([list: tva, tvb, tvc, tvd], t-arrow([list: t-arrow([list: tva, tvb, tvc, tvd], tva), tva, t-list-app(tvb), t-list-app(tvc), t-list-app(tvd)], tva)),
    "fold4", t-forall([list: tva, tvb, tvc, tvd, tve], t-arrow([list: t-arrow([list: tva, tvb, tvc, tvd, tve], tva), tva, t-list-app(tvb), t-list-app(tvc), t-list-app(tvd), t-list-app(tve)], tva)),
    "fold_n", t-forall([list: tva, tvb], t-arrow([list: t-arrow([list: t-number, tva, tvb], tva), t-number, tva, t-list-app(tvb)], tva)),
    "member-with", t-forall([list: tva], t-arrow([list: t-list-app(tva), tva, t-arrow([list: tva, tva], t-equality-result)], t-equality-result)),
    "member3", t-forall([list: tva], t-arrow([list: t-list-app(tva), tva], t-equality-result)),
    "member", t-forall([list: tva], t-arrow([list: t-list-app(tva), tva], t-boolean)),
    "member-always3", t-forall([list: tva], t-arrow([list: t-list-app(tva), tva], t-equality-result)),
    "member-always", t-forall([list: tva], t-arrow([list: t-list-app(tva), tva], t-boolean)),
    "member-now", t-forall([list: tva], t-arrow([list: t-list-app(tva), tva], t-boolean)),
    "member-now3", t-forall([list: tva], t-arrow([list: t-list-app(tva), tva], t-equality-result)),
    "member-identical3", t-forall([list: tva], t-arrow([list: t-list-app(tva), tva], t-equality-result)),
    "member-identical", t-forall([list: tva], t-arrow([list: t-list-app(tva), tva], t-boolean)),
    "shuffle", t-forall([list: tva], t-arrow([list: t-list-app(tva)], t-list-app(tva))),
    "filter-map", t-forall([list: tva, tvb], t-arrow([list: t-arrow([list: tva], t-option-app(tvb)), t-list-app(tva)], t-list-app(tvb))),
    "filter-values", t-forall([list: tva], t-arrow([list: t-list-app(t-option-app(tva))], t-list-app(tva))),
    "distinct", t-forall([list: tva], t-arrow([list: t-list-app(tva)], t-list-app(tva))),
    "take-while", t-forall([list: tva], t-arrow([list: t-arrow([list: tva], t-boolean), t-list-app(tva)], t-tuple([list: t-list-app(tva), t-list-app(tva)]))),
    "join-str", t-forall([list: tva], t-arrow([list: t-list-app(tva), t-string], t-string)),
    "join-str-last", t-forall([list: tva], t-arrow([list: t-list-app(tva), t-string, t-string], t-string)),
    "list",
        t-record([string-dict:
              "make", t-forall([list: tva], t-arrow([list: t-array(tva)], t-list-app(tva))),
              "make0", t-forall([list: tva], t-arrow([list: ], t-data-refinement(t-list-app(tva), "empty"))),
              "make1", t-forall([list: tva], t-arrow([list: tva], t-data-refinement(t-list-app(tva), "link"))),
              "make2", t-forall([list: tva], t-arrow([list: tva, tva], t-data-refinement(t-list-app(tva), "link"))),
              "make3", t-forall([list: tva], t-arrow([list: tva, tva, tva], t-data-refinement(t-list-app(tva), "link"))),
              "make4", t-forall([list: tva], t-arrow([list: tva, tva, tva, tva], t-data-refinement(t-list-app(tva), "link"))),
              "make5", t-forall([list: tva], t-arrow([list: tva, tva, tva, tva, tva], t-data-refinement(t-list-app(tva), "link")))
            ])
  ]),
  SD.make-string-dict()
    .set("List", t-data(
      "List",
      [list: tva],
      [list:
        t-singleton-variant("empty", [string-dict: ]),
        t-variant("link", [list: {"first"; tva}, {"rest"; t-list-app(tva)}], [string-dict: ])
      ],
      [string-dict:
        "length", t-arrow([list: ], t-number),
        "each", t-arrow([list: t-arrow([list: tva], t-nothing)], t-nothing),
        "map", t-forall([list: tvb], t-arrow([list: t-arrow([list: tva], tvb)], t-list-app(tvb))),
        "filter", t-arrow([list: t-arrow([list: tva], t-boolean)], t-list-app(tva)),
        "find", t-arrow([list: t-arrow([list: tva], t-boolean)], t-option-app(tva)),
        "partition", t-arrow([list: t-arrow([list: tva], t-boolean)], t-record([string-dict: "is-true", t-list-app(tva), "is-false", t-list-app(tvb)])),
        "foldr", t-forall([list: tvb], t-arrow([list: t-arrow([list: tva, tvb], tvb), tvb], tvb)),
        "foldl", t-forall([list: tvb], t-arrow([list: t-arrow([list: tva, tvb], tvb), tvb], tvb)),
        "all", t-arrow([list: t-arrow([list: tva], t-boolean)], t-boolean),
        "any", t-arrow([list: t-arrow([list: tva], t-boolean)], t-boolean),
        "member", t-arrow([list: tva], t-boolean),
        "append", t-arrow([list: t-list-app(tva)], t-list-app(tva)),
        "last", t-arrow([list: ], tva),
        "reverse", t-arrow([list: ], t-list-app(tva)),
        "sort-by", t-arrow([list: t-arrow([list: tva, tva], t-boolean), t-arrow([list: tva, tva], t-boolean)], t-list-app(tva)),
        "sort", t-arrow([list: ], t-list-app(tva)),
        "join-str", t-arrow([list: t-string], t-string),
        "join-str-last", t-arrow([list: t-string, t-string], t-string),
        "_output", t-output,
        "_plus", t-arrow([list: t-list-app(tva)], t-list-app(tva)),
        "push", t-arrow([list: tva], t-list-app(tva)),
        "split-at", t-arrow([list: t-number], t-record([string-dict: "prefix", t-list-app(tva), "suffix", t-list-app(tva)])),
        "take", t-arrow([list: t-number], t-list-app(tva)),
        "drop", t-arrow([list: t-number], t-list-app(tva)),
        "get", t-arrow([list: t-number], tva),
        "set", t-arrow([list: t-number, tva], t-list-app(tva)),
        "remove", t-arrow([list: tva], t-list-app(tva)),
      ])),
  SD.make-string-dict()
    .set("List", t-list)
    .set("Either", t-either)
    .set("Option", t-option))

t-and-then =
  t-forall(
    [list: tva, tvb],
    t-arrow(
      [list: t-arrow([list: tva], tvb)],
      t-option-app(tvb)))

module-const-option = t-module("builtin://option",
  t-record([string-dict:
    "Option", t-arrow([list: t-top], t-boolean),
    "is-Option", t-arrow([list: t-top], t-boolean),
    "none", t-forall([list: tva], t-data-refinement(t-option-app(tva), "none")),
    "is-none", t-arrow([list: t-top], t-boolean),
    "some", t-forall([list: tva], t-arrow([list: tva], t-data-refinement(t-option-app(tva), "some"))),
    "is-some", t-arrow([list: t-top], t-boolean)
  ]),
  SD.make-string-dict()
    .set("Option", t-data(
        "Option",
        [list: tva],
        [list:
          t-singleton-variant("none",
            [string-dict:
              "_match", t-top,
              "or-else", t-arrow([list: tva], tva),
              "and-then", t-and-then
            ]
          ),
          t-variant("some",
            [list: {"value"; tva}],
            [string-dict:
              "_match", t-top,
              "or-else", t-arrow([list: tva], tva),
              "and-then", t-and-then
            ]
          )
        ],
        [string-dict:
          "and-then", t-and-then,
          "or-else", t-arrow([list: tva], tva),
          "_match", t-top
      ])),
  SD.make-string-dict()
    .set("Option", t-option)
)

module-const-error = t-module("builtin://error",
  t-record([string-dict:
    "RuntimeError", t-arrow([list: t-top], t-boolean),
    "is-RuntimeError", t-arrow([list: t-top], t-boolean),
    "message-exception", t-arrow([list: t-string], t-runtime-error),
    "is-message-exception", t-arrow([list: t-top], t-boolean),
    "no-branches-matched", t-arrow([list: t-top, t-string], t-runtime-error),
    "is-no-branches-matched", t-arrow([list: t-top], t-boolean),
    "internal-error", t-arrow([list: t-top, t-top], t-runtime-error),
    "is-internal-error", t-arrow([list: t-top], t-boolean),
    "field-not-found", t-arrow([list: t-top, t-top, t-string], t-runtime-error),
    "is-field-not-found", t-arrow([list: t-top], t-boolean),
    "lookup-non-object", t-arrow([list: t-top, t-top, t-string], t-runtime-error),
    "is-lookup-non-object", t-arrow([list: t-top], t-boolean),
    "extend-non-object", t-arrow([list: t-top, t-top], t-runtime-error),
    "is-extend-non-object", t-arrow([list: t-top], t-boolean),
    "non-boolean-condition", t-arrow([list: t-top, t-top, t-top], t-runtime-error),
    "is-non-boolean-condition", t-arrow([list: t-top], t-boolean),
    "non-boolean-op", t-arrow([list: t-top, t-top, t-top, t-top], t-runtime-error),
    "is-non-boolean-op", t-arrow([list: t-top], t-boolean),
    "generic-type-mismatch", t-arrow([list: t-top, t-string], t-runtime-error),
    "is-generic-type-mismatch", t-arrow([list: t-top], t-boolean),
    "outside-numeric-range", t-arrow([list: t-top, t-top, t-top], t-runtime-error),
    "is-outside-numeric-range", t-arrow([list: t-top], t-boolean),
    "plus-error", t-arrow([list: t-top, t-top], t-runtime-error),
    "is-plus-error", t-arrow([list: t-top], t-boolean),
    "numeric-binop-error", t-arrow([list: t-top, t-top, t-top, t-top, t-top], t-runtime-error),
    "is-numeric-binop-error", t-arrow([list: t-top], t-boolean),
    "cases-arity-mismatch", t-arrow([list: t-top, t-top, t-top, t-top, t-top], t-runtime-error),
    "is-cases-arity-mismatch", t-arrow([list: t-top], t-boolean),
    "cases-singleton-mismatch", t-arrow([list: t-top, t-boolean, t-top, t-top], t-runtime-error),
    "is-cases-singleton-mismatch", t-arrow([list: t-top], t-boolean),
    "arity-mismatch", t-arrow([list: t-top, t-top, t-top], t-runtime-error),
    "is-arity-mismatch", t-arrow([list: t-top], t-boolean),
    "non-function-app", t-arrow([list: t-top, t-top], t-runtime-error),
    "is-non-function-app", t-arrow([list: t-top], t-boolean),
    "bad-app", t-arrow([list: t-top, t-string, t-string, t-number, t-top], t-runtime-error),
    "is-bad-app", t-arrow([list: t-top], t-boolean),
    "uninitialized-id", t-arrow([list: t-top, t-string], t-runtime-error),
    "is-uninitialized-id", t-arrow([list: t-top], t-boolean),
    "module-load-failure", t-arrow([list: t-top], t-runtime-error),
    "is-module-load-failure", t-arrow([list: t-top], t-boolean),
    "invalid-array-index", t-arrow([list: t-string, t-top, t-number, t-string], t-runtime-error),
    "is-invalid-array-index", t-arrow([list: t-top], t-boolean),
    "user-break", t-runtime-error,
    "is-user-break", t-arrow([list: t-top], t-boolean),
    "user-exception", t-arrow([list: t-top], t-runtime-error),
    "is-user-exception", t-arrow([list: t-top], t-boolean),
    "exit", t-arrow([list: t-number], t-runtime-error),
    "is-exit", t-arrow([list: t-top], t-boolean),
    "exit-quiet", t-arrow([list: t-number], t-runtime-error),
    "is-exit-quiet", t-arrow([list: t-top], t-boolean),
    "ParseError", t-arrow([list: t-top], t-boolean),
    "is-ParseError", t-arrow([list: t-top], t-boolean),
    "parse-error-next-token", t-arrow([list: t-top, t-string], t-parse-error),
    "is-parse-error-next-token", t-arrow([list: t-top], t-boolean),
    "parse-error-eof", t-arrow([list: t-top], t-parse-error),
    "is-parse-error-eof", t-arrow([list: t-top], t-boolean),
    "parse-error-unterminated-string", t-arrow([list: t-top], t-parse-error),
    "is-parse-error-unterminated-string", t-arrow([list: t-top], t-boolean),
    "parse-error-bad-number", t-arrow([list: t-top], t-parse-error),
    "is-parse-error-bad-number", t-arrow([list: t-top], t-boolean),
    "parse-error-bad-operator", t-arrow([list: t-top], t-parse-error),
    "is-parse-error-bad-operator", t-arrow([list: t-top], t-boolean),
    "parse-error-bad-check-operator", t-arrow([list: t-top], t-parse-error),
    "is-parse-error-bad-check-operator", t-arrow([list: t-top], t-boolean),
    "empty-block", t-arrow([list: t-top], t-parse-error),
    "is-empty-block", t-arrow([list: t-top], t-boolean),
    "bad-block-stmt", t-arrow([list: t-top], t-parse-error),
    "is-bad-block-stmt", t-arrow([list: t-top], t-boolean),
    "bad-check-block-stmt", t-arrow([list: t-top], t-parse-error),
    "is-bad-check-block-stmt", t-arrow([list: t-top], t-boolean),
    "fun-missing-colon", t-arrow([list: t-top], t-parse-error),
    "is-fun-missing-colon", t-arrow([list: t-top], t-boolean),
    "fun-missing-end", t-arrow([list: t-top], t-parse-error),
    "is-fun-missing-end", t-arrow([list: t-top], t-boolean),
    "args-missing-comma", t-arrow([list: t-top], t-parse-error),
    "is-args-missing-comma", t-arrow([list: t-top], t-boolean),
    "app-args-missing-comma", t-arrow([list: t-top], t-parse-error),
    "is-app-args-missing-comma", t-arrow([list: t-top], t-boolean),
    "missing-end", t-arrow([list: t-top], t-parse-error),
    "is-missing-end", t-arrow([list: t-top], t-boolean),
    "missing-comma", t-arrow([list: t-top], t-parse-error),
    "is-missing-comma", t-arrow([list: t-top], t-boolean)
  ]),
  SD.make-string-dict()
    .set("RuntimeError",
      t-data(
        "RuntimeError",
        [list: ],
        [list:
          t-variant("message-exception", [list: {"message"; t-string}], [string-dict: ]),
          t-variant("no-branches-matched", [list: {"loc"; t-top}, {"expression"; t-string}], [string-dict: ]),
          t-variant("internal-error", [list: {"message"; t-top}, {"info-args"; t-top}], [string-dict: ]),
          t-variant("field-not-found", [list: {"loc"; t-top}, {"obj"; t-top}, {"field"; t-string}], [string-dict: ]),
          t-variant("lookup-non-object", [list: {"loc"; t-top}, {"non-obj"; t-top}, {"field"; t-string}], [string-dict: ]),
          t-variant("extend-non-object", [list: {"loc"; t-top}, {"non-obj"; t-top}], [string-dict: ]),
          t-variant("generic-type-mismatch", [list: {"val"; t-top}, {"typ"; t-string}], [string-dict: ]),
          t-variant("numeric-binop-error", [list: {"val1"; t-top}, {"val2"; t-top}, {"opname"; t-top}, {"opdesc"; t-top}, {"methodname"; t-top}], [string-dict: ]),
          t-variant("cases-arity-mismatch", [list: {"branch-loc"; t-top}, {"num-args"; t-top}, {"actual-arity"; t-top}, {"cases-loc"; t-top}], [string-dict: ]),
          t-variant("cases-singleton-mismatch", [list: {"branch-loc"; t-top}, {"should-be-singleton"; t-boolean}, {"cases-loc"; t-top}], [string-dict: ]),
          t-variant("arity-mismatch", [list: {"fun-def-loc"; t-top}, {"fun-def-arity"; t-top}, {"fun-app-args"; t-top}], [string-dict: ]),
          t-variant("non-function-app", [list: {"loc"; t-top}, {"non-fun-val"; t-top}], [string-dict: ]),
          t-variant("uninitialized-id", [list: {"loc"; t-top}, {"name"; t-string}], [string-dict: ]),
          t-variant("module-load-failure", [list: {"names"; t-top}], [string-dict: ]),
          t-variant("invalid-array-index", [list: {"method-name"; t-string}, {"array"; t-top}, {"index"; t-number}, {"reason"; t-string}], [string-dict: ]),
          t-singleton-variant("user-break", [string-dict: ])
        ],
        [string-dict:
          "_match", t-top
        ]))
    .set("ParseError", t-data(
      "ParseError",
      [list: ],
      [list:
        t-variant("parse-error-next-token", [list: {"loc"; t-top}, {"next-token"; t-string}], [string-dict: ]),
        t-variant("parse-error-bad-check-operator", [list: {"loc"; t-top}, {"next-token"; t-string}], [string-dict: ]),
        t-variant("parse-error-bad-operator", [list: {"loc"; t-top}, {"next-token"; t-string}], [string-dict: ]),
        t-variant("parse-error-bad-number", [list: {"loc"; t-top}, {"next-token"; t-string}], [string-dict: ]),
        t-variant("parse-error-eof", [list: {"loc"; t-top}], [string-dict: ]),
        t-variant("parse-error-unterminated-string", [list: {"loc"; t-top}], [string-dict: ]),
        t-variant("empty-block", [list: {"loc"; t-top}], [string-dict: ]),
        t-variant("bad-block-stmt", [list: {"loc"; t-top}], [string-dict: ]),
        t-variant("bad-check-block-stmt", [list: {"loc"; t-top}], [string-dict: ]),
        t-variant("fun-missing-colon", [list: {"loc"; t-top}], [string-dict: ]),
        t-variant("fun-missing-end", [list: {"loc"; t-top}], [string-dict: ]),
        t-variant("args-missing-comma", [list: {"loc"; t-top}], [string-dict: ]),
        t-variant("app-args-missing-comma", [list: {"loc"; t-top}], [string-dict: ]),
        t-variant("missing-end", [list: {"loc"; t-top}], [string-dict: ]),
        t-variant("missing-comma", [list: {"loc"; t-top}], [string-dict: ])
      ],
      [string-dict:
        "loc", t-top,
        "_match", t-top
      ])
    ),
  SD.make-string-dict()
    .set("RuntimeError", t-runtime-error)
    .set("ParseError", t-parse-error)
    .set("Error", t-name(local, A.s-name(A.dummy-loc, "Error")))
)

module-const-either = t-module("builtin://either",
  t-record([string-dict:
    "Either", t-arrow([list: t-top], t-boolean),
    "is-Either", t-arrow([list: t-top], t-boolean),
    "left", t-forall([list: tva, tvb], t-arrow([list: tva], t-data-refinement(t-either-app(tva, tvb), "left"))),
    "is-left", t-arrow([list: t-top], t-boolean),
    "right", t-forall([list: tva, tvb], t-arrow([list: tvb], t-data-refinement(t-either-app(tva, tvb), "right"))),
    "is-right", t-arrow([list: t-top], t-boolean)
  ]),
  SD.make-string-dict()
    .set("Either", t-data(
        "Either",
        [list: tva, tvb],
        [list:
          t-variant("left",
            [list:
              {"v"; tva}
            ],
            [string-dict:
              "_match", t-top,
            ]
          ),
          t-variant("right",
            [list:
              {"v"; tvb}
            ],
            [string-dict:
              "_match", t-top,
            ]
          )
        ],
        [string-dict:
          "v", t-top,
          "_match", t-top
      ])
    ),
  SD.make-string-dict()
    .set("Either", t-either))

s-exp-struct-mems = [string-dict:
  "s-list", t-arrow([list: t-list-app(t-s-exp)], t-s-exp),
  "s-num", t-arrow([list: t-number], t-s-exp),
  "s-str", t-arrow([list: t-string], t-s-exp),
  "s-sym", t-arrow([list: t-string], t-s-exp),
  "is-s-list", t-arrow([list: t-top], t-boolean),
  "is-s-num", t-arrow([list: t-top], t-boolean),
  "is-s-str", t-arrow([list: t-top], t-boolean),
  "is-s-sym", t-arrow([list: t-top], t-boolean)
]

module-const-s-exp = t-module("builtin://s-exp",
  t-record(s-exp-struct-mems.set(
    "read-s-exp", t-arrow([list: t-string], t-s-exp)
  )),
  SD.make-string-dict(),
  SD.make-string-dict()
    .set("S-Exp", t-s-exp)
)

module-const-s-exp-structs = t-module("builtin://s-exp-structs",
  t-record(s-exp-struct-mems),
  SD.make-string-dict()
    .set("S-Exp", t-data(
      "S-Exp",
      [list: ],
      [list:
        t-variant("s-list",
          [list:
            {"exps"; t-list-app(t-s-exp)}
          ],
          [string-dict:
            "_match", t-top,
          ]
        ),
        t-variant("s-num",
          [list:
            {"n"; t-number}
          ],
          [string-dict:
            "_match", t-top,
          ]
        ),
        t-variant("s-str",
          [list:
            {"s"; t-string}
          ],
          [string-dict:
            "_match", t-top,
          ]
        ),
        t-variant("s-sym",
          [list:
            {"s"; t-string}
          ],
          [string-dict:
            "_match", t-top,
          ]
        )
      ],
      [string-dict:
      ])
    ),
  SD.make-string-dict()
    .set("S-Exp", t-s-exp)
)

module-const-json-structs = t-module("builtin://json-structs",
  t-record([string-dict:
    "link", t-forall([list: tva], t-arrow([list: tva, t-list-app(tva)], t-data-refinement(t-list-app(tva), "link"))),
    "empty", t-forall([list: tva], t-data-refinement(t-list-app(tva), "empty")),
    "is-empty", t-arrow([list: t-top], t-boolean),
    "is-link", t-arrow([list: t-top], t-boolean),
    "map", t-forall([list: tva, tvb], t-arrow([list: t-arrow([list: tva], tvb), t-list-app(tva)], t-list-app(tvb))),
    "is-array", t-forall([list: tva], t-arrow([list: t-top], t-boolean)),
    "JSON", t-arrow([list: t-top], t-boolean),
    "is-JSON", t-arrow([list: t-top], t-boolean),
    "j-obj", t-arrow([list: t-string-dict-app(t-json)], t-data-refinement(t-json, "j-obj")),
    "is-j-obj", t-arrow([list: t-top], t-boolean),
    "j-arr", t-arrow([list: t-list-app(t-json)], t-data-refinement(t-json, "j-arr")),
    "is-j-arr", t-arrow([list: t-top], t-boolean),
    "j-num", t-arrow([list: t-number], t-data-refinement(t-json, "j-num")),
    "is-j-num", t-arrow([list: t-top], t-boolean),
    "j-str", t-arrow([list: t-string], t-data-refinement(t-json, "j-str")),
    "is-j-str", t-arrow([list: t-top], t-boolean),
    "j-bool", t-arrow([list: t-boolean], t-data-refinement(t-json, "j-bool")),
    "is-j-bool", t-arrow([list: t-top], t-boolean),
    "j-null", t-arrow([list: ], t-data-refinement(t-json, "j-null")),
    "is-j-null", t-arrow([list: t-top], t-boolean),
    "tojson", t-arrow([list: t-top], t-json)
  ]),
  SD.make-string-dict()
    .set("JSON", t-data(
        "JSON",
        [list: ],
        [list:
          t-variant("j-obj",
            [list: {"dict"; t-string-dict-app(t-json)}],
            [string-dict: ]
          ),
          t-variant("j-arr",
            [list: {"l"; t-list-app(t-json)}],
            [string-dict: ]
          ),
          t-variant("j-num",
            [list: {"n"; t-number}],
            [string-dict: ]
          ),
          t-variant("j-str",
            [list: {"s"; t-string}],
            [string-dict: ]
          ),
          t-variant("j-bool",
            [list: {"b"; t-boolean}],
            [string-dict: ]
          ),
          t-singleton-variant("j-null", [string-dict: ])
        ],
        [string-dict:
          "native", t-arrow([list: ], t-top),
          "serialize", t-arrow([list: ], t-string)
      ])
    ),
  SD.make-string-dict()
    .set("List", t-list)
    .set("JSON", t-json))

default-modules = SD.make-mutable-string-dict()
default-modules.set-now("builtin://equality", module-const-equality)
default-modules.set-now("builtin://lists", module-const-lists)
default-modules.set-now("builtin://option", module-const-option)
default-modules.set-now("builtin://error", module-const-error)
default-modules.set-now("builtin://either", module-const-either)
default-modules.set-now("builtin://arrays", module-const-arrays)
default-modules.set-now("builtin://pick", module-const-pick)
default-modules.set-now("builtin://sets", module-const-sets)
default-modules.set-now("builtin://s-exp", module-const-s-exp)
default-modules.set-now("builtin://s-exp-structs", module-const-s-exp-structs)
default-modules.set-now("builtin://json-structs", module-const-json-structs)
shadow default-modules = default-modules.freeze()

fun make-default-modules() block:
  default-modules
end
