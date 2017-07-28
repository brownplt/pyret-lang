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

t-data                    = TS.t-data(_, _, _, _, A.dummy-loc)

t-number                  = TS.t-number(A.dummy-loc)
t-string                  = TS.t-string(A.dummy-loc)
t-boolean                 = TS.t-boolean(A.dummy-loc)
t-array                   = TS.t-array(_, A.dummy-loc)
t-nothing                 = TS.t-nothing(A.dummy-loc)
t-srcloc                  = TS.t-srcloc(A.dummy-loc)
t-array-name              = TS.t-array-name

type ModuleType           = TS.ModuleType
t-module                  = TS.t-module

type TypeVariant          = TS.TypeVariant
t-variant                 = TS.t-variant(_, _, _, A.dummy-loc)
t-singleton-variant       = TS.t-singleton-variant(_, _, A.dummy-loc)

string-dict = SD.string-dict

s-atom                    = A.s-atom

t-number-binop = t-arrow([list: t-number, t-number], t-number)

t-image = t-name(module-uri("builtin://image"), A.s-type-global("Image"))

t-option = lam(param :: Type):
  t-app(t-name(module-uri("builtin://option"), A.s-type-global("Option")), [list: param])
end

t-reactor = lam(param :: Type):
  t-app(t-name(module-uri("builtin://reactors"), A.s-type-global("Reactor")), [list: param])
end

eq-EqualityResult = t-name(module-uri("builtin://equality"), A.s-type-global("EqualityResult"))

vs-ValueSkeleton = t-name(module-uri("builtin://valueskeleton"), A.s-type-global("ValueSkeleton"))

tva = t-var(A.global-names.make-atom("A"))
tvb = t-var(A.global-names.make-atom("B"))
tvc = t-var(A.global-names.make-atom("C"))
tvd = t-var(A.global-names.make-atom("D"))
tve = t-var(A.global-names.make-atom("E"))
tvf = t-var(A.global-names.make-atom("F"))
tvg = t-var(A.global-names.make-atom("G"))
tvh = t-var(A.global-names.make-atom("H"))

fun make-default-aliases():
  default-aliases = [SD.string-dict:
    A.s-type-global("Nothing").key(), t-nothing,
    A.s-type-global("Method").key(), t-top,
    A.s-type-global("Object").key(), t-top,
    A.s-type-global("Function").key(), t-top,
    A.s-type-global("RawArray").key(), t-array-name,
    A.s-type-global("Number").key(), t-number,
    A.s-type-global("NumNonNegative").key(), t-number,
    A.s-type-global("NumNonPositive").key(), t-number,
    A.s-type-global("NumNegative").key(), t-number,
    A.s-type-global("NumPositive").key(), t-number,
    A.s-type-global("NumRational").key(), t-number,
    A.s-type-global("NumInteger").key(), t-number,
    A.s-type-global("Roughnum").key(), t-number,
    A.s-type-global("Exactnum").key(), t-number,
    A.s-type-global("String").key(), t-string,
    A.s-type-global("Boolean").key(), t-boolean]
  default-aliases
end

fun make-default-types() block:
  default-typs = SD.make-mutable-string-dict()
  default-typs.set-now(A.s-global("builtins").key(), t-record([string-dict:
      "has-field", t-arrow([list: t-record([string-dict: ])], t-boolean),
      "trace-value", t-arrow([list: t-top, t-top], t-bot),
      "current-checker", t-arrow([list: ], t-record([string-dict: # Cheat on these types for now.
          "run-checks", t-bot,
          "check-is", t-bot,
          "check-is-not", t-bot,
          "check-is-refinement", t-bot,
          "check-is-not-refinement", t-bot,
          "check-satisfies", t-bot,
          "check-satisfies-not", t-bot,
          "check-raises-str", t-bot,
          "check-raises-not", t-bot,
          "check-raises-other-str", t-bot,
          "check-raises-satisfies", t-bot,
          "check-raises-violates" , t-bot
      ]))
  ]))

  # Need to be fixed to correct type:
  default-typs.set-now(A.s-global("raw-array-get").key(), t-forall([list: tva], t-arrow([list: t-array(tva)], tva)))
  default-typs.set-now(A.s-global("raw-array-set").key(), t-forall([list: tva], t-arrow([list: t-array(tva), t-number, tva], t-array(tva))))
  default-typs.set-now(A.s-global("raw-array-of").key(), t-forall([list: tva], t-arrow([list: tva, t-number], t-array(tva))))
  default-typs.set-now(A.s-global("raw-array-length").key(), t-forall([list: tva], t-arrow([list: t-array(tva)], t-number)))
  default-typs.set-now(A.s-global("raw-array-to-list").key(), t-forall([list: tva], t-arrow([list: t-array(tva)], t-app(t-list, [list: tva]))))
  default-typs.set-now(A.s-global("raw-array-fold").key(), t-forall([list: tva, tvb], t-arrow([list: t-arrow([list: tvb, tva, t-number], tvb), tvb, t-array(tva), t-number], tvb)))
  default-typs.set-now(A.s-global("raw-array-from-list").key(), t-top)
  default-typs.set-now(A.s-global("raw-array-join-str").key(), t-top)
  default-typs.set-now(A.s-global("raw-array").key(), t-top)
  default-typs.set-now(A.s-global("ref-get").key(), t-top)
  default-typs.set-now(A.s-global("ref-set").key(), t-top)
  default-typs.set-now(A.s-global("ref-freeze").key(), t-top)
  default-typs.set-now(A.s-global("exn-unwrap").key(), t-top)
  default-typs.set-now(A.s-global("test-print").key(), t-forall([list: tva], t-arrow([list: tva], tva)))
  default-typs.set-now(A.s-global("print-error").key(), t-top)
  default-typs.set-now(A.s-global("display-error").key(), t-top)
  default-typs.set-now(A.s-global("brander").key(), t-top)
  default-typs.set-now(A.s-global("run-task").key(), t-top)
  default-typs.set-now(A.s-global("string-split").key(), t-top)
  default-typs.set-now(A.s-global("string-split-all").key(), t-top)
  default-typs.set-now(A.s-global("string-explode").key(), t-top)
  default-typs.set-now(A.s-global("string-index-of").key(), t-top)
  default-typs.set-now(A.s-global("string-to-code-points").key(), t-top)
  default-typs.set-now(A.s-global("string-from-code-points").key(), t-top)
  default-typs.set-now("isBoolean", t-arrow([list: t-top], t-boolean))
  default-typs.set-now("makeSome", t-forall([list: tva], t-arrow([list: tva], t-option(tva))))
  default-typs.set-now("makeNone", t-forall([list: tva], t-arrow([list: ], t-option(tva))))
  default-typs.set-now("checkWrapBoolean", t-arrow([list: t-boolean], t-boolean))
  default-typs.set-now("checkTupleBind", t-arrow([list: t-top, t-number, t-srcloc], t-bot))
  default-typs.set-now("throwNonBooleanCondition", t-arrow([list: t-srcloc, t-string, t-top], t-bot))
  default-typs.set-now("throwNoBranchesMatched", t-arrow([list: t-srcloc, t-string], t-bot))
  default-typs.set-now("throwUnfinishedTemplate", t-arrow([list: t-srcloc], t-bot))
  default-typs.set-now("makeReactor", t-forall([list: tva], t-arrow([list:
      tva,
      t-record([string-dict:
        "on-tick", t-option(t-arrow([list: tva], tva)),
        "on-mouse", t-option(t-arrow([list: tva, t-number, t-number, t-string], tva)),
        "on-key", t-option(t-arrow([list: tva, t-string], tva)),
        "to-draw", t-option(t-arrow([list: tva], t-image)),
        "stop-when", t-option(t-arrow([list: tva], t-boolean)),
        "seconds-per-tick", t-option(t-number),
        "close-when-stop", t-option(t-boolean),
        "title", t-option(t-string)])],
    t-reactor(tva))))
  default-typs.set-now("not", t-arrow([list: t-boolean], t-boolean))
  default-typs.set-now(A.s-global("raise").key(), t-arrow([list: t-top], t-bot))
  default-typs.set-now("hasField", t-arrow([list: t-record([string-dict: ]), t-string], t-boolean))
  default-typs.set-now(A.s-global("_times").key(), t-number-binop)
  default-typs.set-now(A.s-global("_minus").key(), t-number-binop)
  default-typs.set-now(A.s-global("_divide").key(), t-number-binop)
  default-typs.set-now(A.s-global("_plus").key(), t-number-binop)
  default-typs.set-now("makeSrcloc", t-arrow([list: t-srcloc], t-bot))
  default-typs.set-now(A.s-global("string-tonumber").key(), t-arrow([list: t-string], t-number))
  default-typs.set-now(A.s-global("string-to-number").key(), t-arrow([list: t-string], t-option(t-number)))

  default-typs.set-now(A.s-global("_lessthan").key(), t-number-binop)
  default-typs.set-now(A.s-global("_lessequal").key(), t-number-binop)
  default-typs.set-now(A.s-global("_greaterthan").key(), t-number-binop)
  default-typs.set-now(A.s-global("_greaterequal").key(), t-number-binop)
  default-typs.set-now(A.s-global("print").key(), t-forall([list: tva], t-arrow([list: tva], tva)))
  default-typs.set-now(A.s-global("display").key(), t-forall([list: tva], t-arrow([list: tva], tva)))

  default-typs.set-now("getMaker", t-forall([list: tva, tvb], t-arrow([list: t-record([string-dict: "make", t-arrow([list: t-array(tvb)], tva)]), t-string, t-srcloc, t-srcloc], t-arrow([list: t-array(tvb)], tva))))
  default-typs.set-now("getLazyMaker", t-forall([list: tva, tvb], t-arrow([list: t-record([string-dict: "lazy-make", t-arrow([list: t-array(t-arrow([list: ], tvb))], tva)]), t-string, t-srcloc, t-srcloc], t-arrow([list: t-array(t-arrow([list: ], tvb))], tva))))
  default-typs.set-now("getMaker0", t-forall([list: tva], t-arrow([list: t-record([string-dict: "make0", t-arrow([list: ], tva)]), t-string, t-srcloc, t-srcloc], t-arrow([list: ], tva))))
  default-typs.set-now("getMaker1", t-forall([list: tva, tvb], t-arrow([list: t-record([string-dict: "make1", t-arrow([list: tvb], tva)]), t-string, t-srcloc, t-srcloc], t-arrow([list: tvb], tva))))
  default-typs.set-now("getMaker2", t-forall([list: tva, tvb, tvc], t-arrow([list: t-record([string-dict: "make2", t-arrow([list: tvb, tvc], tva)]), t-string, t-srcloc, t-srcloc], t-arrow([list: tvb, tvc], tva))))
  default-typs.set-now("getMaker3", t-forall([list: tva, tvb, tvc, tvd], t-arrow([list: t-record([string-dict: "make3", t-arrow([list: tvb, tvc, tvd], tva)]), t-string, t-srcloc, t-srcloc], t-arrow([list: tvb, tvc, tvd], tva))))
  default-typs.set-now("getMaker4", t-forall([list: tva, tvb, tvc, tvd, tve], t-arrow([list: t-record([string-dict: "make4", t-arrow([list: tvb, tvc, tvd, tve], tva)]), t-string, t-srcloc, t-srcloc], t-arrow([list: tvb, tvc, tvd, tve], tva))))
  default-typs.set-now("getMaker5", t-forall([list: tva, tvb, tvc, tvd, tve, tvf], t-arrow([list: t-record([string-dict: "make5", t-arrow([list: tvb, tvc, tvd, tve, tvf], tva)]), t-string, t-srcloc, t-srcloc], t-arrow([list: tvb, tvc, tvd, tve, tvf], tva))))

  default-typs.freeze()
end

fun make-default-data-exprs() block:
  default-data-exprs = SD.make-string-dict()
  default-data-exprs
end

# Begin hard-coded module types
rec t-list = t-name(module-uri("builtin://lists"), A.s-type-global("List"))
fun mk-list(a :: Type) -> Type:
  t-app(t-list, [list: a])
end

t-big-array = t-name(module-uri("builtin://arrays"), A.s-type-global("Array"))
fun mk-array(typ :: Type):
  t-app(t-big-array, [list: typ])
end

t-set = t-name(module-uri("builtin://sets"), A.s-type-global("Set"))
fun mk-set(typ :: Type):
  t-app(t-set, [list: typ])
end

t-output = t-arrow([list: ], vs-ValueSkeleton)

# Functions for adding hard-coded modules
module-const-equality = t-module("builtin://equality",
  t-record([string-dict:
    "EqualityResult", t-arrow([list: t-top], t-boolean),
    "is-EqualityResult", t-arrow([list: t-top], t-boolean),
    "Equal", t-data-refinement(eq-EqualityResult, "Equal"),
    "is-Equal", t-arrow([list: t-top], t-boolean),
    "NotEqual", t-arrow([list: t-string], t-data-refinement(eq-EqualityResult, "NotEqual")),
    "is-NotEqual", t-arrow([list: t-top], t-boolean),
    "Unknown", t-data-refinement(eq-EqualityResult, "Unknown"),
    "is-Unknown", t-arrow([list: t-top], t-boolean),
    "equal-and", t-arrow([list: eq-EqualityResult, eq-EqualityResult], eq-EqualityResult),
    "equal-or", t-arrow([list: eq-EqualityResult, eq-EqualityResult], eq-EqualityResult),
    "to-boolean", t-arrow([list: eq-EqualityResult], t-boolean)
  ]),
  SD.make-string-dict()
    .set("EqualityResult", t-data(
      "EqualityResult",
      [list: ],
      [list:
        t-singleton-variant("Equal", [string-dict: ]),
        t-variant("NotEqual", [list: {"reason"; t-string}], [string-dict: ]),
        t-singleton-variant("Unknown", [string-dict: ])],
      [string-dict: ])
    ),
  SD.make-string-dict()
)

module-const-arrays = t-module("builtin://arrays",
  t-record([string-dict:
    "array", t-top,
    "build-array", t-forall([list: tva], t-arrow([list: t-arrow([list: t-number], tva), t-number], mk-array(tva))),
    "array-from-list", t-forall([list: tva], t-arrow([list: mk-list(tva)], mk-array(tva))),
    "is-array", t-forall([list: tva], t-arrow([list: t-top], t-boolean)),
    "array-of", t-forall([list: tva], t-arrow([list: tva, t-number], mk-array(tva))),
    "array-set-now", t-forall([list: tva], t-arrow([list: mk-array(tva), t-number, tva], t-nothing)),
    "array-get-now", t-forall([list: tva], t-arrow([list: mk-array(tva), t-number], tva)),
    "array-length", t-forall([list: tva], t-arrow([list: mk-array(tva)], t-number)),
    "array-to-list-now", t-forall([list: tva], t-arrow([list: mk-array(tva)], mk-list(tva)))
  ]),
  SD.make-string-dict()
    .set("Array", t-data(
        "Array",
        [list: tva],
        [list: ],
        [string-dict:
            "get-now", t-arrow([list: t-number], tva),
            "set-now", t-arrow([list: t-number, tva], t-nothing),
            "to-list-now", t-arrow(empty, mk-list(tva)),
            "length", t-arrow(empty, t-number),
            "_output", t-output
        ])),
  SD.make-string-dict()
    .set("Array", t-big-array)
)

set-constructor =
  t-record([string-dict:
      "make", t-forall([list: tva], t-arrow([list: t-array(tva)], mk-set(tva))),
      "make0", t-forall([list: tva], t-arrow([list: ], mk-set(tva))),
      "make1", t-forall([list: tva], t-arrow([list: tva], mk-set(tva))),
      "make2", t-forall([list: tva], t-arrow([list: tva, tva], mk-set(tva))),
      "make3", t-forall([list: tva], t-arrow([list: tva, tva, tva], mk-set(tva))),
      "make4", t-forall([list: tva], t-arrow([list: tva, tva, tva, tva], mk-set(tva))),
      "make5", t-forall([list: tva], t-arrow([list: tva, tva, tva, tva, tva], mk-set(tva)))
    ])

t-empty-set = t-forall([list: tva], mk-set(tva))

t-list-to-set = t-forall([list: tva], t-arrow([list: mk-list(tva)], mk-set(tva)))

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
    "list-to-tree-set", t-list-to-set
  ]),
  let tv-set = mk-set(tva),
      tv-to-tv = t-arrow([list: tv-set], tv-set),
      tv-arg = [list: tva]:
    SD.make-string-dict()
      .set("Set", t-data(
          "Set",
          [list: tva],
          [list: ],
          [string-dict:
              "length", t-arrow(empty, t-number),
              "pick", t-arrow(empty, t-app(t-name(module-uri("builtin://pick"), A.s-type-global("Pick")), [list: tva, mk-set(tva)])),
              "_output", t-output,
              "fold", t-forall([list: tvb], t-arrow([list: t-arrow([list: tvb, tva], tvb), tvb], tvb)),
              "member", t-arrow([list: tva], t-boolean),
              "add", t-arrow([list: tva], tv-set),
              "remove", t-arrow([list: tva], tv-set),
              "to-list", t-arrow(empty, mk-list(tva)),
              "union", tv-to-tv,
              "intersect", tv-to-tv,
              "difference", tv-to-tv,
              "size", t-arrow(empty, t-number)
        ]))
  end,
  SD.make-string-dict()
    .set("Set", t-set)
)

module-const-lists = t-module("builtin://lists",
  t-record([string-dict:
    "List", t-arrow([list: t-top], t-boolean),
    "is-List", t-arrow([list: t-top], t-boolean),
    "empty", t-forall([list: tva], t-data-refinement(mk-list(tva), "empty")),
    "is-empty", t-arrow([list: t-top], t-boolean),
    "link", t-forall([list: tva], t-arrow([list: tva, mk-list(tva)], t-data-refinement(mk-list(tva), "link"))),
    "is-link", t-arrow([list: t-top], t-boolean),
    "range", t-arrow([list: t-number, t-number], mk-list(t-number)),
    "range-by", t-arrow([list: t-number, t-number, t-number], mk-list(t-number)),
    "repeat", t-forall([list: tva], t-arrow([list: t-number, tva], mk-list(tva))),
    "filter", t-forall([list: tva], t-arrow([list: t-arrow([list: tva], t-boolean), mk-list(tva)], mk-list(tva))),
    "partition", t-forall([list: tva], t-arrow([list: t-arrow([list: tva], t-boolean), mk-list(tva)], t-record([string-dict: "is-true", mk-list(tva), "is-false", mk-list(tva)]))),
    "find", t-forall([list: tva], t-arrow([list: t-arrow([list: tva], t-boolean), mk-list(tva)], t-app(t-name(module-uri("builtin://option"), A.s-type-global("Option")), [list: tva]))),
    "split-at", t-forall([list: tva], t-arrow([list: t-number, mk-list(tva)], t-record([string-dict: "prefix", mk-list(tva), "suffix", mk-list(tva)]))),
    "any", t-forall([list: tva], t-arrow([list: t-arrow([list: tva], t-boolean), mk-list(tva)], t-boolean)),
    "all", t-forall([list: tva], t-arrow([list: t-arrow([list: tva], t-boolean), mk-list(tva)], t-boolean)),
    "all2", t-forall([list: tva, tvb], t-arrow([list: t-arrow([list: tva, tvb], t-boolean), mk-list(tva), mk-list(tvb)], t-boolean)),
    "map", t-forall([list: tva, tvb], t-arrow([list: t-arrow([list: tva], tvb), mk-list(tva)], mk-list(tvb))),
    "map2", t-forall([list: tva, tvb, tvc], t-arrow([list: t-arrow([list: tva, tvb], tvc), mk-list(tva), mk-list(tvb)], mk-list(tvc))),
    "map3", t-forall([list: tva, tvb, tvc, tvd], t-arrow([list: t-arrow([list: tva, tvb, tvc], tvd), mk-list(tva), mk-list(tvb), mk-list(tvc)], mk-list(tvd))),
    "map4", t-forall([list: tva, tvb, tvc, tvd, tve], t-arrow([list: t-arrow([list: tva, tvb, tvc, tvd], tve), mk-list(tva), mk-list(tvb), mk-list(tvc), mk-list(tvd)], mk-list(tve))),
    "map_n", t-forall([list: tva, tvb], t-arrow([list: t-arrow([list: t-number, tva], tvb), t-number, mk-list(tva)], mk-list(tvb))),
    "map2_n", t-forall([list: tva, tvb, tvc], t-arrow([list: t-arrow([list: t-number, tva, tvb], tvc), t-number, mk-list(tva), mk-list(tvb)], mk-list(tvc))),
    "map3_n", t-forall([list: tva, tvb, tvc, tvd], t-arrow([list: t-arrow([list: t-number, tva, tvb, tvc], tvd), t-number, mk-list(tva), mk-list(tvb), mk-list(tvc)], mk-list(tvd))),
    "map4_n", t-forall([list: tva, tvb, tvc, tvd, tve], t-arrow([list: t-arrow([list: t-number, tva, tvb, tvc, tvd], tve), t-number, mk-list(tva), mk-list(tvb), mk-list(tvc), mk-list(tvd)], mk-list(tve))),
    "each", t-forall([list: tva], t-arrow([list: t-arrow([list: tva], t-top), mk-list(tva)], t-name(module-uri("builtin://global"), A.s-type-global("Nothing")))),
    "each2", t-forall([list: tva, tvb], t-arrow([list: t-arrow([list: tva, tvb], t-top), mk-list(tva), mk-list(tvb)], t-name(module-uri("builtin://global"), A.s-type-global("Nothing")))),
    "each3", t-forall([list: tva, tvb, tvc], t-arrow([list: t-arrow([list: tva, tvb, tvc], t-top), mk-list(tva), mk-list(tvb), mk-list(tvc)], t-name(module-uri("builtin://global"), A.s-type-global("Nothing")))),
    "each4", t-forall([list: tva, tvb, tvc, tvd], t-arrow([list: t-arrow([list: tva, tvb, tvc, tvd], t-top), mk-list(tva), mk-list(tvb), mk-list(tvc), mk-list(tvd)], t-name(module-uri("builtin://global"), A.s-type-global("Nothing")))),
    "each_n", t-forall([list: tva], t-arrow([list: t-arrow([list: t-number, tva], t-top), t-number, mk-list(tva)], t-name(module-uri("builtin://global"), A.s-type-global("Nothing")))),
    "each2_n", t-forall([list: tva, tvb], t-arrow([list: t-arrow([list: t-number, tva, tvb], t-top), t-number, mk-list(tva), mk-list(tvb)], t-name(module-uri("builtin://global"), A.s-type-global("Nothing")))),
    "each3_n", t-forall([list: tva, tvb, tvc], t-arrow([list: t-arrow([list: t-number, tva, tvb, tvc], t-top), t-number, mk-list(tva), mk-list(tvb), mk-list(tvc)], t-name(module-uri("builtin://global"), A.s-type-global("Nothing")))),
    "each4_n", t-forall([list: tva, tvb, tvc, tvd], t-arrow([list: t-arrow([list: t-number, tva, tvb, tvc, tvd], t-top), t-number, mk-list(tva), mk-list(tvb), mk-list(tvc), mk-list(tvd)], t-name(module-uri("builtin://global"), A.s-type-global("Nothing")))),
    "fold", t-forall([list: tva, tvb], t-arrow([list: t-arrow([list: tva, tvb], tva), tva, mk-list(tvb)], tva)),
    "fold2", t-forall([list: tva, tvb, tvc], t-arrow([list: t-arrow([list: tva, tvb, tvc], tva), tva, mk-list(tvb), mk-list(tvc)], tva)),
    "fold3", t-forall([list: tva, tvb, tvc, tvd], t-arrow([list: t-arrow([list: tva, tvb, tvc, tvd], tva), tva, mk-list(tvb), mk-list(tvc), mk-list(tvd)], tva)),
    "fold4", t-forall([list: tva, tvb, tvc, tvd, tve], t-arrow([list: t-arrow([list: tva, tvb, tvc, tvd, tve], tva), tva, mk-list(tvb), mk-list(tvc), mk-list(tvd), mk-list(tve)], tva)),
    "fold_n", t-forall([list: tva, tvb], t-arrow([list: t-arrow([list: t-number, tva, tvb], tva), t-number, tva, mk-list(tvb)], tva)),
    "length", t-forall([list: tva], t-arrow([list: mk-list(tva)], t-number)),
    "sum", t-arrow([list: mk-list(t-number)], t-number),
    "max", t-arrow([list: mk-list(t-number)], t-number),
    "min", t-arrow([list: mk-list(t-number)], t-number),
    "mean", t-arrow([list: mk-list(t-number)], t-number),
    "median", t-arrow([list: mk-list(t-number)], t-number),
    "stdev", t-arrow([list: mk-list(t-number)], t-number),
    "distinct", t-forall([list: tva], t-arrow([list: mk-list(tva)], mk-list(tva))),
    "list",
        t-record([string-dict:
              "make", t-forall([list: tva], t-arrow([list: t-array(tva)], mk-list(tva))),
              "make0", t-forall([list: tva], t-arrow([list: ], t-data-refinement(mk-list(tva), "empty"))),
              "make1", t-forall([list: tva], t-arrow([list: tva], t-data-refinement(mk-list(tva), "link"))),
              "make2", t-forall([list: tva], t-arrow([list: tva, tva], t-data-refinement(mk-list(tva), "link"))),
              "make3", t-forall([list: tva], t-arrow([list: tva, tva, tva], t-data-refinement(mk-list(tva), "link"))),
              "make4", t-forall([list: tva], t-arrow([list: tva, tva, tva, tva], t-data-refinement(mk-list(tva), "link"))),
              "make5", t-forall([list: tva], t-arrow([list: tva, tva, tva, tva, tva], t-data-refinement(mk-list(tva), "link")))
            ])
  ]),
  let lotv = mk-list(tva),
      tv-arg = [list: tva]:
    SD.make-string-dict()
      .set("List", t-data(
          "List",
          [list: tva],
          [list:
            t-singleton-variant("empty", [string-dict: ]),
            t-variant("link", [list: {"first"; tva}, {"rest"; mk-list(tva)}], [string-dict: ])
          ],
          [string-dict:
            "join-str", t-arrow([list: t-string], t-string),
            "sort", t-arrow(empty, lotv),
            "sort-by", t-arrow([list: t-arrow([list: tva, tva], t-boolean), t-arrow([list: tva, tva], t-boolean)], lotv),
            "reverse", t-arrow(empty, lotv),
            "last", t-arrow(empty, tva),
            "append", t-arrow([list: lotv], lotv),
            "foldl", t-forall([list: tva, tvb], t-arrow([list: t-arrow([list: tva, tvb], tvb), tvb], tvb)),
            "foldr", t-forall([list: tvb], t-arrow([list: t-arrow([list: tva, tvb], tvb), tvb], tvb)),
            "member", t-arrow(tv-arg, t-boolean),
            "filter", t-arrow([list: t-arrow([list: tva], t-boolean)], lotv),
            "map", t-forall([list: tvb], t-arrow([list: t-arrow([list: tva], tvb)], mk-list(tvb))),
            "each", t-arrow([list: t-arrow([list: tva], t-top)], t-nothing),
            "length", t-arrow(empty, t-number),
            "_output", t-output,
            "_match", t-top,
            "_plus", t-arrow([list: lotv], lotv),
            "push", t-arrow([list: ], lotv),
            "split-at", t-arrow([list: t-number], t-record([string-dict:
              "prefix", lotv,
              "suffix", lotv
            ])),
            "take", t-arrow([list: t-number], lotv),
            "drop", t-arrow([list: t-number], lotv),
            "get", t-arrow([list: t-number], tva),
            "set", t-arrow([list: t-number, tva], lotv)
        ]))
  end,
  SD.make-string-dict()
    .set("List", t-name(module-uri("builtin://lists"), A.s-type-global("List"))))

t-and-then =
  t-forall(
    [list: tva],
    t-arrow(
      [list:
        t-arrow([list: tva], t-option(tvb))
      ],
      t-option(tvb)))

module-const-option = t-module("builtin://option",
  t-record([string-dict:
    "Option", t-arrow([list: t-top], t-boolean),
    "is-Option", t-arrow([list: t-top], t-boolean),
    "none", t-forall([list: tva], t-data-refinement(t-option(tva), "none")),
    "is-none", t-arrow([list: t-top], t-boolean),
    "some", t-forall([list: tva], t-arrow([list: tva], t-data-refinement(t-option(tva), "some"))),
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
    .set("Option", t-name(module-uri("builtin://option"), A.s-type-global("Option")))
)

t-runtime-error = t-name(module-uri("builtin://error"), A.s-type-global("RuntimeError"))
t-parse-error = t-name(module-uri("builtin://error"), A.s-type-global("ParseError"))

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
    "cases-arity-mismatch", t-arrow([list: t-top, t-top, t-top, t-top], t-runtime-error),
    "is-cases-arity-mismatch", t-arrow([list: t-top], t-boolean),
    "cases-singleton-mismatch", t-arrow([list: t-top, t-boolean, t-top], t-runtime-error),
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

fun mk-either(typ-1 :: Type, typ-2 :: Type):
  t-app(t-name(module-uri("builtin://either"), A.s-type-global("Either")), [list: typ-1, typ-2])
end

module-const-either =
  t-module("pyret-builtin://either",
    t-record([string-dict:
      "Either", t-arrow([list: t-top], t-boolean),
      "is-Either", t-arrow([list: t-top], t-boolean),
      "left", t-forall([list: tva, tvb], t-arrow([list: tva], t-data-refinement(t-app(t-name(module-uri("builtin://either"), A.s-type-global("Either")), [list: tva, tvb]), "left"))),
      "is-left", t-arrow([list: t-top], t-boolean),
      "right", t-forall([list: tva, tvb], t-arrow([list: tvb], t-data-refinement(t-app(t-name(module-uri("builtin://either"), A.s-type-global("Either")), [list: tva, tvb]), "right"))),
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
      .set("Either", t-name(module-uri("builtin://either"), A.s-type-global("Either"))))

t-s-exp = t-name(module-uri("builtin://s-exp-structs"), A.s-type-global("S-Exp"))

s-exp-struct-mems = [string-dict:
  "s-list", t-arrow([list: mk-list(t-s-exp)], t-s-exp),
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
            {"exps"; mk-list(t-s-exp)}
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

fun make-default-modules() block:
  default-modules = SD.make-mutable-string-dict()
  default-modules.set-now("builtin://equality", module-const-equality)
  default-modules.set-now("builtin://lists", module-const-lists)
  default-modules.set-now("builtin://option", module-const-option)
  default-modules.set-now("builtin://error", module-const-error)
  default-modules.set-now("builtin://either", module-const-either)
  default-modules.set-now("builtin://arrays", module-const-arrays)
  default-modules.set-now("builtin://sets", module-const-sets)
  default-modules.set-now("builtin://s-exp", module-const-s-exp)
  default-modules.set-now("builtin://s-exp-structs", module-const-s-exp-structs)
  default-modules.freeze()
end
