provide *
provide-types *

import ast as A
import string-dict as SD
import srcloc as SL
import "compiler/type-structs.arr" as TS

type Type                 = TS.Type
t-name                    = TS.t-name(_, _, A.dummy-loc)
t-var                     = TS.t-var(_, A.dummy-loc)
t-arrow                   = TS.t-arrow(_, _, A.dummy-loc)
t-top                     = TS.t-top(A.dummy-loc)
t-bot                     = TS.t-bot(A.dummy-loc)
t-app                     = TS.t-app(_, _, A.dummy-loc)
t-record                  = TS.t-record(_, A.dummy-loc)
t-forall                  = TS.t-forall(_, _, A.dummy-loc)
t-data                    = TS.t-data(_, _, _, A.dummy-loc)

t-number                  = TS.t-number(A.dummy-loc)
t-string                  = TS.t-string(A.dummy-loc)
t-boolean                 = TS.t-boolean(A.dummy-loc)
t-array                   = TS.t-array(_, A.dummy-loc)
t-nothing                 = TS.t-nothing(A.dummy-loc)
t-srcloc                  = TS.t-srcloc(A.dummy-loc)

type Variance             = TS.Variance
constant                  = TS.constant
invariant                 = TS.invariant
covariant                 = TS.covariant
contravariant             = TS.contravariant

type TypeMember           = TS.TypeMember
t-member                  = TS.t-member(_, _, A.dummy-loc)

type ModuleType           = TS.ModuleType
t-module                  = TS.t-module

type TypeVariant          = TS.TypeVariant
t-variant                 = TS.t-variant(_, _, _, A.dummy-loc)
t-singleton-variant       = TS.t-singleton-variant(_, _, A.dummy-loc)

s-atom                    = A.s-atom

t-number-binop = t-arrow([list: t-number, t-number], t-number)

fun make-default-aliases():
  default-aliases = [SD.mutable-string-dict:
    A.s-type-global("Number").key(), t-number,
    A.s-type-global("String").key(), t-string,
    A.s-type-global("Boolean").key(), t-boolean]
  default-aliases
end

fun make-default-typs():
  default-typs = SD.make-mutable-string-dict()
  default-typs.set-now(A.s-global("builtins").key(), t-record([list:
      t-member("has-field", t-arrow([list: t-record(empty)], t-boolean)),
      t-member("current-checker", t-arrow([list: ], t-record([list: # Cheat on these types for now.
          t-member("run-checks", t-bot),
          t-member("check-is", t-bot),
          t-member("check-is-refinement", t-bot),
          t-member("check-is-not", t-bot),
          t-member("check-is-not-refinement", t-bot),
          t-member("check-is-refinement", t-bot),
          t-member("check-is-not-refinement", t-bot),
          t-member("check-satisfies", t-bot),
          t-member("check-satisfies-not", t-bot),
          t-member("check-raises-str", t-bot),
          t-member("check-raises-not", t-bot),
          t-member("check-raises-other-str", t-bot),
          t-member("check-raises-satisfies", t-bot),
          t-member("check-raises-violates" , t-bot)
      ])))
  ]))

  # Need to be fixed to correct type:
  default-typs.set-now(A.s-global("raw-array-get").key(), t-top)
  default-typs.set-now(A.s-global("raw-array-set").key(), t-top)
  default-typs.set-now(A.s-global("raw-array-of").key(), t-top)
  default-typs.set-now(A.s-global("raw-array-length").key(), t-top)
  default-typs.set-now(A.s-global("raw-array-to-list").key(), t-top)
  default-typs.set-now(A.s-global("raw-array-fold").key(), t-top)
  default-typs.set-now(A.s-global("raw-array").key(), t-top)
  default-typs.set-now(A.s-global("ref-get").key(), t-top)
  default-typs.set-now(A.s-global("ref-set").key(), t-top)
  default-typs.set-now(A.s-global("ref-freeze").key(), t-top)
  default-typs.set-now(A.s-global("equal-always3").key(), t-top)
  default-typs.set-now(A.s-global("equal-now3").key(), t-top)
  default-typs.set-now(A.s-global("identical3").key(), t-top)
  default-typs.set-now(A.s-global("exn-unwrap").key(), t-top)
  default-typs.set-now(A.s-global("test-print").key(), t-top)
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
  default-typs.set-now(A.s-global("nothing").key(), t-name(none, A.s-type-global("Nothing")))
  default-typs.set-now("isBoolean", t-arrow([list: t-top], t-boolean))
  default-typs.set-now(A.s-global("torepr").key(), t-arrow([list: t-top], t-string))
  default-typs.set-now("checkWrapBoolean", t-arrow([list: t-boolean], t-boolean))
  default-typs.set-now("throwNonBooleanCondition", t-arrow([list: t-srcloc, t-string, t-top], t-bot))
  default-typs.set-now("throwNoBranchesMatched", t-arrow([list: t-srcloc, t-string], t-bot))
  default-typs.set-now("not", t-arrow([list: t-boolean], t-boolean))
  default-typs.set-now(A.s-global("not").key(), t-arrow([list: t-boolean], t-boolean))
  default-typs.set-now(A.s-global("raise").key(), t-arrow([list: t-top], t-bot))
  default-typs.set-now(A.s-global("equal-always").key(), t-arrow([list: t-top, t-top], t-boolean))
  default-typs.set-now(A.s-global("equal-now").key(), t-arrow([list: t-top, t-top], t-boolean))
  default-typs.set-now(A.s-global("identical").key(), t-arrow([list: t-top, t-top], t-boolean))
  default-typs.set-now("hasField", t-arrow([list: t-record(empty), t-string], t-boolean))
  default-typs.set-now(A.s-global("tostring").key(), t-arrow([list: t-top], t-string))
  default-typs.set-now(A.s-global("_times").key(), t-number-binop)
  default-typs.set-now(A.s-global("_minus").key(), t-number-binop)
  default-typs.set-now(A.s-global("_divide").key(), t-number-binop)
  default-typs.set-now(A.s-global("_plus").key(), t-number-binop)
  default-typs.set-now("makeSrcloc", t-arrow([list: t-srcloc], t-bot))
  default-typs.set-now(A.s-global("is-nothing").key(), t-arrow([list: t-top], t-boolean))
  default-typs.set-now(A.s-global("is-number").key(), t-arrow([list: t-top], t-boolean))
  default-typs.set-now(A.s-global("is-string").key(), t-arrow([list: t-top], t-boolean))
  default-typs.set-now(A.s-global("is-boolean").key(), t-arrow([list: t-top], t-boolean))
  default-typs.set-now(A.s-global("is-object").key(), t-arrow([list: t-top], t-boolean))
  default-typs.set-now(A.s-global("is-function").key(), t-arrow([list: t-top], t-boolean))
  default-typs.set-now(A.s-global("is-raw-array").key(), t-arrow([list: t-top], t-boolean))

  # Number functions
  default-typs.set-now(A.s-global("num-max").key(), t-number-binop)
  default-typs.set-now(A.s-global("num-min").key(), t-number-binop)
  default-typs.set-now(A.s-global("num-equal").key(), t-number-binop)
  default-typs.set-now(A.s-global("num-within").key(), t-arrow([list: t-number], t-arrow([list: t-number, t-number], t-boolean)))
  default-typs.set-now(A.s-global("num-within-abs").key(), t-arrow([list: t-number], t-arrow([list: t-number, t-number], t-boolean)))
  default-typs.set-now(A.s-global("num-within-rel").key(), t-arrow([list: t-number], t-arrow([list: t-number, t-number], t-boolean)))
  default-typs.set-now(A.s-global("num-abs").key(), t-arrow([list: t-number], t-number))
  default-typs.set-now(A.s-global("num-sin").key(), t-arrow([list: t-number], t-number))
  default-typs.set-now(A.s-global("num-cos").key(), t-arrow([list: t-number], t-number))
  default-typs.set-now(A.s-global("num-tan").key(), t-arrow([list: t-number], t-number))
  default-typs.set-now(A.s-global("num-asin").key(), t-arrow([list: t-number], t-number))
  default-typs.set-now(A.s-global("num-acos").key(), t-arrow([list: t-number], t-number))
  default-typs.set-now(A.s-global("num-atan").key(), t-arrow([list: t-number], t-number))
  default-typs.set-now(A.s-global("num-modulo").key(), t-number-binop)
  default-typs.set-now(A.s-global("num-truncate").key(), t-arrow([list: t-number], t-number))
  default-typs.set-now(A.s-global("num-sqrt").key(), t-arrow([list: t-number], t-number))
  default-typs.set-now(A.s-global("num-sqr").key(), t-arrow([list: t-number], t-number))
  default-typs.set-now(A.s-global("num-ceiling").key(), t-arrow([list: t-number], t-number))
  default-typs.set-now(A.s-global("num-floor").key(), t-arrow([list: t-number], t-number))
  default-typs.set-now(A.s-global("num-round").key(), t-arrow([list: t-number], t-number))
  default-typs.set-now(A.s-global("num-round-even").key(), t-arrow([list: t-number], t-number))
  default-typs.set-now(A.s-global("num-log").key(), t-arrow([list: t-number], t-number))
  default-typs.set-now(A.s-global("num-exp").key(), t-arrow([list: t-number], t-number))
  default-typs.set-now(A.s-global("num-exact").key(), t-arrow([list: t-number], t-number))
  default-typs.set-now(A.s-global("num-to-rational").key(), t-arrow([list: t-number], t-number))
  default-typs.set-now(A.s-global("num-to-roughnum").key(), t-arrow([list: t-number], t-number))
  default-typs.set-now(A.s-global("num-to-fixnum").key(), t-arrow([list: t-number], t-number))
  default-typs.set-now(A.s-global("num-is-integer").key(), t-arrow([list: t-number], t-number))
  default-typs.set-now(A.s-global("num-is-rational").key(), t-arrow([list: t-number], t-number))
  default-typs.set-now(A.s-global("num-is-roughnum").key(), t-arrow([list: t-number], t-number))
  default-typs.set-now(A.s-global("num-is-positive").key(), t-arrow([list: t-number], t-number))
  default-typs.set-now(A.s-global("num-is-negative").key(), t-arrow([list: t-number], t-number))
  default-typs.set-now(A.s-global("num-is-non-positive").key(), t-arrow([list: t-number], t-number))
  default-typs.set-now(A.s-global("num-is-non-negative").key(), t-arrow([list: t-number], t-number))
  default-typs.set-now(A.s-global("num-is-fixnum").key(), t-arrow([list: t-number], t-number))
  default-typs.set-now(A.s-global("num-expt").key(), t-number-binop)
  default-typs.set-now(A.s-global("num-tostring").key(), t-arrow([list: t-number], t-string))
  default-typs.set-now(A.s-global("num-to-string").key(), t-arrow([list: t-number], t-string))
  default-typs.set-now(A.s-global("num-to-string-digits").key(), t-arrow([list: t-number, t-number], t-string))
  default-typs.set-now(A.s-global("within").key(), t-arrow([list: t-number], t-arrow([list: t-top, t-top], t-boolean)))
  default-typs.set-now(A.s-global("within-abs").key(), t-arrow([list: t-number], t-arrow([list: t-top, t-top], t-boolean)))
  default-typs.set-now(A.s-global("within-rel").key(), t-arrow([list: t-number], t-arrow([list: t-top, t-top], t-boolean)))
  default-typs.set-now(A.s-global("within-now").key(), t-arrow([list: t-number], t-arrow([list: t-top, t-top], t-boolean)))
  default-typs.set-now(A.s-global("within-abs-now").key(), t-arrow([list: t-number], t-arrow([list: t-top, t-top], t-boolean)))
  default-typs.set-now(A.s-global("within-rel-now").key(), t-arrow([list: t-number], t-arrow([list: t-top, t-top], t-boolean)))
  default-typs.set-now(A.s-global("random").key(), t-arrow([list: t-number], t-number))
  default-typs.set-now(A.s-global("num-random").key(), t-arrow([list: t-number], t-number))
  default-typs.set-now(A.s-global("num-random-seed").key(), t-arrow([list: t-number], t-nothing))

  # Time functions
  default-typs.set-now(A.s-global("time-now").key(), t-arrow(empty, t-number))

  # String functions
  default-typs.set-now(A.s-global("gensym").key(), t-arrow(empty, t-string))
  default-typs.set-now(A.s-global("string-repeat").key(), t-arrow([list: t-string, t-number], t-string))
  default-typs.set-now(A.s-global("string-substring").key(), t-arrow([list: t-string, t-number, t-number], t-string))
  default-typs.set-now(A.s-global("string-toupper").key(), t-arrow([list: t-string], t-string))
  default-typs.set-now(A.s-global("string-tolower").key(), t-arrow([list: t-string], t-string))
  default-typs.set-now(A.s-global("string-append").key(), t-arrow([list: t-string, t-string], t-string))
  default-typs.set-now(A.s-global("string-equal").key(), t-arrow([list: t-string, t-string], t-boolean))
  default-typs.set-now(A.s-global("string-contains").key(), t-arrow([list: t-string, t-string], t-boolean))
  default-typs.set-now(A.s-global("string-to-number").key(), t-arrow([list: t-string], t-number))
  default-typs.set-now(A.s-global("string-tonumber").key(), t-arrow([list: t-string], t-number))
  default-typs.set-now(A.s-global("string-length").key(), t-arrow([list: t-string], t-number))
  default-typs.set-now(A.s-global("string-replace").key(), t-arrow([list: t-string, t-string, t-string], t-string))
  default-typs.set-now(A.s-global("string-char-at").key(), t-arrow([list: t-string, t-number], t-string))
  default-typs.set-now(A.s-global("string-to-code-point").key(), t-arrow([list: t-string], t-number))
  default-typs.set-now(A.s-global("string-from-code-point").key(), t-arrow([list: t-number], t-string))

  default-typs.set-now(A.s-global("_lessthan").key(), t-number-binop)
  default-typs.set-now(A.s-global("_lessequal").key(), t-number-binop)
  default-typs.set-now(A.s-global("_greaterthan").key(), t-number-binop)
  default-typs.set-now(A.s-global("_greaterequal").key(), t-number-binop)
  print-variable = A.s-atom(gensym("A"), 1)
  default-typs.set-now(A.s-global("print").key(), t-forall([list: t-var(print-variable)], t-arrow([list: t-var(print-variable)], t-var(print-variable))))
  default-typs.set-now(A.s-global("display").key(), t-forall([list: t-var(print-variable)], t-arrow([list: t-var(print-variable)], t-var(print-variable))))

  default-typs
end

# TODO(MATT): what do these mean
#fun make-default-data-exprs():
#  default-data-exprs = SD.make-mutable-string-dict()
#  default-data-exprs.set-now(A.s-type-global("RawArray").key(),
#    # RawArray is invariant because it can be mutated
#    t-datatype("RawArray", [list: t-var(s-atom("A", 10))], empty, empty))
#  default-data-exprs.set-now(A.s-type-global("Number").key(),
#    t-datatype("Number", empty, empty, empty))
#  default-data-exprs.set-now(A.s-type-global("String").key(),
#    t-datatype("String", empty, empty, empty))
#  default-data-exprs.set-now(A.s-type-global("Boolean").key(),
#    t-datatype("Boolean", empty, empty, empty))
#  default-data-exprs.set-now(A.s-type-global("Nothing").key(),
#    t-datatype("Nothing", empty, empty, empty))
#  default-data-exprs
#end

fun make-default-data-exprs():
  default-data-exprs = SD.make-mutable-string-dict()
  default-data-exprs.set-now(A.s-type-global("RawArray").key(),
    t-data([list: t-var(s-atom("A", 10))], empty, empty))
  default-data-exprs.set-now(A.s-type-global("Number").key(),
    t-data(empty, empty, empty))
  default-data-exprs.set-now(A.s-type-global("String").key(),
    t-data(empty, empty, empty))
  default-data-exprs.set-now(A.s-type-global("Boolean").key(),
    t-data(empty, empty, empty))
  default-data-exprs.set-now(A.s-type-global("Nothing").key(),
    t-data(empty, empty, empty))
  default-data-exprs
end

# Begin hard-coded module types
rec t-list = t-name(some("pyret-builtin://lists"), A.s-global("List"))
fun mk-list(a :: Type) -> Type:
  t-app(t-list, [list: a])
end

t-big-array = t-name(some("pyret-builtin://arrays"), A.s-global("Array"))
fun mk-array(typ :: Type):
  t-app(t-big-array, [list: typ])
end

t-set = t-name(some("pyret-builtin://sets"), A.s-global("Set"))
fun mk-set(typ :: Type):
  t-app(t-set, [list: typ])
end

t-torepr   = t-arrow([list: ], t-string)
t-tostring = t-arrow([list: ], t-string)

eq-EqualityResult = t-name(some("pyret-builtin://equality"), A.s-global("EqualityResult"))

# Functions for adding hard-coded modules
module-const-equality = t-module("pyret-builtin://equality",
  t-record([list:
    t-member("EqualityResult", t-arrow([list: t-top], t-boolean)),
    t-member("is-EqualityResult", t-arrow([list: t-top], t-boolean)),
    t-member("Equal", eq-EqualityResult),
    t-member("is-Equal", t-arrow([list: t-top], t-boolean)),
    t-member("NotEqual", t-arrow([list: t-string], eq-EqualityResult)),
    t-member("is-NotEqual", t-arrow([list: t-top], t-boolean)),
    t-member("Unknown", eq-EqualityResult),
    t-member("is-Unknown", t-arrow([list: t-top], t-boolean)),
    t-member("equal-and", t-arrow([list: eq-EqualityResult, eq-EqualityResult], eq-EqualityResult)),
    t-member("equal-or", t-arrow([list: eq-EqualityResult, eq-EqualityResult], eq-EqualityResult)),
    t-member("to-boolean", t-arrow([list: eq-EqualityResult], t-boolean)),
    t-member("from-boolean", t-arrow([list: t-boolean], eq-EqualityResult))
  ]),
  SD.make-string-dict()
    .set("EqualityResult", t-data(
      [list: ],
      [list:
        t-singleton-variant("Equal", [list: ]),
        t-variant("NotEqual", [list: t-member("reason", t-string)], [list: ]),
        t-singleton-variant("Unknown", [list: ])],
      [list: ])
    ),
  SD.make-string-dict()
)

module-const-arrays = t-module("pyret-builtin://arrays",
  t-record([list:
    t-member("array", let tv = t-var(s-atom("A", 1)):
        t-top
    end),
    t-member("build-array", let tva = s-atom("A", 2), tv = t-var(tva):
        t-forall([list: t-var(tva)], t-arrow([list: t-arrow([list: t-number], tv), t-number], mk-array(tv)))
    end),
    t-member("array-from-list", let tva = s-atom("A", 3), tv = t-var(tva):
        t-forall([list: t-var(tva)], t-arrow([list: mk-list(tv)], mk-array(tv)))
    end),
    t-member("is-array", let tva = s-atom("A", 4), tv = t-var(tva):
        t-forall([list: t-var(tva)], t-arrow([list: t-top], t-boolean))
    end),
    t-member("array-of", let tva = s-atom("A", 5), tv = t-var(tva):
        t-forall([list: t-var(tva)], t-arrow([list: tv, t-number], mk-array(tv)))
    end),
    t-member("array-set-now", let tva = s-atom("A", 6), tv = t-var(tva):
        t-forall([list: t-var(tva)], t-arrow([list: mk-array(tv), t-number, tv], t-nothing))
    end),
    t-member("array-get-now", let tva = s-atom("A", 7), tv = t-var(tva):
        t-forall([list: t-var(tva)], t-arrow([list: mk-array(tv), t-number], tv))
    end),
    t-member("array-length", let tva = s-atom("A", 8), tv = t-var(tva):
      t-forall([list: t-var(tva)], t-arrow([list: mk-array(tv)], t-number))
    end),
    t-member("array-to-list-now", let tva = s-atom("A", 9), tv = t-var(tva):
      t-forall([list: t-var(tva)], t-arrow([list: mk-array(tv)], mk-list(tv)))
    end)
  ]),
  let tva = s-atom("A", 10),
      tv = t-var(tva),
      tv-arg = [list: tv]:
    SD.make-string-dict()
      .set("Array", t-data(
        [list: t-var(tva)],
        [list: ],
        [list:
            t-member("get-now", t-arrow([list: t-number], tv)),
            t-member("set-now", t-arrow([list: t-number, tv], t-nothing)),
            t-member("to-list-now", t-arrow(empty, mk-list(tv))),
            t-member("length", t-arrow(empty, t-number)),
            t-member("_torepr", t-torepr),
            t-member("_tostring", t-tostring)
        ])
      )
  end,
  SD.make-string-dict()
    .set("Array", t-name(none, A.s-name(A.dummy-loc, "Array")))
)

fun set-constructor(tva :: A.Name):
  tv = t-var(tva)
  t-record([list:
      t-member("make", t-forall([list: tv], t-arrow([list: t-array(tv)], mk-set(tv)))),
      t-member("make0", t-forall([list: tv], t-arrow([list: ], mk-set(tv)))),
      t-member("make1", t-forall([list: tv], t-arrow([list: tv], mk-set(tv)))),
      t-member("make2", t-forall([list: tv], t-arrow([list: tv, tv], mk-set(tv)))),
      t-member("make3", t-forall([list: tv], t-arrow([list: tv, tv, tv], mk-set(tv)))),
      t-member("make4", t-forall([list: tv], t-arrow([list: tv, tv, tv, tv], mk-set(tv)))),
      t-member("make5", t-forall([list: tv], t-arrow([list: tv, tv, tv, tv, tv], mk-set(tv))))
    ])
end

fun mk-empty-set(tva :: A.Name):
  tv = t-var(tva)
  t-forall([list: t-var(tva)], mk-set(tv))
end

fun mk-list-to-set(tva :: A.Name):
  tv = t-var(tva)
  t-forall([list: t-var(tva)], t-arrow([list: mk-list(tv)], mk-set(tv)))
end

module-const-sets = t-module("pyret-builtin://sets",
  t-record([list:
    t-member("set", set-constructor(s-atom("A", 11))),
    t-member("list-set", set-constructor(s-atom("A", 12))),
    t-member("tree-set", set-constructor(s-atom("A", 13))),
    t-member("empty-set", mk-empty-set(s-atom("A", 14))),
    t-member("empty-list-set", mk-empty-set(s-atom("A", 15))),
    t-member("empty-tree-set", mk-empty-set(s-atom("A", 16))),
    t-member("list-to-set", mk-list-to-set(s-atom("A", 17))),
    t-member("list-to-list-set", mk-list-to-set(s-atom("A", 18))),
    t-member("list-to-tree-set", mk-list-to-set(s-atom("A", 19)))
  ]),
  let tva = s-atom("A", 20),
      tv = t-var(tva),
      tv-set = mk-set(tv),
      tv-to-tv = t-arrow([list: tv-set], tv-set),
      tv-arg = [list: tv]:
    SD.make-string-dict()
      .set("Set", t-data(
        [list: t-var(tva)],
        [list: ],
        [list:
            t-member("length", t-arrow(empty, t-number)),
            t-member("pick", t-arrow(empty, t-app(t-name(some("pyret-builtin://pick"), A.s-global("Pick")), [list: tv, mk-list(tv)]))),
            t-member("_torepr", t-torepr),
            t-member("fold", let otva = s-atom("B", 21),
                                 otv  = t-var(otva):
              t-arrow([list: t-arrow(tv-arg, otv), otv], otv)
            end),
            t-member("member", t-arrow([list: tv], t-boolean)),
            t-member("add", t-arrow([list: tv], tv-set)),
            t-member("remove", t-arrow([list: tv], tv-set)),
            t-member("to-list", t-arrow(empty, mk-list(tv))),
            t-member("union", tv-to-tv),
            t-member("intersect", tv-to-tv),
            t-member("difference", tv-to-tv),
            t-member("size", t-arrow(empty, t-number))
        ])
      )
  end,
  SD.make-string-dict()
    .set("Set", t-name(none, A.s-name(A.dummy-loc, "Set")))
)

module-const-lists = t-module("pyret-builtin://lists",
  t-record([list:
    t-member("List", t-arrow([list: t-top], t-boolean)),
    t-member("is-List", t-arrow([list: t-top], t-boolean)),
    t-member("empty", t-forall([list: t-var(s-atom("A", 37))], mk-list(t-var(s-atom("A", 37))))),
    t-member("is-empty", t-arrow([list: t-top], t-boolean)),
    t-member("link", t-forall([list: t-var(s-atom("A", 37))], t-arrow([list: t-var(s-atom("A", 37)), mk-list(t-var(s-atom("A", 37)))], mk-list(t-var(s-atom("A", 37)))))),
    t-member("is-link", t-arrow([list: t-top], t-boolean)),
    t-member("range", t-arrow([list: t-number, t-number], mk-list(t-number))),
    t-member("range-by", t-arrow([list: t-number, t-number, t-number], mk-list(t-number))),
    t-member("repeat", t-forall([list: t-var(s-atom("A", 157))], t-arrow([list: t-number, t-var(s-atom("A", 157))], mk-list(t-var(s-atom("A", 157)))))),
    t-member("filter", t-forall([list: t-var(s-atom("A", 160))], t-arrow([list: t-arrow([list: t-var(s-atom("A", 160))], t-boolean), mk-list(t-var(s-atom("A", 160)))], mk-list(t-top)))),
    t-member("partition", t-forall([list: t-var(s-atom("A", 165))], t-arrow([list: t-arrow([list: t-var(s-atom("A", 165))], t-boolean), mk-list(t-var(s-atom("A", 165)))], t-record([list: t-member("is-true", mk-list(t-var(s-atom("A", 165)))), t-member("is-false", mk-list(t-var(s-atom("A", 165))))])))),
    t-member("find", t-forall([list: t-var(s-atom("A", 174))], t-arrow([list: t-arrow([list: t-var(s-atom("A", 174))], t-boolean), mk-list(t-var(s-atom("A", 174)))], t-app(t-name(some("pyret-builtin://option"), A.s-global("Option")), [list: t-var(s-atom("A", 174))])))),
    t-member("split-at", t-forall([list: t-var(s-atom("A", 179))], t-arrow([list: t-number, mk-list(t-var(s-atom("A", 179)))], t-record([list: t-member("prefix", mk-list(t-var(s-atom("A", 179)))), t-member("suffix", mk-list(t-var(s-atom("A", 179))))])))),
    t-member("any", t-forall([list: t-var(s-atom("A", 189))], t-arrow([list: t-arrow([list: t-var(s-atom("A", 189))], t-boolean), mk-list(t-var(s-atom("A", 189)))], t-boolean))),
    t-member("all", t-forall([list: t-var(s-atom("A", 192))], t-arrow([list: t-arrow([list: t-var(s-atom("A", 192))], t-boolean), mk-list(t-var(s-atom("A", 192)))], t-boolean))),
    t-member("all2", t-forall([list: t-var(s-atom("A", 199)), t-var(s-atom("B", 200))], t-arrow([list: t-arrow([list: t-var(s-atom("A", 199)), t-var(s-atom("B", 200))], t-boolean), mk-list(t-var(s-atom("A", 199))), mk-list(t-var(s-atom("B", 200)))], t-boolean))),
    t-member("map", t-forall([list: t-var(s-atom("A", 211)), t-var(s-atom("B", 212))], t-arrow([list: t-arrow([list: t-var(s-atom("A", 211))], t-var(s-atom("B", 212))), mk-list(t-var(s-atom("A", 211)))], mk-list(t-var(s-atom("B", 212)))))),
    t-member("map2", t-top),
    t-member("map3", t-top),
    t-member("map4", t-top),
    t-member("map_n", t-top),
    t-member("map2_n", t-top),
    t-member("map3_n", t-top),
    t-member("map4_n", t-top),
    t-member("each", t-forall([list: t-var(s-atom("A", 217))], t-arrow([list: t-arrow([list: t-var(s-atom("A", 217))], t-top), mk-list(t-var(s-atom("A", 217)))], t-name(none, A.s-type-global("Nothing"))))),
    t-member("each2", t-top),
    t-member("each3", t-top),
    t-member("each4", t-top),
    t-member("each_n", t-top),
    t-member("each2_n", t-top),
    t-member("each3_n", t-top),
    t-member("each4_n", t-top),
    t-member("fold", t-forall([list: t-var(s-atom("A", 224)), t-var(s-atom("B", 225))], t-arrow([list: t-arrow([list: t-var(s-atom("B", 225)), t-var(s-atom("A", 224))], t-var(s-atom("B", 225))), t-var(s-atom("B", 225)), mk-list(t-var(s-atom("A", 224)))], t-var(s-atom("B", 225))))),
    t-member("fold2", t-top),
    t-member("fold3", t-top),
    t-member("fold4", t-top),
    t-member("fold_n", t-top),
    t-member("list", let tva = s-atom("A", 160), tv = t-var(tva):
        t-record([list:
              t-member("make", t-forall([list: tv], t-arrow([list: t-array(tv)], mk-list(tv)))),
              t-member("make0", t-forall([list: tv], t-arrow([list: ], mk-list(tv)))),
              t-member("make1", t-forall([list: tv], t-arrow([list: tv], mk-list(tv)))),
              t-member("make2", t-forall([list: tv], t-arrow([list: tv, tv], mk-list(tv)))),
              t-member("make3", t-forall([list: tv], t-arrow([list: tv, tv, tv], mk-list(tv)))),
              t-member("make4", t-forall([list: tv], t-arrow([list: tv, tv, tv, tv], mk-list(tv)))),
              t-member("make5", t-forall([list: tv], t-arrow([list: tv, tv, tv, tv, tv], mk-list(tv))))
            ])
    end)
  ]),
  let tv = t-var(s-atom("A", 37)),
      lotv = mk-list(tv),
      tv-arg = [list: tv]:
    SD.make-string-dict()
      .set("List", t-data(
        [list:
          t-var(s-atom("A", 37))
        ],
        [list:
          t-singleton-variant("empty", empty),
          t-variant("link", [list: t-member("first", tv), t-member("rest", mk-list(tv))], empty)
        ],
        [list:
          t-member("join-str", t-arrow([list: t-string], t-string)),
          t-member("sort", t-arrow(empty, lotv)),
          t-member("sort-by", t-arrow([list: t-arrow([list: tv, tv], t-boolean), t-arrow([list: tv, tv], t-boolean)], lotv)),
          t-member("_tostring", t-tostring),
          t-member("reverse", t-arrow(empty, lotv)),
          t-member("last", t-arrow(empty, tv)),
          t-member("append", t-arrow([list: lotv], lotv)),
          t-member("foldl", let tb = s-atom("B", 200): t-forall([list: t-var(tb)], t-arrow([list: t-arrow([list: tv, t-var(tb)], t-var(tb))], t-var(tb)));),
          t-member("foldr", let tb = s-atom("B", 201): t-forall([list: t-var(tb)], t-arrow([list: t-arrow([list: tv, t-var(tb)], t-var(tb))], t-var(tb)));),
          t-member("member", t-arrow(tv-arg, t-boolean)),
          t-member("filter", t-top),
          t-member("map", let tb = s-atom("B", 202): t-forall([list: t-var(tb)], t-arrow([list: t-arrow([list: tv], t-var(tb))], mk-list(t-var(tb))));),
          t-member("each", t-arrow([list: t-arrow([list: tv], t-nothing)], t-nothing)),
          t-member("length", t-arrow(empty, t-number)),
          t-member("_torepr", t-torepr),
          t-member("_match", t-top),
          t-member("_plus", t-arrow([list: lotv], lotv)),
          t-member("push", t-arrow([list: ], lotv)),
          t-member("split-at", t-arrow(tv-arg, t-record([list:
            t-member("prefix", lotv),
            t-member("suffix", lotv)
          ]))),
          t-member("take", t-arrow([list: t-number], lotv)),
          t-member("drop", t-arrow([list: t-number], lotv)),
          t-member("get", t-arrow([list: t-number], tv)),
          t-member("set", t-arrow([list: t-number, tv], lotv))
        ])
      )
  end,
  SD.make-string-dict()
    .set("List", t-name(some("pyret-builtin://lists"), A.s-name(A.dummy-loc, "List")))
)

t-option = lam(param :: A.Name):
  t-app(t-name(some("pyret-builtin://option"), A.s-global("Option")), [list: t-var(param)])
end

t-and-then = lam(from-param, to-param :: A.Name):
  t-forall(
    [list: t-var(to-param)],
    t-arrow(
      [list:
        t-arrow([list: t-var(from-param)], t-option(to-param))
      ],
      t-option(to-param)
    )
  )
end

module-const-option = t-module("pyret-builtin://option",
  t-record([list:
    t-member("Option", t-arrow([list: t-top], t-boolean)),
    t-member("is-Option", t-arrow([list: t-top], t-boolean)),
    t-member("none", t-forall([list: t-var(s-atom("A", 10))], t-option(s-atom("A", 10)))),
    t-member("is-none", t-arrow([list: t-top], t-boolean)),
    t-member("some", t-forall([list: t-var(s-atom("A90", 10))], t-arrow([list: t-var(s-atom("A90", 10))], t-option(s-atom("A90", 10))))),
    t-member("is-some", t-arrow([list: t-top], t-boolean))
  ]),
  SD.make-string-dict()
    .set("Option", t-data(
      [list:
        t-var(s-atom("A", 10))
      ],
      [list:
        t-singleton-variant("none",
          [list:
            t-member("_match", t-top),
            t-member("_torepr", t-torepr),
            t-member("or-else", t-arrow([list: t-var(s-atom("A", 10))], t-var(s-atom("A", 10)))),
            t-member("and-then", t-and-then(s-atom("A", 10), s-atom("B", 10)))
          ]
        ),
        t-variant("some",
          [list: t-member("value", t-var(s-atom("A", 10)))],
          [list:
            t-member("_match", t-top),
            t-member("_torepr", t-torepr),
            t-member("or-else", t-arrow([list: t-var(s-atom("A", 10))], t-var(s-atom("A", 10)))),
            t-member("and-then", t-and-then(s-atom("A", 10), s-atom("B", 11)))
          ]
        )
      ],
      [list:
        t-member("and-then", t-and-then(s-atom("A", 10), s-atom("B", 12))),
        t-member("or-else", t-arrow([list: t-var(s-atom("A", 10))], t-var(s-atom("A", 10)))),
        t-member("_torepr", t-torepr),
        t-member("_match", t-top)
      ])
    ),
  SD.make-string-dict()
    .set("Option", t-name(some("pyret-builtin://option"), A.s-name(A.dummy-loc, "Option")))
)

module-const-error = t-module("pyret-builtin://error",
  t-record([list:
    t-member("RuntimeError", t-arrow([list: t-top], t-boolean)),
    t-member("is-RuntimeError", t-arrow([list: t-top], t-boolean)),
    t-member("message-exception", t-arrow([list: t-string], t-name(some("pyret-builtin://error"), A.s-global("RuntimeError")))),
    t-member("is-message-exception", t-arrow([list: t-top], t-boolean)),
    t-member("no-branches-matched", t-arrow([list: t-top, t-string], t-name(some("pyret-builtin://error"), A.s-global("RuntimeError")))),
    t-member("is-no-branches-matched", t-arrow([list: t-top], t-boolean)),
    t-member("internal-error", t-arrow([list: t-top, t-top], t-name(some("pyret-builtin://error"), A.s-global("RuntimeError")))),
    t-member("is-internal-error", t-arrow([list: t-top], t-boolean)),
    t-member("field-not-found", t-arrow([list: t-top, t-top, t-string], t-name(some("pyret-builtin://error"), A.s-global("RuntimeError")))),
    t-member("is-field-not-found", t-arrow([list: t-top], t-boolean)),
    t-member("lookup-non-object", t-arrow([list: t-top, t-top, t-string], t-name(some("pyret-builtin://error"), A.s-global("RuntimeError")))),
    t-member("is-lookup-non-object", t-arrow([list: t-top], t-boolean)),
    t-member("extend-non-object", t-arrow([list: t-top, t-top], t-name(some("pyret-builtin://error"), A.s-global("RuntimeError")))),
    t-member("is-extend-non-object", t-arrow([list: t-top], t-boolean)),
    t-member("non-boolean-condition", t-arrow([list: t-top, t-top, t-top], t-name(some("pyret-builtin://error"), A.s-global("RuntimeError")))),
    t-member("is-non-boolean-condition", t-arrow([list: t-top], t-boolean)),
    t-member("non-boolean-op", t-arrow([list: t-top, t-top, t-top, t-top], t-name(some("pyret-builtin://error"), A.s-global("RuntimeError")))),
    t-member("is-non-boolean-op", t-arrow([list: t-top], t-boolean)),
    t-member("generic-type-mismatch", t-arrow([list: t-top, t-string], t-name(some("pyret-builtin://error"), A.s-global("RuntimeError")))),
    t-member("is-generic-type-mismatch", t-arrow([list: t-top], t-boolean)),
    t-member("outside-numeric-range", t-arrow([list: t-top, t-top, t-top], t-name(some("pyret-builtin://error"), A.s-global("RuntimeError")))),
    t-member("is-outside-numeric-range", t-arrow([list: t-top], t-boolean)),
    t-member("plus-error", t-arrow([list: t-top, t-top], t-name(some("pyret-builtin://error"), A.s-global("RuntimeError")))),
    t-member("is-plus-error", t-arrow([list: t-top], t-boolean)),
    t-member("numeric-binop-error", t-arrow([list: t-top, t-top, t-top, t-top], t-name(some("pyret-builtin://error"), A.s-global("RuntimeError")))),
    t-member("is-numeric-binop-error", t-arrow([list: t-top], t-boolean)),
    t-member("cases-arity-mismatch", t-arrow([list: t-top, t-top, t-top], t-name(some("pyret-builtin://error"), A.s-global("RuntimeError")))),
    t-member("is-cases-arity-mismatch", t-arrow([list: t-top], t-boolean)),
    t-member("cases-singleton-mismatch", t-arrow([list: t-top, t-boolean], t-name(some("pyret-builtin://error"), A.s-global("RuntimeError")))),
    t-member("is-cases-singleton-mismatch", t-arrow([list: t-top], t-boolean)),
    t-member("arity-mismatch", t-arrow([list: t-top, t-top, t-top], t-name(some("pyret-builtin://error"), A.s-global("RuntimeError")))),
    t-member("is-arity-mismatch", t-arrow([list: t-top], t-boolean)),
    t-member("non-function-app", t-arrow([list: t-top, t-top], t-name(some("pyret-builtin://error"), A.s-global("RuntimeError")))),
    t-member("is-non-function-app", t-arrow([list: t-top], t-boolean)),
    t-member("bad-app", t-arrow([list: t-top, t-string, t-string, t-number, t-top], t-name(some("pyret-builtin://error"), A.s-global("RuntimeError")))),
    t-member("is-bad-app", t-arrow([list: t-top], t-boolean)),
    t-member("uninitialized-id", t-arrow([list: t-top, t-string], t-name(some("pyret-builtin://error"), A.s-global("RuntimeError")))),
    t-member("is-uninitialized-id", t-arrow([list: t-top], t-boolean)),
    t-member("module-load-failure", t-arrow([list: t-top], t-name(some("pyret-builtin://error"), A.s-global("RuntimeError")))),
    t-member("is-module-load-failure", t-arrow([list: t-top], t-boolean)),
    t-member("invalid-array-index", t-arrow([list: t-string, t-top, t-number, t-string], t-name(some("pyret-builtin://error"), A.s-global("RuntimeError")))),
    t-member("is-invalid-array-index", t-arrow([list: t-top], t-boolean)),
    t-member("user-break", t-name(some("pyret-builtin://error"), A.s-global("RuntimeError"))),
    t-member("is-user-break", t-arrow([list: t-top], t-boolean)),
    t-member("ParseError", t-arrow([list: t-top], t-boolean)),
    t-member("is-ParseError", t-arrow([list: t-top], t-boolean)),
    t-member("parse-error-next-token", t-arrow([list: t-top, t-string], t-name(some("pyret-builtin://error"), A.s-global("ParseError")))),
    t-member("is-parse-error-next-token", t-arrow([list: t-top], t-boolean)),
    t-member("parse-error-eof", t-arrow([list: t-top], t-name(some("pyret-builtin://error"), A.s-global("ParseError")))),
    t-member("is-parse-error-eof", t-arrow([list: t-top], t-boolean)),
    t-member("parse-error-unterminated-string", t-arrow([list: t-top], t-name(some("pyret-builtin://error"), A.s-global("ParseError")))),
    t-member("is-parse-error-unterminated-string", t-arrow([list: t-top], t-boolean)),
    t-member("empty-block", t-arrow([list: t-top], t-name(some("pyret-builtin://error"), A.s-global("ParseError")))),
    t-member("is-empty-block", t-arrow([list: t-top], t-boolean)),
    t-member("bad-block-stmt", t-arrow([list: t-top], t-name(some("pyret-builtin://error"), A.s-global("ParseError")))),
    t-member("is-bad-block-stmt", t-arrow([list: t-top], t-boolean)),
    t-member("bad-check-block-stmt", t-arrow([list: t-top], t-name(some("pyret-builtin://error"), A.s-global("ParseError")))),
    t-member("is-bad-check-block-stmt", t-arrow([list: t-top], t-boolean)),
    t-member("fun-missing-colon", t-arrow([list: t-top], t-name(some("pyret-builtin://error"), A.s-global("ParseError")))),
    t-member("is-fun-missing-colon", t-arrow([list: t-top], t-boolean)),
    t-member("fun-missing-end", t-arrow([list: t-top], t-name(some("pyret-builtin://error"), A.s-global("ParseError")))),
    t-member("is-fun-missing-end", t-arrow([list: t-top], t-boolean)),
    t-member("args-missing-comma", t-arrow([list: t-top], t-name(some("pyret-builtin://error"), A.s-global("ParseError")))),
    t-member("is-args-missing-comma", t-arrow([list: t-top], t-boolean)),
    t-member("app-args-missing-comma", t-arrow([list: t-top], t-name(some("pyret-builtin://error"), A.s-global("ParseError")))),
    t-member("is-app-args-missing-comma", t-arrow([list: t-top], t-boolean)),
    t-member("missing-end", t-arrow([list: t-top], t-name(some("pyret-builtin://error"), A.s-global("ParseError")))),
    t-member("is-missing-end", t-arrow([list: t-top], t-boolean)),
    t-member("missing-comma", t-arrow([list: t-top], t-name(some("pyret-builtin://error"), A.s-global("ParseError")))),
    t-member("is-missing-comma", t-arrow([list: t-top], t-boolean))
  ]),
  SD.make-string-dict()
    .set("RuntimeError",
      t-data(
        [list: ],
        [list:
          t-variant("message-exception", [list: t-member("message", t-string)], empty),
          t-variant("no-branches-matched", [list: t-member("loc", t-top), t-member("expression", t-string)], empty),
          t-variant("internal-error", [list: t-member("message", t-top), t-member("info-args", t-top)], empty),
          t-variant("field-not-found", [list: t-member("loc", t-top), t-member("obj", t-top), t-member("field", t-string)], empty),
          t-variant("lookup-non-object", [list: t-member("loc", t-top), t-member("non-obj", t-top), t-member("field", t-string)], empty),
          t-variant("extend-non-object", [list: t-member("loc", t-top), t-member("non-obj", t-top)], empty),
          t-variant("non-boolean-condition", [list: t-member("loc", t-top), t-member("typ", t-top), t-member("value", t-top)], empty),
          t-variant("non-boolean-op", [list: t-member("loc", t-top), t-member("position", t-top), t-member("typ", t-top), t-member("value", t-top)], empty),
          t-variant("generic-type-mismatch", [list: t-member("val", t-top), t-member("typ", t-string)], empty),
          t-variant("outside-numeric-range", [list: t-member("val", t-top), t-member("low", t-top), t-member("high", t-top)], empty),
          t-variant("plus-error", [list: t-member("val1", t-top), t-member("val2", t-top)], empty),
          t-variant("numeric-binop-error", [list: t-member("val1", t-top), t-member("val2", t-top), t-member("opname", t-top), t-member("methodname", t-top)], empty),
          t-variant("cases-arity-mismatch", [list: t-member("branch-loc", t-top), t-member("num-args", t-top), t-member("actual-arity", t-top)], empty),
          t-variant("cases-singleton-mismatch", [list: t-member("branch-loc", t-top), t-member("should-be-singleton", t-boolean)], empty),
          t-variant("arity-mismatch", [list: t-member("fun-loc", t-top), t-member("expected-arity", t-top), t-member("args", t-top)], empty),
          t-variant("non-function-app", [list: t-member("loc", t-top), t-member("non-fun-val", t-top)], empty),
          t-variant("bad-app", [list: t-member("loc", t-top), t-member("fun-name", t-string), t-member("message", t-string), t-member("arg-position", t-number), t-member("arg-val", t-top)], empty),
          t-variant("uninitialized-id", [list: t-member("loc", t-top), t-member("name", t-string)], empty),
          t-variant("module-load-failure", [list: t-member("names", t-top)], empty),
          t-variant("invalid-array-index", [list: t-member("method-name", t-string), t-member("array", t-top), t-member("index", t-number), t-member("reason", t-string)], empty),
          t-singleton-variant("user-break", empty)
        ],
        [list:
          t-member("_torepr", t-torepr),
          t-member("_tostring", t-tostring),
          t-member("_match", t-top)
        ]))
    .set("ParseError", t-data(
      [list: ],
      [list:
        t-variant("parse-error-next-token", [list: t-member("loc", t-top), t-member("next-token", t-string)], empty),
        t-variant("parse-error-eof", [list: t-member("loc", t-top)], empty),
        t-variant("parse-error-unterminated-string", [list: t-member("loc", t-top)], empty),
        t-variant("empty-block", [list: t-member("loc", t-top)], empty),
        t-variant("bad-block-stmt", [list: t-member("loc", t-top)], empty),
        t-variant("bad-check-block-stmt", [list: t-member("loc", t-top)], empty),
        t-variant("fun-missing-colon", [list: t-member("loc", t-top)], empty),
        t-variant("fun-missing-end", [list: t-member("loc", t-top)], empty),
        t-variant("args-missing-comma", [list: t-member("loc", t-top)], empty),
        t-variant("app-args-missing-comma", [list: t-member("loc", t-top)], empty),
        t-variant("missing-end", [list: t-member("loc", t-top)], empty),
        t-variant("missing-comma", [list: t-member("loc", t-top)], empty)
      ],
      [list:
        t-member("loc", t-top),
        t-member("_tostring", t-tostring),
        t-member("_torepr", t-torepr),
        t-member("_match", t-top)
      ])
    ),
  SD.make-string-dict()
    .set("Error", t-name(none, A.s-name(A.dummy-loc, "Error")))
)

module-const-either = t-module("pyret-builtin://either",
  t-record([list:
    t-member("Either", t-arrow([list: t-top], t-boolean)),
    t-member("is-Either", t-arrow([list: t-top], t-boolean)),
    t-member("left", t-forall([list: t-var(s-atom("a792", 10)), t-var(s-atom("b793", 11))], t-arrow([list: t-var(s-atom("a792", 10))], t-app(t-name(some("pyret-builtin://either"), A.s-global("Either")), [list: t-var(s-atom("a792", 10)), t-var(s-atom("b793", 11))])))),
    t-member("is-left", t-arrow([list: t-top], t-boolean)),
    t-member("right", t-forall([list: t-var(s-atom("a794", 10)), t-var(s-atom("b795", 11))], t-arrow([list: t-var(s-atom("b795", 11))], t-app(t-name(some("pyret-builtin://either"), A.s-global("Either")), [list: t-var(s-atom("a794", 10)), t-var(s-atom("b795", 11))])))),
    t-member("is-right", t-arrow([list: t-top], t-boolean))
  ]),
  SD.make-string-dict()
    .set("Either", t-data(
      [list:
        t-var(s-atom("a", 10)),
        t-var(s-atom("b", 11))
      ],
      [list:
        t-variant("left",
          [list:
            t-member("v", t-var(s-atom("a", 10)))
          ],
          [list:
            t-member("_match", t-top),
            t-member("_torepr", t-torepr)
          ]
        ),
        t-variant("right",
          [list:
            t-member("v", t-var(s-atom("b", 11)))
          ],
          [list:
            t-member("_match", t-top),
            t-member("_torepr", t-torepr)
          ]
        )
      ],
      [list:
        t-member("v", t-top),
        t-member("_torepr", t-torepr),
        t-member("_match", t-top)
      ])
    ),
  SD.make-string-dict()
    .set("Either", t-name(some("pyret-builtin://either"), A.s-name(A.dummy-loc, "Either")))
)

t-s-exp = t-name(some("pyret-builtin://s-exp-structs"), A.s-global("S-Exp"))

s-exp-struct-mems = [list:
  t-member("s-list", t-arrow([list: mk-list(t-s-exp)], t-s-exp)),
  t-member("s-num", t-arrow([list: t-number], t-s-exp)),
  t-member("s-str", t-arrow([list: t-string], t-s-exp)),
  t-member("s-sym", t-arrow([list: t-string], t-s-exp)),
  t-member("is-s-list", t-arrow([list: t-top], t-boolean)),
  t-member("is-s-num", t-arrow([list: t-top], t-boolean)),
  t-member("is-s-str", t-arrow([list: t-top], t-boolean)),
  t-member("is-s-sym", t-arrow([list: t-top], t-boolean))
]

module-const-s-exp = t-module("pyret-builtin://s-exp",
  t-record(s-exp-struct-mems + [list:
    t-member("read-s-exp", t-arrow([list: t-string], t-s-exp))
  ]),
  SD.make-string-dict(),
  SD.make-string-dict()
    .set("S-Exp", t-s-exp)
)

module-const-s-exp-structs = t-module("pyret-builtin://s-exp-structs",
  t-record(s-exp-struct-mems),
  SD.make-string-dict()
    .set("S-Exp", t-data(
      [list: ],
      [list:
        t-variant("s-list",
          [list:
            t-member("exps", mk-list(t-s-exp))
          ],
          [list:
            t-member("_match", t-top),
            t-member("_torepr", t-torepr)
          ]
        ),
        t-variant("s-num",
          [list:
            t-member("n", t-number)
          ],
          [list:
            t-member("_match", t-top),
            t-member("_torepr", t-torepr)
          ]
        ),
        t-variant("s-str",
          [list:
            t-member("s", t-string)
          ],
          [list:
            t-member("_match", t-top),
            t-member("_torepr", t-torepr)
          ]
        ),
        t-variant("s-sym",
          [list:
            t-member("s", t-string)
          ],
          [list:
            t-member("_match", t-top),
            t-member("_torepr", t-torepr)
          ]
        )
      ],
      [list:
        t-member("_torepr", t-torepr)
      ])
    ),
  SD.make-string-dict()
)

fun make-default-modules():
  default-modules = SD.make-mutable-string-dict()
  default-modules.set-now("pyret-builtin://equality", module-const-equality)
  default-modules.set-now("pyret-builtin://lists", module-const-lists)
  default-modules.set-now("pyret-builtin://option", module-const-option)
  default-modules.set-now("pyret-builtin://error", module-const-error)
  default-modules.set-now("pyret-builtin://either", module-const-either)
  default-modules.set-now("pyret-builtin://arrays", module-const-arrays)
  default-modules.set-now("pyret-builtin://sets", module-const-sets)
  default-modules.set-now("pyret-builtin://s-exp", module-const-s-exp)
  default-modules.set-now("pyret-builtin://s-exp-structs", module-const-s-exp-structs)
  default-modules
end
