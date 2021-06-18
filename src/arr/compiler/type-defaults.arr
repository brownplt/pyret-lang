provide *
provide-types *

import file("ast.arr") as A
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

t-data                    = TS.t-data(_, _, _, _, A.dummy-loc)

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
t-runtime-error = t-name(module-uri("builtin://error"), A.s-type-global("RuntimeError"))
t-parse-error = t-name(module-uri("builtin://error"), A.s-type-global("ParseError"))
t-either = t-name(module-uri("builtin://either"), A.s-type-global("Either"))
t-s-exp = t-name(module-uri("builtin://s-exp-structs"), A.s-type-global("S-Exp"))

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
  default-typs.set-now("roughly-equal-always", t-arrow([list: t-top, t-top], t-boolean))
  default-typs.set-now("roughly-equal-now", t-arrow([list: t-top, t-top], t-boolean))
  default-typs.set-now("roughly-equal", t-arrow([list: t-top, t-top], t-boolean))
  default-typs.set-now("equal-always", t-arrow([list: t-top, t-top], t-boolean))
  default-typs.set-now("equal-now", t-arrow([list: t-top, t-top], t-boolean))
  default-typs.set-now("identical", t-arrow([list: t-top, t-top], t-boolean))
  default-typs.set-now("roughly-equal-always3", t-arrow([list: t-top, t-top], t-equality-result))
  default-typs.set-now("roughly-equal-now3", t-arrow([list: t-top, t-top], t-equality-result))
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
  default-typs.set-now("trace-value", t-arrow([list: t-top, t-top], t-top))

  default-typs.freeze()
end

fun make-default-data-exprs() block:
  default-data-exprs = SD.make-string-dict()
  default-data-exprs
end

#   NOTE(alex): Removed because they were overriding the actual definitions
#     found in src/runtime and potentially src/runtime-arr
default-modules = SD.make-mutable-string-dict()
shadow default-modules = default-modules.freeze()

fun make-default-modules() block:
  default-modules
end
