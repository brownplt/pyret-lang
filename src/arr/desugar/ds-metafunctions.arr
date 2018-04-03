provide *

include string-dict
include file("ds-structs.arr")
import file("ds-sugar.arr") as S

fun reverse-term(e):
  cases (Term) e:
    | g-list(lst) => g-list(lst.reverse())
    | else => fail("Reverse bijection: expected a list, but found: " + tostring(e))
  end
end

add-bijection("reverse", reverse-term, reverse-term)

add-metafunction("get-loc-of", 1,
  lam(args, env):
    cases (Term) args.get(0):
      | g-core(_, shadow args) => args.get(0)
      # TODO: might want to support g-var
      | else =>
        fail("get-loc-of should be used on an already desugared value. Got " +
             tostring(args.get(0)))
    end
  end)

add-metafunction("resugar", 1,
  lam(args, env):
    cases (Option) S.resugar(args.get(0)):
      | some(e) => g-option(some(e))
      | none => g-option(none)
    end
  end)
