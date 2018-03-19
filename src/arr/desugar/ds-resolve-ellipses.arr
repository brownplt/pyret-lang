provide *

include string-dict
include file("ds-structs.arr")
include file("ds-parse.arr")
import  file("union-find.arr") as UF


################################################################################
#  Resolve Ellipsis Labels
#

fun resolve-ellipses(p :: Pattern) -> Pattern block:
  equiv = [mutable-string-dict: ]
  pvar-map = [mutable-string-dict: ]
  
  fun collect(shadow p :: Pattern, ellipses :: List<String>) -> Nothing:
    cases (Pattern) p block:
      | p-pvar(v, _) =>
        cases (Option) pvar-map.get-now(v) block:
          | none => pvar-map.set-now(v, ellipses)
          | some(ellipses2) => 
            if ellipses.length() == ellipses2.length():
              for each2(a from ellipses, b from ellipses2):
                UF.union(equiv, a, b)
              end
            else:
              fail("Pattern variable " + v + " used underneath differing numbers of ellipses")
            end
        end
      | p-value(_) => nothing
      | p-core(_, args) => args.each(collect(_, ellipses))
      | p-aux(_, args) => args.each(collect(_, ellipses))
      | p-surf(_, args) => args.each(collect(_, ellipses))
      | p-var(_) => nothing
      | p-option(opt) =>
        cases (Option) opt:
          | none => nothing
          | some(shadow p) => collect(p, ellipses)
        end
      | p-tag(_, _, _) =>
        panic("Resolve ellipses labels: unexpected tag")
      | p-fresh(_, body) => collect(body, ellipses)
      | p-list(l) => collect-list(l, ellipses)
    end
  end
  
  fun collect-list(ps :: SeqPattern, ellipses :: List<String>):
    cases (SeqPattern) ps block:
      | seq-empty => nothing
      | seq-cons(f, r) =>
        collect(f, ellipses)
        collect-list(r, ellipses)
      | seq-ellipsis(shadow p, label) =>
        collect(p, link(label, ellipses))
      | seq-ellipsis-list(_, _) =>
        panic("Resolve ellipses labels: unexpected ellipsis-list")
    end
  end
  
  fun resolve(shadow p :: Pattern) -> Pattern:
    cases (Pattern) p:
      | p-pvar(_, _) => p
      | p-value(_) => p
      | p-core(name, args) => p-core(name, args.map(resolve))
      | p-aux(name,  args) => p-aux(name,  args.map(resolve))
      | p-surf(name, args) => p-surf(name, args.map(resolve))
      | p-var(_) => p
      | p-option(opt) =>
        cases (Option) opt:
          | none => p-option(none)
          | some(shadow p) => p-option(some(resolve(p)))
        end
      | p-tag(_, _, _) =>
        panic("Resolve ellipses labels: unexpected tag'")
      | p-fresh(fresh, body) => p-fresh(fresh, resolve(body))
      | p-list(l) => p-list(resolve-list(l))
    end
  end
  
  fun resolve-list(ps :: SeqPattern) -> SeqPattern:
    cases (SeqPattern) ps:
      | seq-empty => ps
      | seq-cons(f, r) => seq-cons(resolve(f), resolve-list(r))
      | seq-ellipsis(shadow p, label) => seq-ellipsis(resolve(p), UF.find(equiv, label))
      | seq-ellipsis-list(_, _) =>
        panic("Resolve ellipses labels: unexpected ellipsis-list'")
    end
  end
  
  collect(p, [list: ])
  resolve(p)
end

check:
  resolve-ellipses(parse-pattern(none, "a")) 
    is parse-pattern(none, "a")
  resolve-ellipses(parse-pattern(none, "[a ...]")) 
    is parse-pattern(none, "[a ...]")
  resolve-ellipses(parse-pattern(none, "[[a ...] ...]")) 
    is parse-pattern(none, "[[a ...] ...]")
  resolve-ellipses(parse-pattern(none, "[[a ...] [a ...]]")) 
    is p-list(seq-cons(
      parse-pattern(none, "[a ...]"),
      seq-cons(parse-pattern(none, "[a ...]"), seq-empty)))
  resolve-ellipses(parse-pattern(none, "[[a ...] [b ...]]"))
    is parse-pattern(none, "[[a ...] [b ...]]")
  resolve-ellipses(parse-pattern(none, "[[a ...] [[a ...] ...]]"))
    raises "ellipses"
  resolve-ellipses(parse-pattern(none, "(Bar [(Foo a b) ...] [a ...] [b ...])"))
    is p-surf("Bar", [list:
      parse-pattern(none, "[(Foo a b) ...]"),
      parse-pattern(none, "[a ...]"),
      parse-pattern(none, "[b ...]")])
end
