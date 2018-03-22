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
      | pat-pvar(v, _) =>
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
      | pat-value(_) => nothing
      | pat-core(_, args) => args.each(collect(_, ellipses))
      | pat-aux(_, args) => args.each(collect(_, ellipses))
      | pat-surf(_, args) => args.each(collect(_, ellipses))
      | pat-meta(_, args) => args.each(collect(_, ellipses))
      | pat-var(_) => nothing
      | pat-option(opt) =>
        cases (Option) opt:
          | none => nothing
          | some(shadow p) => collect(p, ellipses)
        end
      | pat-tag(_, _, _) =>
        panic("Resolve ellipses labels: unexpected tag")
      | pat-fresh(_, body) => collect(body, ellipses)
      | pat-list(l) => collect-list(l, ellipses)
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
      | pat-pvar(_, _) => p
      | pat-value(_) => p
      | pat-core(name, args) => pat-core(name, args.map(resolve))
      | pat-aux(name,  args) => pat-aux(name,  args.map(resolve))
      | pat-surf(name, args) => pat-surf(name, args.map(resolve))
      | pat-meta(name, args) => pat-meta(name, args.map(resolve))
      | pat-var(_) => p
      | pat-option(opt) =>
        cases (Option) opt:
          | none => pat-option(none)
          | some(shadow p) => pat-option(some(resolve(p)))
        end
      | pat-tag(_, _, _) =>
        panic("Resolve ellipses labels: unexpected tag'")
      | pat-fresh(fresh, body) => pat-fresh(fresh, resolve(body))
      | pat-list(l) => pat-list(resolve-list(l))
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
    is pat-list(seq-cons(
      parse-pattern(none, "[a ...]"),
      seq-cons(parse-pattern(none, "[a ...]"), seq-empty)))
  resolve-ellipses(parse-pattern(none, "[[a ...] [b ...]]"))
    is parse-pattern(none, "[[a ...] [b ...]]")
  resolve-ellipses(parse-pattern(none, "[[a ...] [[a ...] ...]]"))
    raises "ellipses"
  resolve-ellipses(parse-pattern(none, "(Bar [(Foo a b) ...] [a ...] [b ...])"))
    is pat-surf("Bar", [list:
      parse-pattern(none, "[(Foo a b) ...]"),
      parse-pattern(none, "[a ...]"),
      parse-pattern(none, "[b ...]")])
end
