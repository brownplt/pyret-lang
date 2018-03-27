include file("../../../src/arr/desugar/ds-resolve-ellipses.arr")

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
