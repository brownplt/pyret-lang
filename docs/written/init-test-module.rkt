#lang scribble/base
@(require "../scribble-api.rkt")
@docmodule["list"]{
  @; Ignored type testers
  @ignore[(list "is-empty" "is-link")]
  @; Unknown: PLEASE DOCUMENT
  @ignore[
    (list "none" "is-none" "some" "is-some" "Option" "left" "right" "Either")
  ]
  @section{Data types}
  @; Data expr for List
  @; Singleton data variant empty
  @; Data constructor link
  @para{Here is an xref to @xref["list" "get-help"]} @;need para here to keep xref inline with text
  @section{Functions}
  @;@function["I-should-yield-a-warning"] @;this line here for testing
  @function["get-help" #:contract (list "Any" "Number" "Any")]
  @function["set-help" #:contract (list "Any" "Number" "Any" "Any")]
  @function["reverse-help" #:contract (list "Any" "Any" "Any")]
  @function["raw-fold" #:contract (list "Any" "Any" "List" "Any")]
  @function["range" #:contract (list "Any" "Any" "Any")]
  @function["repeat" #:contract (list "Number" "Any" "List")]{
    @; repeat(0, 10) is empty
    @; repeat(3, -1) is link(-1, link(-1, link(-1, empty)))
    @; repeat(1, "foo") is link("foo", empty)
    
  }
  @function["filter" #:contract (list "Any" "List" "Any")]
  @function["partition" #:contract (list "Any" "List" "Any")]
  @function["find" #:contract (list "(Any -> Bool)" "List" "Option")]{
    @; find(fun(elt): elt > 1 end, link(1, link(2, link(3, empty)))) is some(2)
    @; find(fun(elt): true end, link("find-me", empty)) is some("find-me")
    @; find(fun(elt): elt > 4 end, link(1, link(2, link(3)))) is none
    @; find(fun(elt): true end, empty) is none
    @; find(fun(elt): false end, empty) is none
    @; find(fun(elt): false end, link(1, empty)) is none
    
  }
  @function[
    "split-at"
    #:contract (list "Number" "List" "{ prefix :: List, suffix :: List }")
  ]{
    @; let  one-four = link(1, link(2, link(3, link(4, empty)))):
    @;   split-at(0, one-four) is { "prefix": empty, "suffix": one-four }
    @;   split-at(4, one-four) is { "prefix": one-four, "suffix": empty }
    @;   split-at(2, one-four) is
    @;     { "prefix": link(1, link(2, empty)), "suffix": link(3, link(4, empty)) }
    @;   split-at(-1, one-four) raises "Invalid index"
    @;   split-at(5, one-four) raises "Index too large"
    @; end
    
  }
  @function["any" #:contract (list "(Any -> Bool)" "List" "Bool")]{
    @; any(fun(n): n > 1 end, link(1, link(2, link(3, empty)))) is true
    @; any(fun(n): n > 3 end, link(1, link(2, link(3, empty)))) is false
    @; any(fun(x): true end, empty) is false
    @; any(fun(x): false end, empty) is false
    
  }
  @function["all" #:contract (list "(Any -> Bool)" "List" "Bool")]{
    @; all(fun(n): n > 1 end, link(1, link(2, link(3, empty)))) is false
    @; all(fun(n): n <= 3 end, link(1, link(2, link(3, empty)))) is true
    @; all(fun(x): true end, empty) is true
    @; all(fun(x): false end, empty) is true
    
  }
  @function["map" #:contract (list "Any" "List" "Any")]
  @function["map2" #:contract (list "Any" "List" "List" "Any")]
  @function["map3" #:contract (list "Any" "List" "List" "List" "Any")]
  @function["map4" #:contract (list "Any" "List" "List" "List" "List" "Any")]
  @function["map_n" #:contract (list "Any" "Number" "List" "Any")]
  @function["map2_n" #:contract (list "Any" "Number" "List" "List" "Any")]
  @function[
    "map3_n"
    #:contract (list "Any" "Number" "List" "List" "List" "Any")
  ]
  @function[
    "map4_n"
    #:contract (list "Any" "Number" "List" "List" "List" "List" "Any")
  ]
  @function["each" #:contract (list "Any" "List" "Any")]
  @function["each2" #:contract (list "Any" "List" "List" "Any")]
  @function["each3" #:contract (list "Any" "List" "List" "List" "Any")]
  @function["each4" #:contract (list "Any" "List" "List" "List" "List" "Any")]
  @function["each_n" #:contract (list "Any" "Number" "List" "Any")]
  @function["each2_n" #:contract (list "Any" "Number" "List" "List" "Any")]
  @function[
    "each3_n"
    #:contract (list "Any" "Number" "List" "List" "List" "Any")
  ]
  @function[
    "each4_n"
    #:contract (list "Any" "Number" "List" "List" "List" "List" "Any")
  ]
  @function["fold-while" #:contract (list "Any" "Any" "Any" "Any")]
  @function["fold" #:contract (list "Any" "Any" "List" "Any")]
  @function["fold2" #:contract (list "Any" "Any" "List" "List" "Any")]
  @function["fold3" #:contract (list "Any" "Any" "List" "List" "List" "Any")]
  @function[
    "fold4"
    #:contract (list "Any" "Any" "List" "List" "List" "List" "Any")
  ]
  @function["fold_n" #:contract (list "Any" "Number" "Any" "List" "Any")]
  @function["index" #:contract (list "Any" "Number" "Any")]
}
The program didn't define any tests.
