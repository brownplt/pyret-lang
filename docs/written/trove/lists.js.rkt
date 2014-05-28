#lang scribble/base
@(require "../../scribble-api.rkt")
@docmodule["lists"]{
  @; Unknown: PLEASE DOCUMENT
  @ignore[(list "list")]
  @section[#:tag "lists_ReExports"]{Re-exported values}
  @re-export["none" (from (xref "option" "none"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["option" "none"]}
  }
  @re-export["is-none" (from (xref "option" "is-none"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["option" "is-none"]}
  }
  @re-export["some" (from (xref "option" "some"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["option" "some"]}
  }
  @re-export["is-some" (from (xref "option" "is-some"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["option" "is-some"]}
  }
  @re-export["Option" (from (xref "option" "Option"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["option" "Option"]}
  }
  @re-export["left" (from (xref "either" "left"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["either" "left"]}
  }
  @re-export["right" (from (xref "either" "right"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["either" "right"]}
  }
  @re-export["Either" (from (xref "either" "Either"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["either" "Either"]}
  }
  @section[#:tag "lists_DataTypes"]{Data types}
  @data-spec["List"]{
    @variants{
      @singleton-spec["empty"]{
        @with-members{
          @method-spec[
            "length"
            #:contract
            (a-arrow
              (a-id "List" (xref "lists" "List"))
              (a-id "Number" (xref "<global>" "Number")))
          ]
          @method-spec[
            "each"
            #:contract
            (a-arrow
              (a-id "List" (xref "lists" "List"))
              (a-arrow "Any" (a-id "Nothing" (xref "<global>" "Nothing")))
              (a-id "Nothing" (xref "<global>" "Nothing")))
          ]
          @method-spec[
            "map"
            #:contract
            (a-arrow
              (a-id "List" (xref "lists" "List"))
              (a-arrow "Any" "Any")
              (a-id "List" (xref "lists" "List")))
          ]
          @method-spec[
            "filter"
            #:contract
            (a-arrow
              (a-id "List" (xref "lists" "List"))
              (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
              (a-id "List" (xref "lists" "List")))
          ]
          @method-spec[
            "find"
            #:contract
            (a-arrow
              (a-id "List" (xref "lists" "List"))
              (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
              (a-id "Option" (xref "option" "Option")))
          ]
          @method-spec[
            "partition"
            #:contract (a-arrow (a-id "List" (xref "lists" "List")) "Any" "Any")
          ]
          @method-spec[
            "foldr"
            #:contract
            (a-arrow (a-id "List" (xref "lists" "List")) "Any" "Any" "Any")
          ]
          @method-spec[
            "foldl"
            #:contract
            (a-arrow (a-id "List" (xref "lists" "List")) "Any" "Any" "Any")
          ]
          @method-spec[
            "member"
            #:contract (a-arrow (a-id "List" (xref "lists" "List")) "Any" "Any")
          ]
          @method-spec[
            "append"
            #:contract (a-arrow (a-id "List" (xref "lists" "List")) "Any" "Any")
          ]
          @method-spec[
            "last"
            #:contract (a-arrow (a-id "List" (xref "lists" "List")) "Any")
          ]
          @method-spec[
            "reverse"
            #:contract (a-arrow (a-id "List" (xref "lists" "List")) "Any")
          ]
          @method-spec[
            "tostring"
            #:contract (a-arrow (a-id "List" (xref "lists" "List")) "Any")
          ]
          @method-spec[
            "_torepr"
            #:contract (a-arrow (a-id "List" (xref "lists" "List")) "Any")
          ]
          @method-spec[
            "sort-by"
            #:contract
            (a-arrow (a-id "List" (xref "lists" "List")) "Any" "Any" "Any")
          ]
          @method-spec[
            "sort"
            #:contract (a-arrow (a-id "List" (xref "lists" "List")) "Any")
          ]
          @method-spec[
            "join-str"
            #:contract (a-arrow (a-id "List" (xref "lists" "List")) "Any" "Any")
          ]
        }
      }
      @constr-spec["link"]{
        @members{
          @member-spec["first" #:contract "Any"]
          @member-spec["rest" #:contract (a-id "List" (xref "lists" "List"))]
        }
        @with-members{
          @method-spec[
            "length"
            #:contract (a-arrow (a-id "List" (xref "lists" "List")) "Any")
          ]
          @method-spec[
            "each"
            #:contract (a-arrow (a-id "List" (xref "lists" "List")) "Any" "Any")
          ]
          @method-spec[
            "map"
            #:contract (a-arrow (a-id "List" (xref "lists" "List")) "Any" "Any")
          ]
          @method-spec[
            "filter"
            #:contract (a-arrow (a-id "List" (xref "lists" "List")) "Any" "Any")
          ]
          @method-spec[
            "partition"
            #:contract (a-arrow (a-id "List" (xref "lists" "List")) "Any" "Any")
          ]
          @method-spec[
            "find"
            #:contract (a-arrow (a-id "List" (xref "lists" "List")) "Any" "Any")
          ]
          @method-spec[
            "member"
            #:contract (a-arrow (a-id "List" (xref "lists" "List")) "Any" "Any")
          ]
          @method-spec[
            "foldr"
            #:contract
            (a-arrow (a-id "List" (xref "lists" "List")) "Any" "Any" "Any")
          ]
          @method-spec[
            "foldl"
            #:contract
            (a-arrow (a-id "List" (xref "lists" "List")) "Any" "Any" "Any")
          ]
          @method-spec[
            "append"
            #:contract (a-arrow (a-id "List" (xref "lists" "List")) "Any" "Any")
          ]
          @method-spec[
            "last"
            #:contract (a-arrow (a-id "List" (xref "lists" "List")) "Any")
          ]
          @method-spec[
            "reverse"
            #:contract (a-arrow (a-id "List" (xref "lists" "List")) "Any")
          ]
          @method-spec[
            "tostring"
            #:contract (a-arrow (a-id "List" (xref "lists" "List")) "Any")
          ]
          @method-spec[
            "_torepr"
            #:contract (a-arrow (a-id "List" (xref "lists" "List")) "Any")
          ]
          @method-spec[
            "sort-by"
            #:contract
            (a-arrow (a-id "List" (xref "lists" "List")) "Any" "Any" "Any")
          ]
          @method-spec[
            "sort"
            #:contract (a-arrow (a-id "List" (xref "lists" "List")) "Any")
          ]
          @method-spec[
            "join-str"
            #:contract (a-arrow (a-id "List" (xref "lists" "List")) "Any" "Any")
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "push"
        #:contract (a-arrow (a-id "List" (xref "lists" "List")) "Any" "Any")
      ]
      @method-spec[
        "_plus"
        #:contract
        (a-arrow
          (a-id "List" (xref "lists" "List"))
          (a-id "List" (xref "lists" "List"))
          "Any")
      ]
      @method-spec[
        "split-at"
        #:contract (a-arrow (a-id "List" (xref "lists" "List")) "Any" "Any")
      ]
      @method-spec[
        "take"
        #:contract (a-arrow (a-id "List" (xref "lists" "List")) "Any" "Any")
      ]
      @method-spec[
        "drop"
        #:contract (a-arrow (a-id "List" (xref "lists" "List")) "Any" "Any")
      ]
      @method-spec[
        "get"
        #:contract (a-arrow (a-id "List" (xref "lists" "List")) "Any" "Any")
      ]
      @method-spec[
        "set"
        #:contract
        (a-arrow (a-id "List" (xref "lists" "List")) "Any" "Any" "Any")
      ]
    }
  }
  @section[#:tag "lists_Functions"]{Functions}
  @function[
    "is-empty"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-link"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "get-help"
    #:contract (a-arrow "Any" (a-id "Number" (xref "<global>" "Number")) "Any")
  ]
  @function[
    "set-help"
    #:contract
    (a-arrow "Any" (a-id "Number" (xref "<global>" "Number")) "Any" "Any")
  ]
  @function["reverse-help" #:contract (a-arrow "Any" "Any" "Any")]
  @function[
    "raw-fold"
    #:contract (a-arrow "Any" "Any" (a-id "List" (xref "lists" "List")) "Any")
  ]
  @function["range" #:contract (a-arrow "Any" "Any" "Any")]
  @function[
    "repeat"
    #:contract
    (a-arrow
      (a-id "Number" (xref "<global>" "Number"))
      "Any"
      (a-id "List" (xref "lists" "List")))
    #:examples
    '@{
      @; repeat(0, 10) is empty
      @; repeat(3, -1) is link(-1, link(-1, link(-1, empty)))
      @; repeat(1, "foo") is link("foo", empty)
      
    }
  ]
  @function[
    "filter"
    #:contract (a-arrow "Any" (a-id "List" (xref "lists" "List")) "Any")
  ]
  @function[
    "partition"
    #:contract (a-arrow "Any" (a-id "List" (xref "lists" "List")) "Any")
  ]
  @function[
    "find"
    #:contract
    (a-arrow
      (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
      (a-id "List" (xref "lists" "List"))
      (a-id "Option" (xref "option" "Option")))
    #:examples
    '@{
      @; find(fun(elt): elt > 1 end, link(1, link(2, link(3, empty)))) is some(2)
      @; find(fun(elt): true end, link("find-me", empty)) is some("find-me")
      @; find(fun(elt): elt > 4 end, link(1, link(2, link(3)))) is none
      @; find(fun(elt): true end, empty) is none
      @; find(fun(elt): false end, empty) is none
      @; find(fun(elt): false end, link(1, empty)) is none
      
    }
  ]
  @function[
    "split-at"
    #:contract
    (a-arrow
      (a-id "Number" (xref "<global>" "Number"))
      (a-id "List" (xref "lists" "List"))
      (a-record
        (a-field "prefix" (a-id "List" (xref "lists" "List")))
        (a-field "suffix" (a-id "List" (xref "lists" "List")))))
    #:examples
    '@{
      @; let  one-four = link(1, link(2, link(3, link(4, empty)))):
      @;   split-at(0, one-four) is { "prefix": empty, "suffix": one-four }
      @;   split-at(4, one-four) is { "prefix": one-four, "suffix": empty }
      @;   split-at(2, one-four) is
      @;     { "prefix": link(1, link(2, empty)), "suffix": link(3, link(4, empty)) }
      @;   split-at(-1, one-four) raises "Invalid index"
      @;   split-at(5, one-four) raises "Index too large"
      @; end
      
    }
  ]
  @function[
    "any"
    #:contract
    (a-arrow
      (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
      (a-id "List" (xref "lists" "List"))
      (a-id "Bool" (xref "<global>" "Bool")))
    #:examples
    '@{
      @; any(fun(n): n > 1 end, link(1, link(2, link(3, empty)))) is true
      @; any(fun(n): n > 3 end, link(1, link(2, link(3, empty)))) is false
      @; any(fun(x): true end, empty) is false
      @; any(fun(x): false end, empty) is false
      
    }
  ]
  @function[
    "all"
    #:contract
    (a-arrow
      (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
      (a-id "List" (xref "lists" "List"))
      (a-id "Bool" (xref "<global>" "Bool")))
    #:examples
    '@{
      @; all(fun(n): n > 1 end, link(1, link(2, link(3, empty)))) is false
      @; all(fun(n): n <= 3 end, link(1, link(2, link(3, empty)))) is true
      @; all(fun(x): true end, empty) is true
      @; all(fun(x): false end, empty) is true
      
    }
  ]
  @function[
    "all2"
    #:contract
    (a-arrow
      (a-arrow "Any" "Any" (a-id "Bool" (xref "<global>" "Bool")))
      (a-id "List" (xref "lists" "List"))
      (a-id "List" (xref "lists" "List"))
      (a-id "Bool" (xref "<global>" "Bool")))
    #:examples
    '@{
      @; all2(fun(n, m): n > m end,
      @;   link(1, link(2, link(3, empty))),
      @;   link(0, link(1, link(2, empty)))) is
      @;   true
      @; all2(fun(n, m): (n + m) == 3 end,
      @;   link(1, link(2, link(3, empty))),
      @;   link(2, link(1, link(0, empty)))) is
      @;   true
      @; all2(fun(n, m): n < m end,
      @;   link(1, link(2, link(3, empty))),
      @;   link(0, link(1, link(2, empty)))) is
      @;   false
      @; all2(fun($underscore, $underscore): true end, empty, empty) is true
      @; all2(fun($underscore, $underscore): false end, empty, empty) is true
      
    }
  ]
  @function[
    "map"
    #:contract (a-arrow "Any" (a-id "List" (xref "lists" "List")) "Any")
  ]
  @function[
    "map2"
    #:contract
    (a-arrow
      "Any"
      (a-id "List" (xref "lists" "List"))
      (a-id "List" (xref "lists" "List"))
      "Any")
  ]
  @function[
    "map3"
    #:contract
    (a-arrow
      "Any"
      (a-id "List" (xref "lists" "List"))
      (a-id "List" (xref "lists" "List"))
      (a-id "List" (xref "lists" "List"))
      "Any")
  ]
  @function[
    "map4"
    #:contract
    (a-arrow
      "Any"
      (a-id "List" (xref "lists" "List"))
      (a-id "List" (xref "lists" "List"))
      (a-id "List" (xref "lists" "List"))
      (a-id "List" (xref "lists" "List"))
      "Any")
  ]
  @function[
    "map_n"
    #:contract
    (a-arrow
      "Any"
      (a-id "Number" (xref "<global>" "Number"))
      (a-id "List" (xref "lists" "List"))
      "Any")
  ]
  @function[
    "map2_n"
    #:contract
    (a-arrow
      "Any"
      (a-id "Number" (xref "<global>" "Number"))
      (a-id "List" (xref "lists" "List"))
      (a-id "List" (xref "lists" "List"))
      "Any")
  ]
  @function[
    "map3_n"
    #:contract
    (a-arrow
      "Any"
      (a-id "Number" (xref "<global>" "Number"))
      (a-id "List" (xref "lists" "List"))
      (a-id "List" (xref "lists" "List"))
      (a-id "List" (xref "lists" "List"))
      "Any")
  ]
  @function[
    "map4_n"
    #:contract
    (a-arrow
      "Any"
      (a-id "Number" (xref "<global>" "Number"))
      (a-id "List" (xref "lists" "List"))
      (a-id "List" (xref "lists" "List"))
      (a-id "List" (xref "lists" "List"))
      (a-id "List" (xref "lists" "List"))
      "Any")
  ]
  @function[
    "each"
    #:contract (a-arrow "Any" (a-id "List" (xref "lists" "List")) "Any")
  ]
  @function[
    "each2"
    #:contract
    (a-arrow
      "Any"
      (a-id "List" (xref "lists" "List"))
      (a-id "List" (xref "lists" "List"))
      "Any")
  ]
  @function[
    "each3"
    #:contract
    (a-arrow
      "Any"
      (a-id "List" (xref "lists" "List"))
      (a-id "List" (xref "lists" "List"))
      (a-id "List" (xref "lists" "List"))
      "Any")
  ]
  @function[
    "each4"
    #:contract
    (a-arrow
      "Any"
      (a-id "List" (xref "lists" "List"))
      (a-id "List" (xref "lists" "List"))
      (a-id "List" (xref "lists" "List"))
      (a-id "List" (xref "lists" "List"))
      "Any")
  ]
  @function[
    "each_n"
    #:contract
    (a-arrow
      "Any"
      (a-id "Number" (xref "<global>" "Number"))
      (a-id "List" (xref "lists" "List"))
      "Any")
  ]
  @function[
    "each2_n"
    #:contract
    (a-arrow
      "Any"
      (a-id "Number" (xref "<global>" "Number"))
      (a-id "List" (xref "lists" "List"))
      (a-id "List" (xref "lists" "List"))
      "Any")
  ]
  @function[
    "each3_n"
    #:contract
    (a-arrow
      "Any"
      (a-id "Number" (xref "<global>" "Number"))
      (a-id "List" (xref "lists" "List"))
      (a-id "List" (xref "lists" "List"))
      (a-id "List" (xref "lists" "List"))
      "Any")
  ]
  @function[
    "each4_n"
    #:contract
    (a-arrow
      "Any"
      (a-id "Number" (xref "<global>" "Number"))
      (a-id "List" (xref "lists" "List"))
      (a-id "List" (xref "lists" "List"))
      (a-id "List" (xref "lists" "List"))
      (a-id "List" (xref "lists" "List"))
      "Any")
  ]
  @function["fold-while" #:contract (a-arrow "Any" "Any" "Any" "Any")]
  @function[
    "fold"
    #:contract (a-arrow "Any" "Any" (a-id "List" (xref "lists" "List")) "Any")
  ]
  @function[
    "fold2"
    #:contract
    (a-arrow
      "Any"
      "Any"
      (a-id "List" (xref "lists" "List"))
      (a-id "List" (xref "lists" "List"))
      "Any")
  ]
  @function[
    "fold3"
    #:contract
    (a-arrow
      "Any"
      "Any"
      (a-id "List" (xref "lists" "List"))
      (a-id "List" (xref "lists" "List"))
      (a-id "List" (xref "lists" "List"))
      "Any")
  ]
  @function[
    "fold4"
    #:contract
    (a-arrow
      "Any"
      "Any"
      (a-id "List" (xref "lists" "List"))
      (a-id "List" (xref "lists" "List"))
      (a-id "List" (xref "lists" "List"))
      (a-id "List" (xref "lists" "List"))
      "Any")
  ]
  @function[
    "fold_n"
    #:contract
    (a-arrow
      "Any"
      (a-id "Number" (xref "<global>" "Number"))
      "Any"
      (a-id "List" (xref "lists" "List"))
      "Any")
  ]
  @function[
    "index"
    #:contract (a-arrow "Any" (a-id "Number" (xref "<global>" "Number")) "Any")
  ]
}