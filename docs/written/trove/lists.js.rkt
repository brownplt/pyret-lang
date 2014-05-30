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
          ]{Returns the length of the list. Always zero for an empty.}
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
          ]{Returns the length of the list. This is always greater than zero for a link.}
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
    #:examples
    '@{
      is-empty([list: ]) is true
      is-empty([list: 1]) is false
    }
    #:alt-docstrings '()
  ]{

    Returns true if @pyret{val} is an empty list, otherwise false.
  }
  @function[
    "is-link"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
    #:examples
    '@{
      is-link([list: ]) is false
      is-link([list: 1]) is true
    }
    #:alt-docstrings '()
  ]{

    Returns true if @pyret{val} is a non-empty list, otherwise false.
  }
  @function[
    "get-help"
    #:contract (a-arrow "Any" (a-id "Number" (xref "<global>" "Number")) "Any")
    #:examples
    '@{
      get-help([list: 1, 2, 3], 0) is 1
      get-help([list: ], 0) raises ""
    }
  ]
  @function[
    "set-help"
    #:contract
    (a-arrow "Any" (a-id "Number" (xref "<global>" "Number")) "Any" "Any")
    #:examples
    '@{
      set-help([list: 1, 2, 3], 0, 5) is [list: 5, 2, 3]
      set-help([list: 1, 2, 3], 5, 5) raises ""
    }
  ]
  @function[
    "reverse-help"
    #:contract (a-arrow "Any" "Any" "Any")
    #:examples
    '@{
      reverse-help([list: ], [list: ]) is [list: ]
      reverse-help([list: 1, 3], [list: ]) is [list: 3, 1]
    }
  ]
  @function[
    "raw-fold"
    #:contract (a-arrow "Any" "Any" (a-id "List" (xref "lists" "List")) "Any")
    #:examples
    '@{
      raw-fold(lam(x,y): x;, 1, [list: 1, 2, 3, 4]) is 1
      raw-fold(lam(x,y): y;, 1, [list: 1, 2, 3, 4]) is 4
      raw-fold(lam(x,y): x + y;, 0, [list: 1, 2, 3, 4]) is 10
    }
    #:alt-docstrings '()
  ]{

    @pyret{raw-fold} applies a procedure, @pyret{f}, to combine or "fold" the elements of
    a list into a single value.

    @pyret{f} takes two arguments. The first is the result thus far, the second is the
    current element of this list. @pyret{f} is initially invoked with base, and the first
    item of each list, as there is no result thus far. Each element from left to right is
    then successively fed to @pyret{f}, and the result of the whole @pyret{raw-fold}
    application is the result of the last application of @pyret{f}. If the list is empty,
    base is returned.
  }
  @function[
    "range"
    #:contract (a-arrow "Any" "Any" "Any")
    #:examples
    '@{
      range(0,0) is [list: ]
      range(0,1) is [list: 0]
      range(-5,5) is [list: -5, -4, -3, -2, -1, 0, 1, 2, 3, 4]
    }
  ]
  @function[
    "repeat"
    #:contract
    (a-arrow
      (a-id "Number" (xref "<global>" "Number"))
      "Any"
      (a-id "List" (xref "lists" "List")))
    #:examples
    '@{
      repeat(0, 10) is empty
      repeat(3, -1) is [list: -1, -1, -1]
    }
  ]
  @function[
    "filter"
    #:contract (a-arrow "Any" (a-id "List" (xref "lists" "List")) "Any")
    #:examples
    '@{
      filter(lam(e): e > 5;, [list: -1, 1]) is [list: ]
      filter(lam(e): e > 0;, [list: -1, 1]) is [list: 1]
    }
  ]
  @function[
    "partition"
    #:contract (a-arrow "Any" (a-id "List" (xref "lists" "List")) "Any")
    #:examples
    '@{
      partition(lam(e): e > 0;, [list: -1, 1])
        is { is-true: [list: 1], is-false : [list: -1] }
    }
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
      find(lam(elt): elt > 1 end, [list: 1, 2, 3]) is some(2)
      find(lam(elt): true end, [list: "find-me", "miss-me"]) is some("find-me")
      find(lam(elt): true end, empty) is none
      find(lam(elt): false end, [list: "miss-me"]) is none
      find(lam(elt): false end, empty) is none
    }
    #:alt-docstrings '()
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
      let one-four = [list: 1, 2, 3, 4]:
        split-at(0, one-four) is { prefix: empty, suffix: one-four }
        split-at(4, one-four) is { prefix: one-four, suffix: empty }
        split-at(2, one-four) is
          { prefix: [list: 1, 2], suffix: [list: 3, 4] }
        split-at(-1, one-four) raises "Invalid index"
        split-at(5, one-four) raises "Index too large"
      end
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
      any(fun(n): n > 1 end, link(1, link(2, link(3, empty)))) is true
      any(fun(n): n > 3 end, link(1, link(2, link(3, empty)))) is false
      any(fun(x): true end, empty) is false
      any(fun(x): false end, empty) is false
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
      all(fun(n): n > 1 end, link(1, link(2, link(3, empty)))) is false
      all(fun(n): n <= 3 end, link(1, link(2, link(3, empty)))) is true
      all(fun(x): true end, empty) is true
      all(fun(x): false end, empty) is true
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
      all2(fun(n, m): n > m end,
        link(1, link(2, link(3, empty))),
        link(0, link(1, link(2, empty)))) is
        true
      all2(fun(n, m): (n + m) == 3 end,
        link(1, link(2, link(3, empty))),
        link(2, link(1, link(0, empty)))) is
        true
      all2(fun(n, m): n < m end,
        link(1, link(2, link(3, empty))),
        link(0, link(1, link(2, empty)))) is
        false
      all2(fun($underscore, $underscore): true end, empty, empty) is true
      all2(fun($underscore, $underscore): false end, empty, empty) is true
    }
  ]
  @function[
    "map"
    #:contract (a-arrow "Any" (a-id "List" (xref "lists" "List")) "Any")
    #:examples
    '@{

    }
  ]
  @function[
    "map2"
    #:contract
    (a-arrow
      "Any"
      (a-id "List" (xref "lists" "List"))
      (a-id "List" (xref "lists" "List"))
      "Any")
    #:examples
    '@{

    }
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
    #:examples
    '@{

    }
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
    #:examples
    '@{

    }
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
    #:examples
    '@{

    }
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
    #:examples
    '@{

    }
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
    #:examples
    '@{

    }
  ]
  @function[
    "each"
    #:contract (a-arrow "Any" (a-id "List" (xref "lists" "List")) "Any")
    #:examples
    '@{

    }
  ]
  @function[
    "each2"
    #:contract
    (a-arrow
      "Any"
      (a-id "List" (xref "lists" "List"))
      (a-id "List" (xref "lists" "List"))
      "Any")
    #:examples
    '@{

    }
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
    #:examples
    '@{

    }
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
    #:examples
    '@{

    }
  ]
  @function[
    "each_n"
    #:contract
    (a-arrow
      "Any"
      (a-id "Number" (xref "<global>" "Number"))
      (a-id "List" (xref "lists" "List"))
      "Any")
    #:examples
    '@{

    }
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
    #:examples
    '@{

    }
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
    #:examples
    '@{

    }
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
    #:examples
    '@{

    }
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
    #:examples
    '@{

    }
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
    #:examples
    '@{

    }
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
    #:examples
    '@{

    }
  ]
  @function[
    "index"
    #:contract (a-arrow "Any" (a-id "Number" (xref "<global>" "Number")) "Any")
    #:examples
    '@{

    }
  ]
}
