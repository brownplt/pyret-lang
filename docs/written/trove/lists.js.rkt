#lang scribble/base
@(require "../../scribble-api.rkt" "../abbrevs.rkt")
@docmodule["lists"]{
  @; Unknown: PLEASE DOCUMENT
  @ignore[(list "list")]
  @section[#:tag "lists_DataTypes"]{Data types}
  @data-spec["List"]{
    @variants{
      @singleton-spec["empty"]{
        @with-members{
          @method-spec[
            "length"
            #:contract
            (a-arrow (L-of "a") N)
          ]{Returns the length of the list. Always zero for an empty.}
          @method-spec[
            "each"
            #:contract
            (a-arrow (L-of "a") (a-arrow "a" No) No)
          ]
          @method-spec[
            "map"
            #:contract
            (a-arrow (L-of "a") (a-arrow "a" "b") (L-of "a"))
          ]
          @method-spec[
            "filter"
            #:contract
            (a-arrow (L-of "a") (a-arrow "a" B) (L-of "a"))
          ]
          @method-spec[
            "find"
            #:contract
            (a-arrow (L-of "a") (a-arrow "a" B) (O-of "a"))
          ]
          @method-spec[
            "partition"
            #:contract
            (a-arrow (L-of "a") (a-arrow "a" B) (a-record (a-field "is-true" (L-of "a")) (a-field "is-false" (L-of "a"))))
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
    "range"
    #:contract (a-arrow "Any" "Any" "Any")
    #:examples
    '@{
      range(0, 0) is [list: ]
      range(0, 1) is [list: 0]
      range(-5, 5) is [list: -5, -4, -3, -2, -1, 0, 1, 2, 3, 4]
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
      repeat(1, "foo") is link("foo", empty)
    }
  ]
  @function[
    "filter"
    #:contract (a-arrow "Any" (a-id "List" (xref "lists" "List")) "Any")
    #:examples
    '@{
      filter(lam(e): e > 5 end, [list: -1, 1]) is [list: ]
      filter(lam(e): e > 0 end, [list: -1, 1]) is [list: 1]
    }
  ]
  @function[
    "partition"
    #:contract (a-arrow "Any" (a-id "List" (xref "lists" "List")) "Any")
    #:examples
    '@{
      partition(lam(e): e > 0 end, [list: -1, 1]) is
        { "is-true": [list: 1], "is-false": [list: -1] }
      partition(lam(e): e > 5 end, [list: -1, 1]) is
        { "is-true": [list: ], "is-false": [list: -1, 1] }
      partition(lam(e): e < 5 end, [list: -1, 1]) is
        { "is-true": [list: -1, 1], "is-false": [list: ] }
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
      find(lam(elt): elt > 4 end, [list: 1, 2, 3]) is none
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
      any(lam(n): n > 1 end, [list: 1, 2, 3]) is true
      any(lam(n): n > 3 end, [list: 1, 2, 3]) is false
      any(lam(x): true end, empty) is false
      any(lam(x): false end, empty) is false
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
      all(lam(n): n > 1 end, [list: 1, 2, 3]) is false
      all(lam(n): n <= 3 end, [list: 1, 2, 3]) is true
      all(lam(x): true end, empty) is true
      all(lam(x): false end, empty) is true
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
      all2(lam(n, m): n > m end, [list: 1, 2, 3], [list: 0, 1, 2]) is true
      all2(lam(n, m): (n + m) == 3 end, [list: 1, 2, 3], [list: 2, 1, 0]) is true
      all2(lam(n, m): n < m end, [list: 1, 2, 3], [list: 0, 1, 2]) is false
      all2(lam(_, _): true end, empty, empty) is true
      all2(lam(_, _): false end, empty, empty) is true
    }
  ]
  @function[
    "map"
    #:contract (a-arrow "Any" (a-id "List" (xref "lists" "List")) "Any")
    #:examples
    '@{
      map(lam(_): raise("shipwrecked!") end, [list: ]) is [list: ]
      map(lam(_): 2 end, [list: 1, 2, 3, 4]) is [list: 2, 2, 2, 2]
      map(lam(x): x + 1 end, [list: 1, 2, 3, 4]) is [list: 2, 3, 4, 5]
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
      map2(lam(_, _): raise("shipwrecked!") end,
        [list: ],
        [list: ]) is
        [list: ]
      map2(lam(x, y): x or y end, [list: true, false], [list: false, false]) is
        [list: true, false]
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
    #:examples
    '@{
      map_n(lam(n, e): n end, 0, [list: "captain", "first mate"]) is [list: 0, 1]
    }
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
    #:examples
    '@{
      let one-four = [list: 1, 2, 3, 4]:
        let  var counter = 0:
          each(lam(n): counter := counter + n end, one-four)
          counter is 1 + 2 + 3 + 4
          counter is 10
        end
        let  var counter = 1:
          each(lam(n): counter := counter * n end, one-four)
          counter is 1 * 2 * 3 * 4
          counter is 24
        end
      end
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
    #:examples
    '@{
      fold(lam(acc, cur): acc end, 1, [list: 1, 2, 3, 4]) is 1
      fold(lam(acc, cur): cur end, 1, [list: 1, 2, 3, 4]) is 4
      fold(lam(acc, cur): acc + cur end, 0, [list: 1, 2, 3, 4]) is 10
    }
    #:alt-docstrings '()
  ]{

    @pyret{fold} applies a procedure, @pyret{f}, to combine or "fold" the elements of
    a list into a single value.

    @pyret{f} takes two arguments. The first is the result thus far, the second is the
    current element of this list. @pyret{f} is initially invoked with base, and the first
    item of each list, as there is no result thus far. Each element from left to right is
    then successively fed to @pyret{f}, and the result of the whole @pyret{fold}
    application is the result of the last application of @pyret{f}. If the list is empty,
    base is returned.
  }
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
    #:examples
    '@{
      fold_n(lam(n, acc, _): n * acc end, 1, 1, [list: "a", "b", "c", "d"]) is
        1 * 2 * 3 * 4
      fold_n(lam(n, acc, cur): tostring(n) + " " + cur + ", " + acc end,
        95,
        "and so forth...",
        repeat(5, "jugs o' grog in the hold")) is
        "99 jugs o' grog in the hold, 98 jugs o' grog in the hold, "
        +
        "97 jugs o' grog in the hold, 96 jugs o' grog in the hold, "
          +
          "95 jugs o' grog in the hold, and so forth..."
      fold_n(lam(n, acc, cur): ((num-modulo(n, 2) == 0) or cur) and acc end,
        0,
        true,
        [list: false, true, false]) is
        true
    }
  ]
  @function[
    "index"
    #:contract (a-arrow "Any" (a-id "Number" (xref "<global>" "Number")) "Any")
  ]
}
