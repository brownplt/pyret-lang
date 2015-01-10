#lang scribble/base
@(require "../../scribble-api.rkt")
@docmodule["lists"]{
  @; Ignored type testers
  @ignore[(list "is-empty" "is-link")]
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
            ;; N.B. Pyret contract: (List -> Number)
            
          ]{Returns the length of the list. Always zero for an empty.}
          @method-spec[
            "each"
            ;; N.B. Pyret contract: (List, (Any -> Nothing) -> Nothing)
            
          ]
          @method-spec[
            "map"
            ;; N.B. Pyret contract: (List, (Any -> Any) -> List64)
            
          ]
          @method-spec[
            "filter"
            ;; N.B. Pyret contract: (List, (Any -> Boolean) -> List64)
            
          ]
          @method-spec[
            "find"
            ;; N.B. Pyret contract: (List, (Any -> Boolean) -> O2.Option)
            
          ]
          @method-spec[
            "partition"
            ;; N.B. Pyret contract: (List, Any -> Any)
            
          ]
          @method-spec[
            "foldr"
            ;; N.B. Pyret contract: (List, Any, Any -> Any)
            
          ]
          @method-spec[
            "foldl"
            ;; N.B. Pyret contract: (List, Any, Any -> Any)
            
          ]
          @method-spec[
            "member"
            ;; N.B. Pyret contract: (List, Any -> Any)
            
          ]
          @method-spec[
            "append"
            ;; N.B. Pyret contract: (List, Any -> Any)
            
          ]
          @method-spec[
            "last"
            ;; N.B. Pyret contract: (List -> Any)
            
          ]
          @method-spec[
            "reverse"
            ;; N.B. Pyret contract: (List -> Any)
            
          ]
          @method-spec[
            "_tostring"
            ;; N.B. Pyret contract: (List -> Any)
            
          ]
          @method-spec[
            "_torepr"
            ;; N.B. Pyret contract: (List -> Any)
            
          ]
          @method-spec[
            "sort-by"
            ;; N.B. Pyret contract: (List, Any, Any -> Any)
            
          ]
          @method-spec[
            "sort"
            ;; N.B. Pyret contract: (List -> Any)
            
          ]
          @method-spec[
            "join-str"
            ;; N.B. Pyret contract: (List, Any -> Any)
            
          ]
        }
      }
      @constr-spec["link"]{
        @members{@member-spec["first"] @member-spec["rest"]}
        @with-members{
          @method-spec[
            "length"
            ;; N.B. Pyret contract: (List -> Any)
            
          ]{Returns the length of the list. This is always greater than zero for a link.}
          @method-spec[
            "each"
            ;; N.B. Pyret contract: (List, Any -> Any)
            
          ]
          @method-spec[
            "map"
            ;; N.B. Pyret contract: (List, Any -> Any)
            
          ]
          @method-spec[
            "filter"
            ;; N.B. Pyret contract: (List, Any -> Any)
            
          ]
          @method-spec[
            "partition"
            ;; N.B. Pyret contract: (List, Any -> Any)
            
          ]
          @method-spec[
            "find"
            ;; N.B. Pyret contract: (List, Any -> Any)
            
          ]
          @method-spec[
            "member"
            ;; N.B. Pyret contract: (List, Any -> Any)
            
          ]
          @method-spec[
            "foldr"
            ;; N.B. Pyret contract: (List, Any, Any -> Any)
            
          ]
          @method-spec[
            "foldl"
            ;; N.B. Pyret contract: (List, Any, Any -> Any)
            
          ]
          @method-spec[
            "append"
            ;; N.B. Pyret contract: (List, Any -> Any)
            
          ]
          @method-spec[
            "last"
            ;; N.B. Pyret contract: (List -> Any)
            
          ]
          @method-spec[
            "reverse"
            ;; N.B. Pyret contract: (List -> Any)
            
          ]
          @method-spec[
            "_tostring"
            ;; N.B. Pyret contract: (List -> Any)
            
          ]
          @method-spec[
            "_torepr"
            ;; N.B. Pyret contract: (List -> Any)
            
          ]
          @method-spec[
            "sort-by"
            ;; N.B. Pyret contract: (List, Any, Any -> Any)
            
          ]
          @method-spec[
            "sort"
            ;; N.B. Pyret contract: (List -> Any)
            
          ]
          @method-spec[
            "join-str"
            ;; N.B. Pyret contract: (List, Any -> Any)
            
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "_plus"
        ;; N.B. Pyret contract: (List, List64 -> Any)
        
      ]
      @method-spec[
        "push"
        ;; N.B. Pyret contract: (List, Any -> Any)
        
      ]
      @method-spec[
        "split-at"
        ;; N.B. Pyret contract: (List, Any -> Any)
        
      ]
      @method-spec[
        "take"
        ;; N.B. Pyret contract: (List, Any -> Any)
        
      ]
      @method-spec[
        "drop"
        ;; N.B. Pyret contract: (List, Any -> Any)
        
      ]
      @method-spec[
        "get"
        ;; N.B. Pyret contract: (List, Any -> Any)
        
      ]
      @method-spec[
        "set"
        ;; N.B. Pyret contract: (List, Any, Any -> Any)
        
      ]
    }
  }
  
  @section[#:tag "lists_Functions"]{Functions}
  @function[
    "get-help"
    #:examples
    '@{
      get-help([list: 1, 2, 3], 0) is 1
      get-help([list: ], 0) raises ""
    }
  ]
  @function[
    "set-help"
    #:examples
    '@{
      set-help([list: 1, 2, 3], 0, 5) is [list: 5, 2, 3]
      set-help([list: 1, 2, 3], 5, 5) raises ""
    }
  ]
  @function[
    "reverse-help"
    #:examples
    '@{
      reverse-help([list: ], [list: ]) is [list: ]
      reverse-help([list: 1, 3], [list: ]) is [list: 3, 1]
    }
  ]
  @function[
    "range"
    #:examples
    '@{
      range(0, 0) is [list: ]
      range(0, 1) is [list: 0]
      range(-5, 5) is [list: -5, -4, -3, -2, -1, 0, 1, 2, 3, 4]
    }
  ]
  @function[
    "repeat"
    #:examples
    '@{
      repeat(0, 10) is empty
      repeat(3, -1) is [list: -1, -1, -1]
      repeat(1, "foo") is link("foo", empty)
    }
  ]
  @function[
    "filter"
    #:examples
    '@{
      filter(lam(e): e > 5 end, [list: -1, 1]) is [list: ]
      filter(lam(e): e > 0 end, [list: -1, 1]) is [list: 1]
    }
  ]
  @function[
    "partition"
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
    #:examples
    '@{
      map(lam(_): raise("shipwrecked!") end, [list: ]) is [list: ]
      map(lam(_): 2 end, [list: 1, 2, 3, 4]) is [list: 2, 2, 2, 2]
      map(lam(x): x + 1 end, [list: 1, 2, 3, 4]) is [list: 2, 3, 4, 5]
    }
  ]
  @function[
    "map2"
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
  @function["map3"]
  @function["map4"]
  @function[
    "map_n"
    #:examples
    '@{
      map_n(lam(n, e): n end, 0, [list: "captain", "first mate"]) is [list: 0, 1]
    }
  ]
  @function["map2_n"]
  @function["map3_n"]
  @function["map4_n"]
  @function[
    "each"
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
  @function["each2"]
  @function["each3"]
  @function["each4"]
  @function["each_n"]
  @function["each2_n"]
  @function["each3_n"]
  @function["each4_n"]
  @function["fold-while"]
  @function[
    "fold"
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
  @function["fold2"]
  @function["fold3"]
  @function["fold4"]
  @function[
    "fold_n"
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
    #:examples
    '@{
      @; get-help([list: 1, 2, 3], 0) is 1
      @; get-help([list: ], 0) raises ""
      
    }
  ]
}
