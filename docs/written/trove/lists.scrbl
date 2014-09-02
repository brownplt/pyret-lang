#lang scribble/base
@(require "../../scribble-api.rkt" "../abbrevs.rkt")

@(define (list-method name)
  (method-doc "List" "link" name #:alt-docstrings ""))

@docmodule["lists"]{
  @section{The List Datatype}

  @data-spec2["List" (list "a") (list
  @singleton-spec2["List" "empty"]
  @constructor-spec["List" "link" (list `("first" ("type" "normal") ("contract" ,(a-id "a"))) `("rest" ("type" "normal") ("contract" ,(L-of "a"))))])]

  @nested[#:style 'inset]{
  @singleton-doc["List" "empty" (L-of "a")]
  @constructor-doc["List" "link" (list `("first" ("type" "normal") ("contract" ,(a-id "a"))) `("rest" ("type" "normal") ("contract" ,(L-of "a")))) (L-of "a")]{
    @;{
    @member-spec["first" #:type "normal" #:contract (a-id "a")]{
      The first element of the list
    }
    @member-spec["rest" #:type "normal" #:contract (L-of "a")]{
      The rest of the list
    }
    }
  }

  @function["is-empty" #:alt-docstrings ""]

  @function["is-link" #:alt-docstrings ""]

  }

@section{The @pyret{list} Constructor}

@collection-doc["list" (list (cons "elt" "a")) (L-of "a")]

Constructs a list out of the @pyret{elt}s by chaining @pyret-id{link}s,
ending in a single @pyret-id{empty}.

@examples{
check:
  [list: ] is empty
  [list: 1] is link(1, empty)
  [list: 1, 2] is link(1, link(2, empty))
end
}


@section{List Methods}

These methods are available on all lists (both @(tt "link") and @(tt "empty")
instances).  The examples show how to use the dot operator to access and call
them on particular lists.


@list-method["length"]

Returns the number of elements in the list.

@examples{
check:
  empty.length() is 0
  link("a", empty).length() is 1
end
}

@list-method["map"]

Applies @pyret{f} to each element of the list, constructing a new list out
of the return values in the same order.

@examples{
check:
  [list: 1, 2].map(lam(n): n + 1 end) is [list: 2, 3] 
  [list: 1, 2].map(num-tostring) is [list: "1", "2"] 
end
}

@list-method["each"]

Applies @pyret{f} to each element of the list, returning nothing

@examples{
check:
  var x = 1
  [list: 1, 2].each(lam(n): x := x + n end) is nothing
  x is 4
end
}

@list-method["filter"]

Applies @pyret{f} to each element of list, constructing a new list out of the
elements for which @pyret{f} returned @pyret{true}.

@examples{
check:
  fun length-is-one(s :: String): string-length(s) == 1 end
  [list: "ab", "a", "", "c"].filter(length-is-one) is [list: "a", "c"]
  
  [list: empty, link(1, empty), empty].filter(is-link)
    is [list: link(1, empty)]
end
}

@list-method["push"]

Returns @tt{link(elt, rest)}.

@examples{
check:
  empty.push("a") is link("a", empty)
  link("a", empty).push("b") is link("b", link("a", empty))
end
}

@list-method["split-at"]
@list-method["take"]
@list-method["drop"]
@list-method["get"]
@list-method["set"]
@list-method["foldl"]
@list-method["foldr"]
@list-method["member"]
@list-method["append"]
@list-method["last"]
@list-method["reverse"]
@list-method["sort"]
@list-method["sort-by"]
@list-method["join-str"]



@section{List Functions}

  These functions are available on the @tt{lists} module object.  So, for
  example, if you used @pyret{import lists as L}, you would write
  @pyret{L.fold} to access @pyret{fold} below.  The list module itself, along
  with many list functions, are available by default in Pyret.  Check out
  @seclink["<global>" "the section on global identifiers"] to learn more.

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
  ]{
    See, for example, @seclink[(xref "lists" "get-help") "get-help"]
  }
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
  @function[
    "member"
  ]
  @function[
    "member-with"
  ]
}
