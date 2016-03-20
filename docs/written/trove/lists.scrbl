#lang scribble/base
@(require "../../scribble-api.rkt" "../abbrevs.rkt")

@(append-gen-docs
'(module
  "lists"
  (path "src/arr/base/lists.arr")
  (unknown-item
    (name "none")
    ;; O9.none
    )
  (unknown-item
    (name "is-none")
    ;; O9.is-none
    )
  (unknown-item
    (name "some")
    ;; O9.some
    )
  (unknown-item
    (name "is-some")
    ;; O9.is-some
    )
  (unknown-item
    (name "left")
    ;; E11.left
    )
  (unknown-item
    (name "right")
    ;; E11.right
    )
  (data-spec
    (name "List")
    (type-vars (a74))
    (variants ("empty" "link"))
    (shared
      ((method-spec
        (name "_output")
        (arity 1)
        (params ())
        (args ("self"))
        (return
          (a-compound
            (a-dot "VS" "ValueSkeleton")
            (xref "valueskeleton" "ValueSkeleton")))
        (contract
          (a-arrow
            (a-id "List" (xref "lists" "List"))
            (a-compound
              (a-dot "VS" "ValueSkeleton")
              (xref "valueskeleton" "ValueSkeleton")))))
      (method-spec
        (name "_plus")
        (arity 2)
        (params ())
        (args ("self" "other"))
        (return (a-app (a-id "List" (xref "lists" "List")) "a"))
        (contract
          (a-arrow
            (a-id "List" (xref "lists" "List"))
            (a-app (a-id "List" (xref "lists" "List")) "a")
            (a-app (a-id "List" (xref "lists" "List")) "a"))))
      (method-spec
        (name "push")
        (arity 2)
        (params ())
        (args ("self" "elt"))
        (return (a-app (a-id "List" (xref "lists" "List")) "a"))
        (contract
          (a-arrow
            (a-id "List" (xref "lists" "List"))
            "a"
            (a-app (a-id "List" (xref "lists" "List")) "a")))
        (doc "Adds an element to the front of the list, returning a new list"))
      (method-spec
        (name "split-at")
        (arity 2)
        (params ())
        (args ("self" "n"))
        (return
          (a-record
            (a-field "prefix" (a-app (a-id "List" (xref "lists" "List")) "a"))
            (a-field "suffix" (a-app (a-id "List" (xref "lists" "List")) "a"))))
        (contract
          (a-arrow
            (a-id "List" (xref "lists" "List"))
            (a-id "Number" (xref "<global>" "Number"))
            (a-record
              (a-field "prefix" (a-app (a-id "List" (xref "lists" "List")) "a"))
              (a-field "suffix" (a-app (a-id "List" (xref "lists" "List")) "a")))))
        (doc
          "Splits this list into two lists, one containing the first n elements, and the other containing the rest"))
      (method-spec
        (name "take")
        (arity 2)
        (params ())
        (args ("self" "n"))
        (return (a-app (a-id "List" (xref "lists" "List")) "a"))
        (contract
          (a-arrow
            (a-id "List" (xref "lists" "List"))
            (a-id "Number" (xref "<global>" "Number"))
            (a-app (a-id "List" (xref "lists" "List")) "a")))
        (doc "Returns the first n elements of this list"))
      (method-spec
        (name "drop")
        (arity 2)
        (params ())
        (args ("self" "n"))
        (return (a-app (a-id "List" (xref "lists" "List")) "a"))
        (contract
          (a-arrow
            (a-id "List" (xref "lists" "List"))
            (a-id "Number" (xref "<global>" "Number"))
            (a-app (a-id "List" (xref "lists" "List")) "a")))
        (doc "Returns all but the first n elements of this list"))
      (method-spec
        (name "get")
        (arity 2)
        (params ())
        (args ("self" "n"))
        (return "a")
        (contract
          (a-arrow
            (a-id "List" (xref "lists" "List"))
            (a-id "Number" (xref "<global>" "Number"))
            "a"))
        (doc
          "Returns the nth element of this list, or raises an error if n is out of range"))
      (method-spec
        (name "set")
        (arity 3)
        (params ())
        (args ("self" "n" "e"))
        (return (a-app (a-id "List" (xref "lists" "List")) "a"))
        (contract
          (a-arrow
            (a-id "List" (xref "lists" "List"))
            (a-id "Number" (xref "<global>" "Number"))
            "a"
            (a-app (a-id "List" (xref "lists" "List")) "a")))
        (doc
          "Returns a new list with the nth element set to the given value, or raises an error if n is out of range"))
      (method-spec
        (name "remove")
        (arity 2)
        (params ())
        (args ("self" "e"))
        (return (a-app (a-id "List" (xref "lists" "List")) "a"))
        (contract
          (a-arrow
            (a-id "List" (xref "lists" "List"))
            "a"
            (a-app (a-id "List" (xref "lists" "List")) "a")))
        (doc
          "Returns the list without the element if found, or the whole list if it is not")))))
  
  (singleton-spec
    (name "empty")
    (with-members
      ((method-spec
        (name "length")
        (arity 1)
        (params ())
        (args ("self"))
        (return (a-id "Number" (xref "<global>" "Number")))
        (contract
          (a-arrow
            (a-id "is-List" (xref "lists" "is-List"))
            (a-id "Number" (xref "<global>" "Number"))))
        (doc
          "Takes no other arguments and returns the number of links in the list"))
      (method-spec
        (name "each")
        (arity 2)
        (params ())
        (args ("self" "f"))
        (return (a-id "Nothing" (xref "<global>" "Nothing")))
        (contract
          (a-arrow
            (a-id "is-List" (xref "lists" "is-List"))
            (a-arrow "a" (a-id "Nothing" (xref "<global>" "Nothing")))
            (a-id "Nothing" (xref "<global>" "Nothing"))))
        (doc
          "Takes a function and calls that function for each element in the list. Returns nothing"))
      (method-spec
        (name "map")
        (arity 2)
        (params ("b"))
        (args ("self" "f"))
        (return (a-app (a-id "List" (xref "lists" "List")) "b"))
        (contract
          (a-arrow
            (a-id "is-List" (xref "lists" "is-List"))
            (a-arrow "a" "b")
            (a-app (a-id "List" (xref "lists" "List")) "b")))
        (doc
          "Takes a function and returns a list of the result of applying that function every element in this list"))
      (method-spec
        (name "filter")
        (arity 2)
        (params ())
        (args ("self" "f"))
        (return (a-app (a-id "List" (xref "lists" "List")) "a"))
        (contract
          (a-arrow
            (a-id "is-List" (xref "lists" "is-List"))
            (a-arrow "a" (a-id "Boolean" (xref "<global>" "Boolean")))
            (a-app (a-id "List" (xref "lists" "List")) "a")))
        (doc
          "Takes a predicate and returns a list containing the items in this list for which the predicate returns true."))
      (method-spec
        (name "find")
        (arity 2)
        (params ())
        (args ("self" "f"))
        (return
          (a-app (a-compound (a-dot "O" "Option") (xref "option" "Option")) "a"))
        (contract
          (a-arrow
            (a-id "is-List" (xref "lists" "is-List"))
            (a-arrow "a" (a-id "Boolean" (xref "<global>" "Boolean")))
            (a-app
              (a-compound (a-dot "O" "Option") (xref "option" "Option"))
              "a")))
        (doc
          "Takes a predicate and returns on option containing either the first item in this list that passes the predicate, or none"))
      (method-spec
        (name "partition")
        (arity 2)
        (params ())
        (args ("self" "f"))
        (return
          (a-record
            (a-field "is-true" (a-app (a-id "List" (xref "lists" "List")) "a"))
            (a-field "is-false" (a-app (a-id "List" (xref "lists" "List")) "a"))))
        (contract
          (a-arrow
            (a-id "is-List" (xref "lists" "is-List"))
            (a-arrow "a" (a-id "Boolean" (xref "<global>" "Boolean")))
            (a-record
              (a-field
                "is-true"
                (a-app (a-id "List" (xref "lists" "List")) "a"))
              (a-field
                "is-false"
                (a-app (a-id "List" (xref "lists" "List")) "a")))))
        (doc
          "Takes a predicate and returns an object with two fields:\n            the 'is-true' field contains the list of items in this list for which the predicate holds,\n            and the 'is-false' field contains the list of items in this list for which the predicate fails"))
      (method-spec
        (name "foldr")
        (arity 3)
        (params ("b"))
        (args ("self" "f" "base"))
        (return "b")
        (contract
          (a-arrow
            (a-id "is-List" (xref "lists" "is-List"))
            (a-arrow "a" "b" "b")
            "b"
            "b"))
        (doc
          "Takes a function and an initial value, and folds the function over this list from the right,\n            starting with the base value"))
      (method-spec
        (name "foldl")
        (arity 3)
        (params ("b"))
        (args ("self" "f" "base"))
        (return "b")
        (contract
          (a-arrow
            (a-id "is-List" (xref "lists" "is-List"))
            (a-arrow "a" "b" "b")
            "b"
            "b"))
        (doc
          "Takes a function and an initial value, and folds the function over this list from the left,\n            starting with the base value"))
      (method-spec
        (name "all")
        (arity 2)
        (params ())
        (args ("self" "f"))
        (return (a-id "Boolean" (xref "<global>" "Boolean")))
        (contract
          (a-arrow
            (a-id "is-List" (xref "lists" "is-List"))
            (a-arrow "a" (a-id "Boolean" (xref "<global>" "Boolean")))
            (a-id "Boolean" (xref "<global>" "Boolean"))))
        (doc
          "Returns true if the given predicate is true for every element in this list"))
      (method-spec
        (name "any")
        (arity 2)
        (params ())
        (args ("self" "f"))
        (return (a-id "Boolean" (xref "<global>" "Boolean")))
        (contract
          (a-arrow
            (a-id "is-List" (xref "lists" "is-List"))
            (a-arrow "a" (a-id "Boolean" (xref "<global>" "Boolean")))
            (a-id "Boolean" (xref "<global>" "Boolean"))))
        (doc
          "Returns true if the given predicate is true for any element in this list"))
      (method-spec
        (name "member")
        (arity 2)
        (params ())
        (args ("self" "elt"))
        (return (a-id "Boolean" (xref "<global>" "Boolean")))
        (contract
          (a-arrow
            (a-id "is-List" (xref "lists" "is-List"))
            "a"
            (a-id "Boolean" (xref "<global>" "Boolean"))))
        (doc
          "Returns true when the given element is equal to a member of this list"))
      (method-spec
        (name "append")
        (arity 2)
        (params ())
        (args ("self" "other"))
        (return (a-app (a-id "List" (xref "lists" "List")) "a"))
        (contract
          (a-arrow
            (a-id "is-List" (xref "lists" "is-List"))
            (a-app (a-id "List" (xref "lists" "List")) "a")
            (a-app (a-id "List" (xref "lists" "List")) "a")))
        (doc
          "Takes a list and returns the result of appending the given list to this list"))
      (method-spec
        (name "last")
        (arity 1)
        (params ())
        (args ("self"))
        (return "a")
        (contract (a-arrow (a-id "is-List" (xref "lists" "is-List")) "a"))
        (doc
          "Returns the last element of this list, or raises an error if the list is empty"))
      (method-spec
        (name "reverse")
        (arity 1)
        (params ())
        (args ("self"))
        (return (a-app (a-id "List" (xref "lists" "List")) "a"))
        (contract
          (a-arrow
            (a-id "is-List" (xref "lists" "is-List"))
            (a-app (a-id "List" (xref "lists" "List")) "a")))
        (doc
          "Returns a new list containing the same elements as this list, in reverse order"))
      (method-spec
        (name "_tostring")
        (arity 2)
        (params ())
        (args ("self" "tostring"))
        (return (a-id "String" (xref "<global>" "String")))
        (contract
          (a-arrow
            (a-id "is-List" (xref "lists" "is-List"))
            (a-arrow "Any" (a-id "String" (xref "<global>" "String")))
            (a-id "String" (xref "<global>" "String")))))
      (method-spec
        (name "_torepr")
        (arity 2)
        (params ())
        (args ("self" "torepr"))
        (return (a-id "String" (xref "<global>" "String")))
        (contract
          (a-arrow
            (a-id "is-List" (xref "lists" "is-List"))
            (a-arrow "Any" (a-id "String" (xref "<global>" "String")))
            (a-id "String" (xref "<global>" "String")))))
      (method-spec
        (name "sort-by")
        (arity 3)
        (params ())
        (args ("self" "cmp" "eq"))
        (return (a-app (a-id "List" (xref "lists" "List")) "a"))
        (contract
          (a-arrow
            (a-id "is-List" (xref "lists" "is-List"))
            (a-arrow "a" "a" (a-id "Boolean" (xref "<global>" "Boolean")))
            (a-arrow "a" "a" (a-id "Boolean" (xref "<global>" "Boolean")))
            (a-app (a-id "List" (xref "lists" "List")) "a")))
        (doc
          "Takes a comparator to check for elements that are strictly greater\n            or less than one another, and an equality procedure for elements that are\n            equal, and sorts the list accordingly.  The sort is not guaranteed to be stable."))
      (method-spec
        (name "sort")
        (arity 1)
        (params ())
        (args ("self"))
        (return (a-app (a-id "List" (xref "lists" "List")) "a"))
        (contract
          (a-arrow
            (a-id "is-List" (xref "lists" "is-List"))
            (a-app (a-id "List" (xref "lists" "List")) "a")))
        (doc
          "Returns a new list whose contents are the smae as those in this list,\n            sorted by the default ordering and equality"))
      (method-spec
        (name "join-str")
        (arity 2)
        (params ())
        (args ("self" "str"))
        (return (a-id "String" (xref "<global>" "String")))
        (contract
          (a-arrow
            (a-id "is-List" (xref "lists" "is-List"))
            (a-id "String" (xref "<global>" "String"))
            (a-id "String" (xref "<global>" "String"))))
        (doc
          "Returns a string containing the tostring() forms of the elements of this list,\n            joined by the provided separator string")))))
  (fun-spec
    (name "is-empty")
    (arity 1)
    (params [list: ])
    (args ("val"))
    (return (a-id "Boolean" (xref "<global>" "Boolean")))
    (contract (a-arrow "Any" (a-id "Boolean" (xref "<global>" "Boolean"))))
    (doc "Checks whether the provided argument is in fact an empty"))
  (constr-spec
    (name "link")
    (members
      (("first" (type normal) (contract "a"))
      ("rest"
        (type normal)
        (contract (a-app (a-id "List" (xref "lists" "List")) "a")))))
    (with-members
      ((method-spec
        (name "length")
        (arity 1)
        (params ())
        (args ("self"))
        (return (a-id "Number" (xref "<global>" "Number")))
        (contract
          (a-arrow
            (a-id "is-List" (xref "lists" "is-List"))
            (a-id "Number" (xref "<global>" "Number"))))
        (doc
          "Takes no other arguments and returns the number of links in the list"))
      (method-spec
        (name "each")
        (arity 2)
        (params ())
        (args ("self" "f"))
        (return (a-id "Nothing" (xref "<global>" "Nothing")))
        (contract
          (a-arrow
            (a-id "is-List" (xref "lists" "is-List"))
            (a-arrow "a" (a-id "Nothing" (xref "<global>" "Nothing")))
            (a-id "Nothing" (xref "<global>" "Nothing"))))
        (doc
          "Takes a function and calls that function for each element in the list. Returns nothing"))
      (method-spec
        (name "map")
        (arity 2)
        (params ("b"))
        (args ("self" "f"))
        (return (a-app (a-id "List" (xref "lists" "List")) "b"))
        (contract
          (a-arrow
            (a-id "is-List" (xref "lists" "is-List"))
            (a-arrow "a" "b")
            (a-app (a-id "List" (xref "lists" "List")) "b")))
        (doc
          "Takes a function and returns a list of the result of applying that function every element in this list"))
      (method-spec
        (name "filter")
        (arity 2)
        (params ())
        (args ("self" "f"))
        (return (a-app (a-id "List" (xref "lists" "List")) "a"))
        (contract
          (a-arrow
            (a-id "is-List" (xref "lists" "is-List"))
            (a-arrow "a" (a-id "Boolean" (xref "<global>" "Boolean")))
            (a-app (a-id "List" (xref "lists" "List")) "a")))
        (doc
          "Takes a predicate and returns a list containing the items in this list for which the predicate returns true."))
      (method-spec
        (name "partition")
        (arity 2)
        (params ())
        (args ("self" "f"))
        (return
          (a-record
            (a-field "is-true" (a-app (a-id "List" (xref "lists" "List")) "a"))
            (a-field "is-false" (a-app (a-id "List" (xref "lists" "List")) "a"))))
        (contract
          (a-arrow
            (a-id "is-List" (xref "lists" "is-List"))
            (a-arrow "a" (a-id "Boolean" (xref "<global>" "Boolean")))
            (a-record
              (a-field
                "is-true"
                (a-app (a-id "List" (xref "lists" "List")) "a"))
              (a-field
                "is-false"
                (a-app (a-id "List" (xref "lists" "List")) "a")))))
        (doc
          "Takes a predicate and returns an object with two fields:\n            the 'is-true' field contains the list of items in this list for which the predicate holds,\n            and the 'is-false' field contains the list of items in this list for which the predicate fails"))
      (method-spec
        (name "find")
        (arity 2)
        (params ())
        (args ("self" "f"))
        (return
          (a-app (a-compound (a-dot "O" "Option") (xref "option" "Option")) "a"))
        (contract
          (a-arrow
            (a-id "is-List" (xref "lists" "is-List"))
            (a-arrow "a" (a-id "Boolean" (xref "<global>" "Boolean")))
            (a-app
              (a-compound (a-dot "O" "Option") (xref "option" "Option"))
              "a")))
        (doc
          "Takes a predicate and returns on option containing either the first item in this list that passes the predicate, or none"))
      (method-spec
        (name "member")
        (arity 2)
        (params ())
        (args ("self" "elt"))
        (return (a-id "Boolean" (xref "<global>" "Boolean")))
        (contract
          (a-arrow
            (a-id "is-List" (xref "lists" "is-List"))
            "a"
            (a-id "Boolean" (xref "<global>" "Boolean"))))
        (doc
          "Returns true when the given element is equal to a member of this list"))
      (method-spec
        (name "foldr")
        (arity 3)
        (params ("b"))
        (args ("self" "f" "base"))
        (return "b")
        (contract
          (a-arrow
            (a-id "is-List" (xref "lists" "is-List"))
            (a-arrow "a" "b" "b")
            "b"
            "b"))
        (doc
          "Takes a function and an initial value, and folds the function over this list from the right,\n            starting with the initial value"))
      (method-spec
        (name "foldl")
        (arity 3)
        (params ("b"))
        (args ("self" "f" "base"))
        (return "b")
        (contract
          (a-arrow
            (a-id "is-List" (xref "lists" "is-List"))
            (a-arrow "a" "b" "b")
            "b"
            "b"))
        (doc
          "Takes a function and an initial value, and folds the function over this list from the left,\n            starting with the initial value"))
      (method-spec
        (name "all")
        (arity 2)
        (params ())
        (args ("self" "f"))
        (return (a-id "Boolean" (xref "<global>" "Boolean")))
        (contract
          (a-arrow
            (a-id "is-List" (xref "lists" "is-List"))
            (a-arrow "a" (a-id "Boolean" (xref "<global>" "Boolean")))
            (a-id "Boolean" (xref "<global>" "Boolean"))))
        (doc
          "Returns true if the given predicate is true for every element in this list"))
      (method-spec
        (name "any")
        (arity 2)
        (params ())
        (args ("self" "f"))
        (return (a-id "Boolean" (xref "<global>" "Boolean")))
        (contract
          (a-arrow
            (a-id "is-List" (xref "lists" "is-List"))
            (a-arrow "a" (a-id "Boolean" (xref "<global>" "Boolean")))
            (a-id "Boolean" (xref "<global>" "Boolean"))))
        (doc
          "Returns true if the given predicate is true for any element in this list"))
      (method-spec
        (name "append")
        (arity 2)
        (params ())
        (args ("self" "other"))
        (return (a-app (a-id "List" (xref "lists" "List")) "a"))
        (contract
          (a-arrow
            (a-id "is-List" (xref "lists" "is-List"))
            (a-app (a-id "List" (xref "lists" "List")) "a")
            (a-app (a-id "List" (xref "lists" "List")) "a")))
        (doc
          "Takes a list and returns the result of appending the given list to this list"))
      (method-spec
        (name "last")
        (arity 1)
        (params ())
        (args ("self"))
        (return "a")
        (contract (a-arrow (a-id "is-List" (xref "lists" "is-List")) "a"))
        (doc
          "Returns the last element of this list, or raises an error if the list is empty"))
      (method-spec
        (name "reverse")
        (arity 1)
        (params ())
        (args ("self"))
        (return (a-app (a-id "List" (xref "lists" "List")) "a"))
        (contract
          (a-arrow
            (a-id "is-List" (xref "lists" "is-List"))
            (a-app (a-id "List" (xref "lists" "List")) "a")))
        (doc
          "Returns a new list containing the same elements as this list, in reverse order"))
      (method-spec
        (name "_tostring")
        (arity 2)
        (params ())
        (args ("self" "tostring"))
        (return (a-id "String" (xref "<global>" "String")))
        (contract
          (a-arrow
            (a-id "is-List" (xref "lists" "is-List"))
            (a-arrow "Any" (a-id "String" (xref "<global>" "String")))
            (a-id "String" (xref "<global>" "String")))))
      (method-spec
        (name "_torepr")
        (arity 2)
        (params ())
        (args ("self" "torepr"))
        (return (a-id "String" (xref "<global>" "String")))
        (contract
          (a-arrow
            (a-id "is-List" (xref "lists" "is-List"))
            (a-arrow "Any" (a-id "String" (xref "<global>" "String")))
            (a-id "String" (xref "<global>" "String")))))
      (method-spec
        (name "sort-by")
        (arity 3)
        (params ())
        (args ("self" "cmp" "eq"))
        (return (a-app (a-id "List" (xref "lists" "List")) "a"))
        (contract
          (a-arrow
            (a-id "is-List" (xref "lists" "is-List"))
            (a-arrow "a" "a" (a-id "Boolean" (xref "<global>" "Boolean")))
            (a-arrow "a" "a" (a-id "Boolean" (xref "<global>" "Boolean")))
            (a-app (a-id "List" (xref "lists" "List")) "a")))
        (doc
          "Takes a comparator to check for elements that are strictly greater\n            or less than one another, and an equality procedure for elements that are\n            equal, and sorts the list accordingly.  The sort is not guaranteed to be stable."))
      (method-spec
        (name "sort")
        (arity 1)
        (params ())
        (args ("self"))
        (return (a-app (a-id "List" (xref "lists" "List")) "a"))
        (contract
          (a-arrow
            (a-id "is-List" (xref "lists" "is-List"))
            (a-app (a-id "List" (xref "lists" "List")) "a")))
        (doc
          "Returns a new list whose contents are the same as those in this list,\n            sorted by the default ordering and equality"))
      (method-spec
        (name "join-str")
        (arity 2)
        (params ())
        (args ("self" "str"))
        (return (a-id "String" (xref "<global>" "String")))
        (contract
          (a-arrow
            (a-id "is-List" (xref "lists" "is-List"))
            (a-id "String" (xref "<global>" "String"))
            (a-id "String" (xref "<global>" "String"))))
        (doc
          "Returns a string containing the tostring() forms of the elements of this list,\n            joined by the provided separator string")))))
  (fun-spec
    (name "is-link")
    (arity 1)
    (params [list: ])
    (args ("val"))
    (return (a-id "Boolean" (xref "<global>" "Boolean")))
    (contract (a-arrow "Any" (a-id "Boolean" (xref "<global>" "Boolean"))))
    (doc "Checks whether the provided argument is in fact a link"))
  (fun-spec
    (name "get")
    (arity 2)
    (params [list: leaf("a")])
    (args ("lst" "n"))
    (return "a")
    (contract
      (a-arrow
        (a-app (a-id "List" (xref "lists" "List")) "a")
        (a-id "Number" (xref "<global>" "Number"))
        "a"))
    (doc
      "Returns the nth element of the given list, or raises an error if n is out of range"))
  (fun-spec
    (name "set")
    (arity 3)
    (params [list: leaf("a")])
    (args ("lst" "n" "v"))
    (return "a")
    (contract
      (a-arrow
        (a-app (a-id "List" (xref "lists" "List")) "a")
        (a-id "Number" (xref "<global>" "Number"))
        "Any"
        "a"))
    (doc
      "Returns a new list with the same values as the given list but with the nth element\n        set to the given value, or raises an error if n is out of range"))
  (fun-spec
    (name "reverse-help")
    (arity 2)
    (params [list: leaf("a")])
    (args ("lst" "acc"))
    (return (a-app (a-id "List" (xref "lists" "List")) "a"))
    (contract
      (a-arrow
        (a-app (a-id "List" (xref "lists" "List")) "a")
        (a-app (a-id "List" (xref "lists" "List")) "a")
        (a-app (a-id "List" (xref "lists" "List")) "a")))
    (doc
      "Returns a new list containing the same elements as this list, in reverse order"))
  (fun-spec
    (name "reverse")
    (arity 1)
    (params [list: leaf("a")])
    (args ("lst"))
    (return (a-app (a-id "List" (xref "lists" "List")) "a"))
    (contract
      (a-arrow
        (a-app (a-id "List" (xref "lists" "List")) "a")
        (a-app (a-id "List" (xref "lists" "List")) "a"))))
  (fun-spec
    (name "range")
    (arity 2)
    (params [list: ])
    (args ("start" "stop"))
    (return
      (a-app
        (a-id "List" (xref "lists" "List"))
        (a-id "Number" (xref "<global>" "Number"))))
    (contract
      (a-arrow
        (a-id "Number" (xref "<global>" "Number"))
        (a-id "Number" (xref "<global>" "Number"))
        (a-app
          (a-id "List" (xref "lists" "List"))
          (a-id "Number" (xref "<global>" "Number")))))
    (doc "Creates a list of numbers, starting with start, ending with stop-1"))
  (fun-spec
    (name "range-by")
    (arity 3)
    (params [list: ])
    (args ("start" "stop" "delta"))
    (return
      (a-app
        (a-id "List" (xref "lists" "List"))
        (a-id "Number" (xref "<global>" "Number"))))
    (contract
      (a-arrow
        (a-id "Number" (xref "<global>" "Number"))
        (a-id "Number" (xref "<global>" "Number"))
        (a-id "Number" (xref "<global>" "Number"))
        (a-app
          (a-id "List" (xref "lists" "List"))
          (a-id "Number" (xref "<global>" "Number")))))
    (doc
      "Creates a list of numbers, starting with start, in intervals of delta,\n          until reaching (but not including) stop"))
  (fun-spec
    (name "repeat")
    (arity 2)
    (params [list: leaf("a")])
    (args ("n" "e"))
    (return (a-app (a-id "List" (xref "lists" "List")) "a"))
    (contract
      (a-arrow
        (a-id "Number" (xref "<global>" "Number"))
        "a"
        (a-app (a-id "List" (xref "lists" "List")) "a")))
    (doc "Creates a list with n copies of e"))
  (fun-spec
    (name "filter")
    (arity 2)
    (params [list: leaf("a")])
    (args ("f" "lst"))
    (return (a-app (a-id "List" (xref "lists" "List")) "a"))
    (contract
      (a-arrow
        (a-arrow "a" (a-id "Boolean" (xref "<global>" "Boolean")))
        (a-app (a-id "List" (xref "lists" "List")) "a")
        (a-app (a-id "List" (xref "lists" "List")) "a")))
    (doc "Returns the subset of lst for which f(elem) is true"))
  (fun-spec
    (name "partition")
    (arity 2)
    (params [list: leaf("a")])
    (args ("f" "lst"))
    (return
      (a-record
        (a-field "is-true" (a-app (a-id "List" (xref "lists" "List")) "a"))
        (a-field "is-false" (a-app (a-id "List" (xref "lists" "List")) "a"))))
    (contract
      (a-arrow
        (a-arrow "a" (a-id "Boolean" (xref "<global>" "Boolean")))
        (a-app (a-id "List" (xref "lists" "List")) "a")
        (a-record
          (a-field "is-true" (a-app (a-id "List" (xref "lists" "List")) "a"))
          (a-field "is-false" (a-app (a-id "List" (xref "lists" "List")) "a")))))
    (doc
      "Splits the list into two lists, one for which f(elem) is true, and one for which f(elem) is false"))
  (fun-spec
    (name "remove")
    (arity 2)
    (params [list: leaf("a")])
    (args ("lst" "elt"))
    (return (a-app (a-id "List" (xref "lists" "List")) "a"))
    (contract
      (a-arrow
        (a-app (a-id "List" (xref "lists" "List")) "a")
        "a"
        (a-app (a-id "List" (xref "lists" "List")) "a")))
    (doc
      "Returns the list without the element if found, or the whole list if it is not"))
  (fun-spec
    (name "find")
    (arity 2)
    (params [list: leaf("a")])
    (args ("f" "lst"))
    (return
      (a-app (a-compound (a-dot "O" "Option") (xref "option" "Option")) "a"))
    (contract
      (a-arrow
        (a-arrow "a" (a-id "Boolean" (xref "<global>" "Boolean")))
        (a-app (a-id "List" (xref "lists" "List")) "a")
        (a-app (a-compound (a-dot "O" "Option") (xref "option" "Option")) "a")))
    (doc
      "Returns some(elem) where elem is the first elem in lst for which\n        f(elem) returns true, or none otherwise"))
  (fun-spec
    (name "split-at")
    (arity 2)
    (params [list: leaf("a")])
    (args ("n" "lst"))
    (return
      (a-record
        (a-field "prefix" (a-app (a-id "List" (xref "lists" "List")) "a"))
        (a-field "suffix" (a-app (a-id "List" (xref "lists" "List")) "a"))))
    (contract
      (a-arrow
        (a-id "Number" (xref "<global>" "Number"))
        (a-app (a-id "List" (xref "lists" "List")) "a")
        (a-record
          (a-field "prefix" (a-app (a-id "List" (xref "lists" "List")) "a"))
          (a-field "suffix" (a-app (a-id "List" (xref "lists" "List")) "a")))))
    (doc
      "Splits the list into two lists, one containing the first n elements, and the other containing the rest"))
  (fun-spec
    (name "any")
    (arity 2)
    (params [list: leaf("a")])
    (args ("f" "lst"))
    (return (a-id "Boolean" (xref "<global>" "Boolean")))
    (contract
      (a-arrow
        (a-arrow "a" (a-id "Boolean" (xref "<global>" "Boolean")))
        (a-app (a-id "List" (xref "lists" "List")) "a")
        (a-id "Boolean" (xref "<global>" "Boolean"))))
    (doc "Returns true if f(elem) returns true for any elem of lst"))
  (fun-spec
    (name "all")
    (arity 2)
    (params [list: leaf("a")])
    (args ("f" "lst"))
    (return (a-id "Boolean" (xref "<global>" "Boolean")))
    (contract
      (a-arrow
        (a-arrow "a" (a-id "Boolean" (xref "<global>" "Boolean")))
        (a-app (a-id "List" (xref "lists" "List")) "a")
        (a-id "Boolean" (xref "<global>" "Boolean"))))
    (doc "Returns true if f(elem) returns true for all elems of lst"))
  (fun-spec
    (name "all2")
    (arity 3)
    (params [list: leaf("a"), leaf("b")])
    (args ("f" "lst1" "lst2"))
    (return (a-id "Boolean" (xref "<global>" "Boolean")))
    (contract
      (a-arrow
        (a-arrow "a" "b" (a-id "Boolean" (xref "<global>" "Boolean")))
        (a-app (a-id "List" (xref "lists" "List")) "b")
        (a-app (a-id "List" (xref "lists" "List")) "b")
        (a-id "Boolean" (xref "<global>" "Boolean"))))
    (doc
      "Returns true if f(elem1, elem2) returns true for all corresponding elems of lst1 and list2.\n        Returns true when either list is empty"))
  (fun-spec
    (name "map")
    (arity 2)
    (params [list: leaf("a"), leaf("b")])
    (args ("f" "lst"))
    (return (a-app (a-id "List" (xref "lists" "List")) "b"))
    (contract
      (a-arrow
        (a-arrow "a" "b")
        (a-app (a-id "List" (xref "lists" "List")) "a")
        (a-app (a-id "List" (xref "lists" "List")) "b")))
    (doc "Returns a list made up of f(elem) for each elem in lst"))
  (fun-spec
    (name "map2")
    (arity 3)
    (params [list: leaf("a"), leaf("b"), leaf("c")])
    (args ("f" "l1" "l2"))
    (return (a-app (a-id "List" (xref "lists" "List")) "c"))
    (contract
      (a-arrow
        (a-arrow "a" "b" "c")
        (a-app (a-id "List" (xref "lists" "List")) "a")
        (a-app (a-id "List" (xref "lists" "List")) "b")
        (a-app (a-id "List" (xref "lists" "List")) "c")))
    (doc
      "Returns a list made up of f(elem1, elem2) for each elem1 in l1, elem2 in l2"))
  (fun-spec
    (name "map3")
    (arity 4)
    (params [list: leaf("a"), leaf("b"), leaf("c"), leaf("d")])
    (args ("f" "l1" "l2" "l3"))
    (return (a-app (a-id "List" (xref "lists" "List")) "d"))
    (contract
      (a-arrow
        (a-arrow "a" "b" "c" "d")
        (a-app (a-id "List" (xref "lists" "List")) "a")
        (a-app (a-id "List" (xref "lists" "List")) "b")
        (a-app (a-id "List" (xref "lists" "List")) "c")
        (a-app (a-id "List" (xref "lists" "List")) "d")))
    (doc
      "Returns a list made up of f(e1, e2, e3) for each e1 in l1, e2 in l2, e3 in l3"))
  (fun-spec
    (name "map4")
    (arity 5)
    (params [list: leaf("a"), leaf("b"), leaf("c"), leaf("d"), leaf("e")])
    (args ("f" "l1" "l2" "l3" "l4"))
    (return (a-app (a-id "List" (xref "lists" "List")) "e"))
    (contract
      (a-arrow
        (a-arrow "a" "b" "c" "d" "e")
        (a-app (a-id "List" (xref "lists" "List")) "a")
        (a-app (a-id "List" (xref "lists" "List")) "b")
        (a-app (a-id "List" (xref "lists" "List")) "c")
        (a-app (a-id "List" (xref "lists" "List")) "d")
        (a-app (a-id "List" (xref "lists" "List")) "e")))
    (doc
      "Returns a list made up of f(e1, e2, e3, e4) for each e1 in l1, e2 in l2, e3 in l3, e4 in l4"))
  (fun-spec
    (name "map_n")
    (arity 3)
    (params [list: leaf("a"), leaf("b")])
    (args ("f" "n" "lst"))
    (return (a-app (a-id "List" (xref "lists" "List")) "b"))
    (contract
      (a-arrow
        (a-arrow (a-id "Number" (xref "<global>" "Number")) "a" "b")
        (a-id "Number" (xref "<global>" "Number"))
        (a-app (a-id "List" (xref "lists" "List")) "a")
        (a-app (a-id "List" (xref "lists" "List")) "b")))
    (doc
      "Returns a list made up of f(n, e1), f(n+1, e2) .. for e1, e2 ... in lst"))
  (fun-spec
    (name "map2_n")
    (arity 4)
    (params [list: leaf("a"), leaf("b"), leaf("c")])
    (args ("f" "n" "l1" "l2"))
    (return (a-app (a-id "List" (xref "lists" "List")) "c"))
    (contract
      (a-arrow
        (a-arrow (a-id "Number" (xref "<global>" "Number")) "a" "b" "c")
        (a-id "Number" (xref "<global>" "Number"))
        (a-app (a-id "List" (xref "lists" "List")) "a")
        (a-app (a-id "List" (xref "lists" "List")) "b")
        (a-app (a-id "List" (xref "lists" "List")) "c")))
    (doc
      "Returns a list made up of f(i, e1, e2) for each e1 in l1, e2 in l2, and i counting up from n"))
  (fun-spec
    (name "map3_n")
    (arity 5)
    (params [list: leaf("a"), leaf("b"), leaf("c"), leaf("d")])
    (args ("f" "n" "l1" "l2" "l3"))
    (return (a-app (a-id "List" (xref "lists" "List")) "d"))
    (contract
      (a-arrow
        (a-arrow (a-id "Number" (xref "<global>" "Number")) "a" "b" "c" "d")
        (a-id "Number" (xref "<global>" "Number"))
        (a-app (a-id "List" (xref "lists" "List")) "a")
        (a-app (a-id "List" (xref "lists" "List")) "b")
        (a-app (a-id "List" (xref "lists" "List")) "c")
        (a-app (a-id "List" (xref "lists" "List")) "d")))
    (doc
      "Returns a list made up of f(i, e1, e2, e3) for each e1 in l1, e2 in l2, e3 in l3, and i counting up from n"))
  (fun-spec
    (name "map4_n")
    (arity 6)
    (params [list: leaf("a"), leaf("b"), leaf("c"), leaf("d"), leaf("e")])
    (args ("f" "n" "l1" "l2" "l3" "l4"))
    (return (a-app (a-id "List" (xref "lists" "List")) "e"))
    (contract
      (a-arrow
        (a-arrow (a-id "Number" (xref "<global>" "Number")) "a" "b" "c" "d" "e")
        (a-id "Number" (xref "<global>" "Number"))
        (a-app (a-id "List" (xref "lists" "List")) "a")
        (a-app (a-id "List" (xref "lists" "List")) "b")
        (a-app (a-id "List" (xref "lists" "List")) "c")
        (a-app (a-id "List" (xref "lists" "List")) "d")
        (a-app (a-id "List" (xref "lists" "List")) "e")))
    (doc
      "Returns a list made up of f(i, e1, e2, e3, e4) for each e1 in l1, e2 in l2, e3 in l3, e4 in l4, and i counting up from n"))
  (fun-spec
    (name "each")
    (arity 2)
    (params [list: leaf("a")])
    (args ("f" "lst"))
    (return (a-id "Nothing" (xref "<global>" "Nothing")))
    (contract
      (a-arrow
        (a-arrow "a" (a-id "Nothing" (xref "<global>" "Nothing")))
        (a-app (a-id "List" (xref "lists" "List")) "a")
        (a-id "Nothing" (xref "<global>" "Nothing"))))
    (doc "Calls f for each elem in lst, and returns nothing"))
  (fun-spec
    (name "each2")
    (arity 3)
    (params [list: leaf("a"), leaf("b")])
    (args ("f" "lst1" "lst2"))
    (return (a-id "Nothing" (xref "<global>" "Nothing")))
    (contract
      (a-arrow
        (a-arrow "a" "b" (a-id "Nothing" (xref "<global>" "Nothing")))
        (a-app (a-id "List" (xref "lists" "List")) "a")
        (a-app (a-id "List" (xref "lists" "List")) "b")
        (a-id "Nothing" (xref "<global>" "Nothing"))))
    (doc
      "Calls f on each pair of corresponding elements in l1 and l2, and returns nothing.  Stops after the shortest list"))
  (fun-spec
    (name "each3")
    (arity 4)
    (params [list: leaf("a"), leaf("b"), leaf("c")])
    (args ("f" "lst1" "lst2" "lst3"))
    (return (a-id "Nothing" (xref "<global>" "Nothing")))
    (contract
      (a-arrow
        (a-arrow "a" "b" "c" (a-id "Nothing" (xref "<global>" "Nothing")))
        (a-app (a-id "List" (xref "lists" "List")) "a")
        (a-app (a-id "List" (xref "lists" "List")) "b")
        (a-app (a-id "List" (xref "lists" "List")) "c")
        (a-id "Nothing" (xref "<global>" "Nothing"))))
    (doc
      "Calls f on each triple of corresponding elements in l1, l2 and l3, and returns nothing.  Stops after the shortest list"))
  (fun-spec
    (name "each4")
    (arity 5)
    (params [list: leaf("a"), leaf("b"), leaf("c"), leaf("d")])
    (args ("f" "lst1" "lst2" "lst3" "lst4"))
    (return "Any")
    (contract
      (a-arrow
        (a-arrow "a" "b" "c" "d" (a-id "Nothing" (xref "<global>" "Nothing")))
        (a-app (a-id "List" (xref "lists" "List")) "a")
        (a-app (a-id "List" (xref "lists" "List")) "b")
        (a-app (a-id "List" (xref "lists" "List")) "c")
        (a-app (a-id "List" (xref "lists" "List")) "d")
        "Any"))
    (doc
      "Calls f on each tuple of corresponding elements in l1, l2, l3 and l4, and returns nothing.  Stops after the shortest list"))
  (fun-spec
    (name "each_n")
    (arity 3)
    (params [list: leaf("a")])
    (args ("f" "num" "lst"))
    (return (a-id "Nothing" (xref "<global>" "Nothing")))
    (contract
      (a-arrow
        (a-arrow
          (a-id "Number" (xref "<global>" "Number"))
          "a"
          (a-id "Nothing" (xref "<global>" "Nothing")))
        (a-id "Number" (xref "<global>" "Number"))
        (a-app (a-id "List" (xref "lists" "List")) "a")
        (a-id "Nothing" (xref "<global>" "Nothing"))))
    (doc
      "Calls f(i, e) for each e in lst and with i counting up from num, and returns nothing"))
  (fun-spec
    (name "each2_n")
    (arity 4)
    (params [list: leaf("a"), leaf("b")])
    (args ("f" "num" "lst1" "lst2"))
    (return (a-id "Nothing" (xref "<global>" "Nothing")))
    (contract
      (a-arrow
        (a-arrow
          (a-id "Number" (xref "<global>" "Number"))
          "a"
          "b"
          (a-id "Nothing" (xref "<global>" "Nothing")))
        (a-id "Number" (xref "<global>" "Number"))
        (a-app (a-id "List" (xref "lists" "List")) "a")
        (a-app (a-id "List" (xref "lists" "List")) "b")
        (a-id "Nothing" (xref "<global>" "Nothing"))))
    (doc
      "Calls f(i, e1, e2) for each e1 in lst1, e2 in lst2 and with i counting up from num, and returns nothing"))
  (fun-spec
    (name "each3_n")
    (arity 5)
    (params [list: leaf("a"), leaf("b"), leaf("c")])
    (args ("f" "num" "lst1" "lst2" "lst3"))
    (return (a-id "Nothing" (xref "<global>" "Nothing")))
    (contract
      (a-arrow
        (a-arrow
          (a-id "Number" (xref "<global>" "Number"))
          "a"
          "b"
          "c"
          (a-id "Nothing" (xref "<global>" "Nothing")))
        (a-id "Number" (xref "<global>" "Number"))
        (a-app (a-id "List" (xref "lists" "List")) "a")
        (a-app (a-id "List" (xref "lists" "List")) "b")
        (a-app (a-id "List" (xref "lists" "List")) "c")
        (a-id "Nothing" (xref "<global>" "Nothing"))))
    (doc
      "Calls f(i, e1, e2, e3) for each e1 in lst1, e2 in lst2, e3 in lst3 and with i counting up from num, and returns nothing"))
  (fun-spec
    (name "each4_n")
    (arity 6)
    (params [list: leaf("a"), leaf("b"), leaf("c"), leaf("d")])
    (args ("f" "num" "lst1" "lst2" "lst3" "lst4"))
    (return (a-id "Nothing" (xref "<global>" "Nothing")))
    (contract
      (a-arrow
        (a-arrow "a" "b" "c" "d" (a-id "Nothing" (xref "<global>" "Nothing")))
        (a-id "Number" (xref "<global>" "Number"))
        (a-app (a-id "List" (xref "lists" "List")) "a")
        (a-app (a-id "List" (xref "lists" "List")) "b")
        (a-app (a-id "List" (xref "lists" "List")) "c")
        (a-app (a-id "List" (xref "lists" "List")) "d")
        (a-id "Nothing" (xref "<global>" "Nothing"))))
    (doc
      "Calls f(i, e1, e2, e3, e4) for each e1 in lst1, e2 in lst2, e3 in lst3, e4 in lst4 and with i counting up from num, and returns nothing"))
  (fun-spec
    (name "fold-while")
    (arity 3)
    (params [list: leaf("a"), leaf("b")])
    (args ("f" "base" "lst"))
    (return "a")
    (contract
      (a-arrow
        (a-arrow
          "a"
          "b"
          (a-app (a-id "Either" (xref "either" "Either")) "a" "a"))
        "a"
        (a-app (a-id "List" (xref "lists" "List")) "b")
        "a"))
    (doc
      "Takes a function that takes two arguments and returns an Either, and also a base value, and folds\n        over the given list from the left as long as the function returns a left() value, and returns either\n        the final value or the right() value"))
  (fun-spec
    (name "fold")
    (arity 3)
    (params [list: leaf("a"), leaf("b")])
    (args ("f" "base" "lst"))
    (return "a")
    (contract
      (a-arrow
        (a-arrow "a" "b" "a")
        "a"
        (a-app (a-id "List" (xref "lists" "List")) "b")
        "a"))
    (doc
      "Takes a function, an initial value and a list, and folds the function over the list from the left,\n        starting with the initial value"))
  (fun-spec
    (name "foldl")
    (arity 3)
    (params [list: leaf("a"), leaf("b")])
    (args ("f" "base" "lst"))
    (return "a")
    (contract
      (a-arrow
        (a-arrow "a" "b" "a")
        "a"
        (a-app (a-id "List" (xref "lists" "List")) "b")
        "a"))
    (doc
      "Takes a function, an initial value and a list, and folds the function over the list from the left,\n        starting with the initial value"))
  (fun-spec
    (name "foldr")
    (arity 3)
    (params [list: leaf("a"), leaf("b")])
    (args ("f" "base" "lst"))
    (return "a")
    (contract
      (a-arrow
        (a-arrow "a" "b" "a")
        "a"
        (a-app (a-id "List" (xref "lists" "List")) "b")
        "a"))
    (doc
      "Takes a function, an initial value and a list, and folds the function over the list from the right,\n        starting with the initial value"))
  (fun-spec
    (name "fold2")
    (arity 4)
    (params [list: leaf("a"), leaf("b"), leaf("c")])
    (args ("f" "base" "l1" "l2"))
    (return "a")
    (contract
      (a-arrow
        (a-arrow "a" "b" "c" "a")
        "a"
        (a-app (a-id "List" (xref "lists" "List")) "b")
        (a-app (a-id "List" (xref "lists" "List")) "c")
        "a"))
    (doc
      "Takes a function, an initial value and two lists, and folds the function over the lists in parallel\n        from the left, starting with the initial value and ending when either list is empty"))
  (fun-spec
    (name "fold3")
    (arity 5)
    (params [list: leaf("a"), leaf("b"), leaf("c"), leaf("d")])
    (args ("f" "base" "l1" "l2" "l3"))
    (return "a")
    (contract
      (a-arrow
        (a-arrow "a" "b" "c" "d" "a")
        "a"
        (a-app (a-id "List" (xref "lists" "List")) "b")
        (a-app (a-id "List" (xref "lists" "List")) "c")
        (a-app (a-id "List" (xref "lists" "List")) "d")
        "a"))
    (doc
      "Takes a function, an initial value and three lists, and folds the function over the lists in parallel\n        from the left, starting with the initial value and ending when any list is empty"))
  (fun-spec
    (name "fold4")
    (arity 6)
    (params [list: leaf("a"), leaf("b"), leaf("c"), leaf("d"), leaf("e")])
    (args ("f" "base" "l1" "l2" "l3" "l4"))
    (return "a")
    (contract
      (a-arrow
        (a-arrow "a" "b" "c" "d" "e" "a")
        "a"
        (a-app (a-id "List" (xref "lists" "List")) "b")
        (a-app (a-id "List" (xref "lists" "List")) "c")
        (a-app (a-id "List" (xref "lists" "List")) "d")
        (a-app (a-id "List" (xref "lists" "List")) "e")
        "a"))
    (doc
      "Takes a function, an initial value and four lists, and folds the function over the lists in parallel\n        from the left, starting with the initial value and ending when any list is empty"))
  (fun-spec
    (name "fold_n")
    (arity 4)
    (params [list: leaf("a"), leaf("b")])
    (args ("f" "num" "base" "lst"))
    (return "a")
    (contract
      (a-arrow
        (a-arrow (a-id "Number" (xref "<global>" "Number")) "a" "b" "a")
        (a-id "Number" (xref "<global>" "Number"))
        "a"
        (a-app (a-id "List" (xref "lists" "List")) "b")
        "a"))
    (doc
      "Takes a function, an initial value and a list, and folds the function over the list from the left,\n        starting with the initial value and passing along the index (starting with the given num)"))
  (fun-spec
    (name "member-with")
    (arity 3)
    (params [list: leaf("a")])
    (args ("lst" "elt" "eq"))
    (return "Any")
    (contract
      (a-arrow
        (a-app (a-id "List" (xref "lists" "List")) "a")
        "a"
        (a-arrow
          "a"
          "a"
          (a-compound
            (a-dot "equality" "EqualityResult")
            (xref "equality" "EqualityResult")))
        "Any")))
  (fun-spec
    (name "member3")
    (arity 2)
    (params [list: leaf("a")])
    (args ("lst" "elt"))
    (return
      (a-compound
        (a-dot "equality" "EqualityResult")
        (xref "equality" "EqualityResult")))
    (contract
      (a-arrow
        (a-app (a-id "List" (xref "lists" "List")) "a")
        "a"
        (a-compound
          (a-dot "equality" "EqualityResult")
          (xref "equality" "EqualityResult")))))
  (fun-spec
    (name "member")
    (arity 2)
    (params [list: leaf("a")])
    (args ("lst" "elt"))
    (return (a-id "Boolean" (xref "<global>" "Boolean")))
    (contract
      (a-arrow
        (a-app (a-id "List" (xref "lists" "List")) "a")
        "a"
        (a-id "Boolean" (xref "<global>" "Boolean")))))
  (fun-spec
    (name "member-always3")
    (arity 2)
    (params [list: leaf("a")])
    (args ("lst" "elt"))
    (return
      (a-compound
        (a-dot "equality" "EqualityResult")
        (xref "equality" "EqualityResult")))
    (contract
      (a-arrow
        (a-app (a-id "List" (xref "lists" "List")) "a")
        "a"
        (a-compound
          (a-dot "equality" "EqualityResult")
          (xref "equality" "EqualityResult")))))
  (fun-spec
    (name "member-always")
    (arity 2)
    (params [list: leaf("a")])
    (args ("lst" "elt"))
    (return (a-id "Boolean" (xref "<global>" "Boolean")))
    (contract
      (a-arrow
        (a-app (a-id "List" (xref "lists" "List")) "a")
        "a"
        (a-id "Boolean" (xref "<global>" "Boolean")))))
  (fun-spec
    (name "member-now3")
    (arity 2)
    (params [list: leaf("a")])
    (args ("lst" "elt"))
    (return
      (a-compound
        (a-dot "equality" "EqualityResult")
        (xref "equality" "EqualityResult")))
    (contract
      (a-arrow
        (a-app (a-id "List" (xref "lists" "List")) "a")
        "a"
        (a-compound
          (a-dot "equality" "EqualityResult")
          (xref "equality" "EqualityResult")))))
  (fun-spec
    (name "member-now")
    (arity 2)
    (params [list: leaf("a")])
    (args ("lst" "elt"))
    (return (a-id "Boolean" (xref "<global>" "Boolean")))
    (contract
      (a-arrow
        (a-app (a-id "List" (xref "lists" "List")) "a")
        "a"
        (a-id "Boolean" (xref "<global>" "Boolean")))))
  (fun-spec
    (name "member-identical3")
    (arity 2)
    (params [list: leaf("a")])
    (args ("lst" "elt"))
    (return
      (a-compound
        (a-dot "equality" "EqualityResult")
        (xref "equality" "EqualityResult")))
    (contract
      (a-arrow
        (a-app (a-id "List" (xref "lists" "List")) "a")
        "a"
        (a-compound
          (a-dot "equality" "EqualityResult")
          (xref "equality" "EqualityResult")))))
  (fun-spec
    (name "member-identical")
    (arity 2)
    (params [list: leaf("a")])
    (args ("lst" "elt"))
    (return (a-id "Boolean" (xref "<global>" "Boolean")))
    (contract
      (a-arrow
        (a-app (a-id "List" (xref "lists" "List")) "a")
        "a"
        (a-id "Boolean" (xref "<global>" "Boolean")))))
  (fun-spec
    (name "shuffle")
    (arity 1)
    (params [list: leaf("a")])
    (args ("lst"))
    (return (a-app (a-id "List" (xref "lists" "List")) "a"))
    (contract
      (a-arrow
        (a-app (a-id "List" (xref "lists" "List")) "a")
        (a-app (a-id "List" (xref "lists" "List")) "a"))))
  (fun-spec
    (name "index")
    (arity 2)
    (params [list: leaf("a")])
    (args ("lst" "n"))
    (return "a")
    (contract
      (a-arrow
        (a-app (a-id "List" (xref "lists" "List")) "a")
        (a-id "Number" (xref "<global>" "Number"))
        "a"))
    (doc
      "Returns the nth element of the given list, or raises an error if n is out of range"))
  (fun-spec
    (name "get-help")
    (arity 2)
    (params [list: leaf("a")])
    (args ("lst" "n"))
    (return "a")
    (contract
      (a-arrow
        (a-app (a-id "List" (xref "lists" "List")) "a")
        (a-id "Number" (xref "<global>" "Number"))
        "a"))
    (doc
      "Returns the nth element of the given list, or raises an error if n is out of range"))
  (fun-spec
    (name "set-help")
    (arity 3)
    (params [list: leaf("a")])
    (args ("lst" "n" "v"))
    (return "a")
    (contract
      (a-arrow
        (a-app (a-id "List" (xref "lists" "List")) "a")
        (a-id "Number" (xref "<global>" "Number"))
        "Any"
        "a"))
    (doc
      "Returns a new list with the same values as the given list but with the nth element\n        set to the given value, or raises an error if n is out of range"))
  (unknown-item
    (name "list")
    ;; { make: lam(arr499): raw-array-to-list(arr499) end }
    )))

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
  }

  @function["is-empty" #:alt-docstrings ""]

  @function["is-link" #:alt-docstrings ""]

  }

@section{The @pyret{list} Constructor}

@collection-doc["list" #:contract `(a-arrow ("elt" "a") ,(L-of "a"))]

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

@examples{
check:
  one-four = link(1, link(2, link(3, link(4, empty))))

  one-four.split-at(0) is { prefix: empty, suffix: one-four }
  one-four.split-at(4) is { prefix: one-four, suffix: empty }
  one-four.split-at(2) is { prefix: link(1, link(2, empty)), suffix: link(3, link(4, empty)) }
  one-four.split-at(-1) raises "Invalid index"
  one-four.split-at(5) raises "Index too large"
end
}

@list-method["take"]

@examples{
check:
  [list: 1, 2, 3, 4, 5, 6].take(3) is [list: 1, 2, 3]
end
}

@list-method["drop"]

@examples{
check:
  [list: 1, 2, 3, 4, 5, 6].drop(3) is [list: 4, 5, 6]
end
}

@list-method["get"]

@examples{
check:
  [list: 1, 2, 3].get(0) is 1
  [list: ].get(0) raises "too large"
end
}

@list-method["set"]

@examples{
check:
  [list: 1, 2, 3].set(0, 5) is [list: 5, 2, 3]
  [list: ].set(0, 5) raises "too large"
end
}

@list-method["foldl"]

Applies @pyret{f(last-elt, f(second-last-elt, ... f(first-elt, base)))}.  For
@pyret-id{empty}, returns @pyret{base}.

@examples{
check:
  [list: 3, 2, 1].foldl(link, empty) is [list: 1, 2, 3]
end
}

@list-method["foldr"]

Applies @pyret{f(first-elt, f(second-elt, ... f(last-elt, base)))}.  For
@pyret-id{empty}, returns @pyret{base}.

@examples{
check:
  [list: 3, 2, 1].foldr(link, empty) is [list: 3, 2, 1]
end
}

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
    "get"
    #:examples
    '@{
    check:
      lists.get([list: 1, 2, 3], 0) is 1
      lists.get([list: ], 0) raises ""
    end
    }
  ]
  @function[
    "set"
    #:examples
    '@{
    check:
      set([list: 1, 2, 3], 0, 5) is [list: 5, 2, 3]
      set([list: 1, 2, 3], 5, 5) raises ""
    end
    }
  ]
  @;{@function[
    "reverse"
    #:examples
    '@{
    check:
      reverse([list: ], [list: ]) is [list: ]
      reverse([list: 1, 3], [list: ]) is [list: 3, 1]
    end
    }
  ]}
  @function[
    "range"
    #:examples
    '@{
    check:
      range(0, 0) is [list: ]
      range(0, 1) is [list: 0]
      range(-5, 5) is [list: -5, -4, -3, -2, -1, 0, 1, 2, 3, 4]
    end
    }
  ]
  @function[
    "repeat"
    #:examples
    '@{
    check:
      repeat(0, 10) is empty
      repeat(3, -1) is [list: -1, -1, -1]
      repeat(1, "foo") is link("foo", empty)
    end
    }
  ]
  @function[
    "filter"
    #:examples
    '@{
    check:
      filter(lam(e): e > 5 end, [list: -1, 1]) is [list: ]
      filter(lam(e): e > 0 end, [list: -1, 1]) is [list: 1]
    end
    }
  ]
  @function[
    "partition"
    #:examples
    '@{
    check:
      partition(lam(e): e > 0 end, [list: -1, 1]) is
        { is-true: [list: 1], is-false: [list: -1] }
      partition(lam(e): e > 5 end, [list: -1, 1]) is
        { is-true: [list: ], is-false: [list: -1, 1] }
      partition(lam(e): e < 5 end, [list: -1, 1]) is
        { is-true: [list: -1, 1], is-false: [list: ] }
    end
    }
  ]
  @function[
    "find"
    #:examples
    '@{
    check:
      find(lam(elt): elt > 1 end, [list: 1, 2, 3]) is some(2)
      find(lam(elt): elt > 4 end, [list: 1, 2, 3]) is none
      find(lam(elt): true end, [list: "find-me", "miss-me"]) is some("find-me")
      find(lam(elt): true end, empty) is none
      find(lam(elt): false end, [list: "miss-me"]) is none
      find(lam(elt): false end, empty) is none
    end
    }
    #:alt-docstrings '()
  ]
  @function[
    "split-at"
    #:examples
    '@{
    check:
      let one-four = [list: 1, 2, 3, 4]:
        split-at(0, one-four) is { prefix: empty, suffix: one-four }
        split-at(4, one-four) is { prefix: one-four, suffix: empty }
        split-at(2, one-four) is
          { prefix: [list: 1, 2], suffix: [list: 3, 4] }
        split-at(-1, one-four) raises "Invalid index"
        split-at(5, one-four) raises "Index too large"
      end
    end
    }
  ]
  @function[
    "any"
    #:examples
    '@{
    check:
      any(lam(n): n > 1 end, [list: 1, 2, 3]) is true
      any(lam(n): n > 3 end, [list: 1, 2, 3]) is false
      any(lam(x): true end, empty) is false
      any(lam(x): false end, empty) is false
    end
    }
  ]
  @function[
    "all"
    #:examples
    '@{
    check:
      all(lam(n): n > 1 end, [list: 1, 2, 3]) is false
      all(lam(n): n <= 3 end, [list: 1, 2, 3]) is true
      all(lam(x): true end, empty) is true
      all(lam(x): false end, empty) is true
    end
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
      map(lam(_): 2 end, [list: 1, 2, 3, 4]) is [list: 2, 2, 2, 2]
      map(lam(x): x + 1 end, [list: 1, 2, 3, 4]) is [list: 2, 3, 4, 5]
    }
  ]
  @function[
    "map2"
    #:examples
    '@{
      map2(lam(x, y): x or y end, [list: true, false], [list: false, false]) is
        [list: true, false]
    }
  ]
  @function["map3"]
  @function["map4"]
  @function["map_n"]

  Like map, but also includes a numeric argument for the position in the list
  that is currently being mapped over.

  @examples{
  check:
    map_n(lam(n, e): n end, 0, [list: "captain", "first mate"]) is [list: 0, 1]
  end
  }

  @function["map2_n"]

  Like @pyret-id{map_n}, but for two-argument functions.

  @function["map3_n"]
  @function["map4_n"]

  @function[
    "each"
    #:examples
    '@{
    check:
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
    end
    }
  ]

  @function["each2"]
  @function["each3"]
  @function["each4"]

  @function["each_n"]

  Like @pyret-id{each}, but also includes a numeric argument for the position in the list
  that is currently being visited.

  @function["each2_n"]
  @function["each3_n"]
  @function["each4_n"]
  @function["fold-while"]
  @function[
    "fold"
    #:examples
    '@{
    check:
      fold(lam(acc, cur): acc end, 1, [list: 1, 2, 3, 4]) is 1
      fold(lam(acc, cur): cur end, 1, [list: 1, 2, 3, 4]) is 4
      fold(lam(acc, cur): acc + cur end, 0, [list: 1, 2, 3, 4]) is 10
      fold(lam(lst, elt): link(elt, lst) end, empty, [list: 1, 2, 3]) is [list: 3, 2, 1]
    end
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
  @function["foldl"]
  Another name for @pyret-id["fold"].
  @function["foldr"]
  Like @pyret-id["foldl"], but right-associative:
@examples{
check:
  foldr(lam(acc, cur): acc + cur end, 0, [list: 1, 2, 3, 4]) is 10
  foldr(lam(lst, elt): link(elt, lst) end, empty, [list: 1, 2, 3]) is [list: 1, 2, 3]
end
}

  @function["fold2"]
  @function["fold3"]
  @function["fold4"]
  @function[
    "fold_n"
    #:examples
    '@{
    check:
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
    end
    }
  ]{

  Like @pyret-id{fold}, but takes a numeric argument for the position in the
  list that is currently being visited.

  }

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
  @function[
    "reverse"
  ]


  @function[
    "shuffle"
  ]

  Returns a new list with all the elements of the original list in random
  order.

@examples{
check "shuffle":
  l = [list: 1, 2, 3, 4]                                                                         
  l-mixed = lists.shuffle(l)
  sets.list-to-set(l-mixed) is sets.list-to-set(l)                                               
  l-mixed.length() is l.length()  
end
}

}

