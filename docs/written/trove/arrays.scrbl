#lang scribble/base
@(require "../../scribble-api.rkt" "../abbrevs.rkt")

@(define a-of-a '(a-app (a-id "Array" (xref "arrays" "Array")) "a"))

@(define (a-method name #:args args #:return ret #:contract contract)
  (method-doc "Array" "array" name #:alt-docstrings "" #:args args #:return ret #:contract contract))
@(define (a-ref name)
  (pyret-method "Array" name "arrays"))

@(append-gen-docs
  '(module "arrays"
    (path "src/js/base/runtime-anf.js")
    (fun-spec
      (name "array")
      (arity 1))
    (data-spec
      (name "Array")
      (type-vars (a-id "a"))
      (variants ("array"))
      (shared (
        (method-spec (name "get-now"))
        (method-spec (name "set-now"))
        (method-spec (name "length"))
        (method-spec (name "to-list-now"))
        )))
    (fun-spec
      (name "build-array")
      (arity 2)
      (params [list: leaf("a")])
      (args ("f" "len"))
      (return "Any")
      (contract
        (a-arrow
          (a-arrow (a-id "Number" (xref "<global>" "Number")) "a")
          (a-id "Number" (xref "<global>" "Number"))
          "Any")))
    (fun-spec
      (name "array-from-list")
      (arity 1)
      (params [list: ])
      (args ("l"))
      (return "Any")
      (contract (a-arrow "Any" "Any")))
    (fun-spec
      (name "array-of")
      (arity 2)
      (args ("value" "count"))
      (doc ""))
    (fun-spec
      (name "array-get-now")
      (arity 2)
      (args ("array" "index"))
      (doc ""))
    (fun-spec
      (name "array-set-now")
      (arity 3)
      (args ("array" "index" "new-value"))
      (doc ""))
    (fun-spec
      (name "array-length")
      (arity 1)
      (args ("array"))
      (doc ""))
    (fun-spec
      (name "array-to-list-now")
      (arity 1)
      (args ("array"))
      (doc ""))
))


@docmodule["arrays" #:noimport #t]{
   @type-spec["Array" (list)]

   The type of Array values.  Arrays are a mutable, fixed-length collection
   indexed by non-negative integers.

@section{Array Constructor}

@collection-doc["array" #:contract `(a-arrow ("elt" "a") ,(A-of "a"))]

Creates an @pyret-id{Array} with the given @pyret{elt}s.

@examples{
  a :: Array<String> = [array: "a", "b"]
}

@section{Other Ways to Build Arrays}

@function["array-of"
  #:contract (a-arrow "a" N (A-of "a"))
  #:args (list (list "elt" #f) (list "count" #f))
  #:return (A-of "a")
]

Constructs an array of length @pyret{count}, where every element is the value
given as @pyret{elt}.  Note the use of @pyret-id["is=~" "testing"] below, which
is necessary because arrays are mutable and only compare their contents when
using @pyret-id["equal-now" "equality"].

@examples{
check:
  a = array-of(true, 5)
  a is=~ [array: true, true, true, true, true]
end
}

Note that the value is @emph{not} copied, so if you construct an array of
arrays with @pyret-id["array-of"], all the elements will share updates.

@examples{
check:
  a1 = [array: "a1"]
  a2 = array-of(a1, 3)
  a2 is=~ [array: [array: "a1"], [array: "a1"], [array: "a1"]]

  a1.set-now(0, "b1")
  a2 is=~ [array: [array: "b1"], [array: "b1"], [array: "b1"]]
end
}

To create an array of arrays where each array is new and independent, use
@pyret-id{build-array}.

@function["build-array"
  #:contract (a-arrow (a-arrow N "a") N (A-of "a"))
  #:args (list (list "f" #f) (list "count" #f))
  #:return (A-of "a")
]

Takes a function (@pyret{f}) that creates a new element when given a number,
and a number to count up to (@pyret{count}), and calls @pyret{f} on each number
from @pyret{0} to @pyret{count - 1}, creating an array out of the results.

@examples{
check:
  fun build(n :: Number) -> Array<String>:
    array-of("_", 3)
  end
  a = build-array(build, 3)
  
  a is=~ [array:
    [array: "_", "_", "_"],
    [array: "_", "_", "_"],
    [array: "_", "_", "_"]]

  a.get-now(0).set-now(0, "X")
  a.get-now(1).set-now(1, "O")

  a is=~ [array:
    [array: "X", "_", "_"],
    [array: "_", "O", "_"],
    [array: "_", "_", "_"]]
end
}

@section{Array Methods}

@a-method["get-now"
  #:contract (a-arrow (A-of "a") N "a")
  #:args (list (list "self" #f) (list "index" #f))
  #:return "a"
]

Returns the value at the given @pyret{index}.  If the array is too large, is
negative, or isn't a whole number, an error is signaled.  This method has a
@pyret{-now} suffix because its answer can change from one call to the next if,
for example, @a-ref["set-now"] is used.

@examples{
check:
  a = [array: "a", "b", "c"]
  a.get-now(0) is "a"
  a.get-now(1) is "b"
  a.get-now(2) is "c"
  a.get-now(3) raises "index too large"
end
}

@a-method["to-list-now"
  #:contract (a-arrow (A-of "a") (L-of "a"))
  #:args (list (list "self" #f))
  #:return (L-of "a")
]

Returns a @pyret-id["List" "lists"] containing the same elements as this array
in the same order.  This method has a
@pyret{-now} suffix because its answer can change from one call to the next if,
for example, @a-ref["set-now"] is used.

@examples{
check:
  a = [array: "a", "b", "c"]
  a.to-list-now() is [list: "a", "b", "c"]
end
}


@a-method["set-now"
  #:contract (a-arrow (A-of "a") N "a" No)
  #:args (list (list "self" #f) (list "index" #f) (list "value" #f))
  #:return No
]

Updates the value at the given @pyret{index}, returning @pyret-id["Nothing"
"<global>"].  The update is stateful, so all references to the array see the
update.  This also justifies the @pyret{-now} suffix; in the example below
calling @pyret{a.get-now()} and @pyret{a.to-list-now()} at two different points
in the program produces two different results.

@examples{
check:
  a = [array: "a", "b", "c"]

  a.get-now(0) is "a"
  a.to-list-now() is [list: "a", "b", "c"]

  b = a

  a.set-now(0, "d")

  a.get-now(0) is "d"
  a.to-list-now() is [list: "d", "b", "c"]

  b.get-now(0) is "d"
end
}

@a-method["length"
  #:contract (a-arrow (A-of "a") N)
  #:args (list (list "self" #f))
  #:return N
]

Returns the length of the array.  The length of an array is set when it is
created and cannot be changed.

@examples{
check:
  a = [array: "a", "b"]
  a.length() is 2
  
  b = [array:]
  b.length() is 0
end
}


@section{Array Functions}

@function["array-get-now"
  #:contract (a-arrow (A-of "a") N "a")
  #:args (list (list "array" #f) (list "index" #f))
  #:return "a"
]

Equivalent to @pyret{array}@a-ref["get-now"]@pyret{(index)} (function examples below).

@function["array-set-now"
  #:contract (a-arrow (A-of "a") N "a" (A-of "a"))
  #:args (list (list "array" #f) (list "index" #f) (list "value" #f))
  #:return No
]

Equivalent to @pyret{array}@a-ref["set-now"]@pyret{(index, value)} (function examples below).

@function["array-to-list-now"
  #:contract (a-arrow (A-of "a") (L-of "a"))
  #:args (list (list "array" #f))
  #:return (L-of "a")
]

Equivalent to @pyret{array}@a-ref["to-list-now"]@pyret{()} (function examples below).

@function["array-length"
  #:contract (a-arrow (A-of "a") N)
  #:args (list (list "array" #f))
  #:return N
]

Equivalent to @pyret{array}@a-ref["length"]@pyret{()} (function examples below).

@examples{
check:
  a = array-of("a", 3)
  a is=~ [array: "a", "a", "a"]

  array-set-now(a, 1, "b")
  a is=~ [array: "a", "b", "a"]

  array-get-now(a, 1) is "b"

  array-length(a) is 3
  l = array-to-list-now(a)
  l is [list: "a", "b", "a"]

  # Updating doesn't change the old to-list value
  array-set-now(a, 2, "c")
  l is [list: "a", "b", "a"]
  l2 = array-to-list-now(a)
  l2 is [list: "a", "b", "c"]
end
}

}
