
#lang scribble/base
@(require "../../scribble-api.rkt" "../abbrevs.rkt")

@(define ra-of-a '(a-app (a-id "RawArray" (xref "raw-arrays" "RawArray")) "a"))

@(append-gen-docs
  `(module "raw-arrays"
    (path "src/js/base/runtime-anf.js")
    (data-spec
      (name "RawArray")
      (variants)
      (shared))
    (fun-spec
      (name "raw-array-of")
      (arity 2)
      (args ("value" "count"))
      (return ,ra-of-a)
      (doc ""))
    (fun-spec
      (name "raw-array-get")
      (arity 2)
      (args ("array" "index"))
      (doc ""))
    (fun-spec
      (name "raw-array-set")
      (arity 3)
      (args ("array" "index" "new-value"))
      (doc ""))
    (fun-spec
      (name "raw-array-length")
      (arity 1)
      (args ("array"))
      (doc ""))
    (fun-spec
      (name "raw-array-to-list")
      (arity 1)
      (args ("array"))
      (doc ""))
    (fun-spec
      (name "raw-array-fold")
      (arity 4)
      (args ("f" "init" "array" "start-index"))
      (doc ""))
))


@docmodule["raw-arrays" #:noimport #t #:friendly-title "RawArray"]{
   @type-spec["RawArray" (list "a")]

   The type of raw array values.  Raw arrays are a primitive datastructure
   that allows for lookup and (mutable) update by non-negative integers.

   Raw arrays are used in the interface to constructor functions like
   @pyret{[constr: e1, e2, ...]}, as the way of bundling up the values from
   @pyret{e1}, @pyret{e2}, etc. to pass them to @pyret{constr.make}.

     @section{RawArray Functions}

@collection-doc["raw-array" (list (cons "elt" "a")) (RA-of "a")]

Creates a @pyret-id{RawArray} with the given elements.  Note that
@pyret-id{RawArray}s are mutable, so comparisons using @pyret["=="]
(the operator for @pyret-id["equal-always" "equality"]) will only
return @pyret{true} on @pyret-id{RawArray}s when they are also
@pyret-id["identical" "equality"], regardless of their contents.  To compare
the elements, use @pyret-id["equal-now" "equality"]/@pyret["=~"],
and test with @pyret-id["is=~" "testing"].

@examples{
check:
  [raw-array: 1, 2, 3] is-not== [raw-array: 1, 2, 3]
  [raw-array: 1, 2, 3] is-not [raw-array: 1, 2, 3]
  [raw-array: 1, 2, 3] is=~ [raw-array: 1, 2, 3]

  a = [raw-array: 1, 2, 3]
  a is a
  a is== a
end
}

  @function["raw-array-of" #:contract (a-arrow "a" N (RA-of "a"))]

Creates a @pyret-id{RawArray} with length equal to @pyret{count}, and with each
index holding @pyret{value}.  Note that @pyret{value} is not @emph{copied}, so,
the elements of arrays created with @pyret-id{raw-array-of} will always be
@pyret-id["identical" "equality"] (with the usual caveats if the @pyret{value}
was a function or method).

@examples{
check:
  a1 = raw-array-of("init", 3)
  raw-array-length(a1) is 3
  a1 is=~ [raw-array: "init", "init", "init"]

  a2 = raw-array-of({}, 3)
  raw-array-length(a2) is 3
  raw-array-get(a2, 0) is<=> raw-array-get(a2, 1)
  raw-array-get(a2, 1) is<=> raw-array-get(a2, 2)
end
}

  @function["raw-array-get" #:contract (a-arrow (RA-of "a") N "a")]
  @function["raw-array-set" #:contract (a-arrow (RA-of "a") N "a" (RA-of "a"))]
  @function["raw-array-length" #:contract (a-arrow (RA-of "a") N)]

@examples{
  a1 = raw-array-of(0, 3)
  raw-array-length(a1) is 3

  raw-array-set(a1, 0, 1)
  raw-array-set(a1, 1, 2)
  raw-array-set(a1, 2, 3)

  raw-array-get(a1, 0) is 1
  raw-array-get(a1, 1) is 2
  raw-array-get(a1, 2) is 3
  raw-array-get(a1, 3) raises "too large"
}
  @function["raw-array-to-list" #:contract (a-arrow (RA-of "a") (L-of "a"))]

    Converts a @pyret-id{RawArray} to a @pyret-id["List" "lists"] containing
    the same elements in the same order.

    Note that it doesn @emph{not} recursively convert @pyret-id{RawArray}s;
    only the top-level is converted.

    @examples{
check:
  a = [raw-array: 1, 2, 3]
  raw-array-to-list(a) is [list: 1, 2, 3]
        
  a2 = raw-array-of([raw-array:], 3)
  raw-array-to-list(a2) is=~ [list: [raw-array:], [raw-array:], [raw-array:]]
  raw-array-to-list(a2) is-not=~ [list: [list:], [list:], [list:]]
end
    }

  @function["raw-array-fold" #:contract (a-arrow (a-arrow "b" "a" N) "b" (RA-of "a") N "b")]

  Combines the elements in the array with a function that accumulates each
  element with an intermediate result.  Similar to @pyret-id["fold_n" "lists"].
  Has an argument order that works with @pyret{for}.  The numeric argument to
  the accumulator is the index of the current element.

  @examples{
check:
  a = [raw-array: "a", "b", "c"]
  str = for raw-array-fold(str from "", elt from a, i from 0):
    if i < (raw-array-length(a) - 1):
      str + elt + ": " + tostring(i) + ", "
    else:
      str + elt + ": " + tostring(i)
    end
  end
  str is "a: 0, b: 1, c: 2"
end
  }

}
