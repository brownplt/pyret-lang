#lang scribble/base
@(require "../../scribble-api.rkt" "../abbrevs.rkt")
@(require (only-in scribble/core delayed-block))

@(define (SD-of typ) (a-app (a-id "StringDict" (xref "string-dict" "StringDict")) typ))
@(define (MSD-of typ) (a-app (a-id "MutableStringDict" (xref "string-dict" "MutableStringDict")) typ))

@(define (sd-method name #:args args #:return ret #:contract contract)
  (method-doc "StringDict" "string-dict" name #:alt-docstrings "" #:args args #:return ret #:contract contract))
@(define (msd-method name)
  (method-doc "MutableStringDict" "string-dict" name #:alt-docstrings ""))

@(append-gen-docs
  `(module "string-dict"
    (path "src/js/base/runtime-anf.js")
    (fun-spec (name "make-mutable-string-dict"))
    (fun-spec (name "make-string-dict"))
    (fun-spec (name "string-dict"))
    (fun-spec (name "mutable-string-dict"))
    (data-spec
      (name "StringDict")
      (type-vars (a-id "a"))
      (variants ("string-dict"))
      (shared (
        (method-spec (name "set"))
        (method-spec (name "get"))
        (method-spec (name "get-value"))
        (method-spec (name "remove"))
        (method-spec (name "keys"))
        (method-spec (name "has-key"))
        (method-spec (name "count")))))
    (data-spec
      (name "MutableStringDict")
      (type-vars (a-id "a"))
      (variants ("mutable-string-dict"))
      (shared (
        (method-spec (name "set-now"))
        (method-spec (name "get-now"))
        (method-spec (name "get-value-now"))
        (method-spec (name "remove-now"))
        (method-spec (name "keys-now"))
        (method-spec (name "has-key-now"))
        (method-spec (name "count-now"))
        (method-spec (name "freeze"))
        (method-spec (name "seal")))))
  ))

@docmodule["string-dict"]{

@section{The StringDict Type}

@type-spec["StringDict" (list "a")]

There are no variants for @pyret-id{StringDict}s, and programs cannot use
@pyret{cases} statements with @pyret-id{StringDict}s.  Instead, they can be
created with the constructors below, and manipulated with the methods and
functions below.

@pyret-id{StringDict}s keep track of a mapping from @pyret-id["String"
"strings"]s to any Pyret value.

@collection-doc["string-dict" (list (cons "key" "String") (cons "elt" "a")) (SD-of "a")]

Creates a string dicts with the given @pyret{elt}s.

@sd-method["get"
  #:contract (a-arrow (SD-of "a") S (O-of "a"))
  #:args (list (list "self" #f) (list "key" #f))
  #:return (O-of "a")
]

Returns @pyret-id["none" "option"] if the key is not in the dictionary, and a
@pyret-id["some" "option"] containing the value the key maps to is present:

@examples{
check:
  [string-dict: "a", 5].get("a") is some(5)
  [string-dict: "a", 5].get("b") is none
end
}

@sd-method["get-value"
  #:contract (a-arrow (SD-of "a") S "a")
  #:args (list (list "self" #f) (list "key" #f))
  #:return "a"
]

Returns the value that @pyret{key} maps to if it is present, and throws an
exception otherwise.:

@examples{
check:
  [string-dict: "a", 5].get-value("a") is 5
  [string-dict: "a", 5].get-value("b") raises "Not found"
end
}

@section{The MutableStringDict Type}

@type-spec["MutableStringDict" (list "a")]

@collection-doc["mutable-string-dict" (list (cons "elt" "a")) (SD-of "a")]

}
