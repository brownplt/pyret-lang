#lang scribble/base
@(require "../../scribble-api.rkt" "../abbrevs.rkt")
@(require (only-in scribble/core delayed-block))

@(define (SD-of typ) (a-app (a-id "StringDict" (xref "string-dict" "StringDict")) typ))
@(define (MSD-of typ) (a-app (a-id "MutableStringDict" (xref "string-dict" "MutableStringDict")) typ))

@(define (sd-method name #:args args #:return ret #:contract contract)
  (method-doc "StringDict" "string-dict" name #:alt-docstrings "" #:args args #:return ret #:contract contract))
@(define (msd-method name #:args args #:return ret #:contract contract)
  (method-doc "MutableStringDict" "string-dict" name #:alt-docstrings "" #:args args #:return ret #:contract contract))

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
        (method-spec (name "count"))
        (method-spec (name "unfreeze"))
        )))
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

@section{StringDict Constructor}

@collection-doc["string-dict" (list (cons "key" "String") (cons "elt" "a")) (SD-of "a")]

Creates a string-dict with the given @pyret{elt}s.

@section{StringDict Methods}

@sd-method["get"
  #:contract (a-arrow (SD-of "a") S (O-of "a"))
  #:args (list (list "self" #f) (list "key" #f))
  #:return (O-of "a")
]

Returns @pyret-id["none" "option"] if the key is not in the dictionary, and a
@pyret-id["some" "option"] containing the value the key maps to
if the key is in the dictionary.

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
exception otherwise.

@examples{
check:
  [string-dict: "a", 5].get-value("a") is 5
  [string-dict: "a", 5].get-value("b") raises "Not found"
end
}

@sd-method["set"
  #:contract (a-arrow (SD-of "a") S "a" (SD-of "a"))
  #:args (list (list "self" #f) (list "key" #f) (list "value" #f))
  #:return (SD-of "a")
]

Returns a new string-dict that maps @pyret{key} to @pyret{value}
and is otherwise similar to the original string-dict.

@examples{
check:
  sd1 = [string-dict: "a", 5]
  sd2 = sd1.set("b", 10)
  sd2.get("b") is 10
  sd2.get("a") is 5
end
}

@sd-method["has-key"
  #:contract (a-arrow (SD-of "a") S B)
  #:args (list (list "self" #f) (list "key" #f))
  #:return B
]

@sd-method["keys"
  #:contract (a-arrow (SD-of "a") (S-of S))
  #:args (list (list "self" #f))
  #:return (S-of S)
]

@sd-method["remove"
  #:contract (a-arrow (SD-of "a") S (SD-of "a"))
  #:args (list (list "self" #f) (list "key" #f))
  #:return (SD-of "a")
]

@sd-method["count"
  #:contract (a-arrow (SD-of "a") N)
  #:args (list (list "self" #f))
  #:return N
]

@sd-method["unfreeze"
  #:contract (a-arrow (SD-of "a") (MSD-of "a"))
  #:args (list (list "self" #f))
  #:return (MSD-of "a")
]

/* equality */

@section{The MutableStringDict Type}

@type-spec["MutableStringDict" (list "a")]

@section{MutableStringDict Constructor}

@collection-doc["mutable-string-dict" (list (cons "elt" "a")) (SD-of "a")]

@section{MutableStringDict Methods}

@msd-method["get-now"
  #:contract (a-arrow (MSD-of "a") S (O-of "a"))
  #:args (list (list "self" #f) (list "key" #f))
  #:return (O-of "a")
]

@msd-method["get-value-now"
  #:contract (a-arrow (MSD-of "a") S "a")
  #:args (list (list "self" #f) (list "key" #f))
  #:return "a"
]

@msd-method["set-now"
  #:contract (a-arrow (MSD-of "a") S "a" No)
  #:args (list (list "self" #f) (list "key" #f) (list "value" #f))
  #:return No
]

@msd-method["has-key-now"
  #:contract (a-arrow (MSD-of "a") S B)
  #:args (list (list "self" #f) (list "key" #f))
  #:return B
]

@msd-method["keys-now"
  #:contract (a-arrow (MSD-of "a") (S-of S))
  #:args (list (list "self" #f))
  #:return (S-of S)
]

@msd-method["remove-now"
  #:contract (a-arrow (MSD-of "a") S No)
  #:args (list (list "self" #f) (list "key" #f))
  #:return No
]

@msd-method["count-now"
  #:contract (a-arrow (MSD-of "a") N)
  #:args (list (list "self" #f))
  #:return N
]

@msd-method["freeze"
  #:contract (a-arrow (MSD-of "a") (SD-of "a"))
  #:args (list (list "self" #f))
  #:return (SD-of "a")
]

@msd-method["seal"
  #:contract (a-arrow (MSD-of "a") (MSD-of "a"))
  #:args (list (list "self" #f))
  #:return (MSD-of "a")
]

/* equality */

}
