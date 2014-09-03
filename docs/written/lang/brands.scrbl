#lang scribble/base
@(require "../../scribble-api.rkt" "../abbrevs.rkt")

@(let ()
  (curr-module-name "brands"))

@(append-gen-docs
  `(module "brands"
    (path "src/js/base/runtime-anf.js")
    (data-spec
      (name "Brand")
      (type-vars (a-id "a"))
      (variants ("brands"))
      (shared
        ((method-spec
          (name "brand")
          (arity 2)
          (params [list: leaf("b")])
          (args ("self" "arg"))
          (return (a-id "a"))
        )
        (method-spec
          (name "test")
          (arity 2)
          (params [list: leaf("a")])
          (args ("self" "arg"))
          (return (a-id "Boolean" (xref "<global>" "Boolean")))
        ))))
    (fun-spec
      (name "brander")
      (arity 0)
      (args ())
      (return (a-app (a-id "Brand" (xref "brands" "Brand")) "a"))
      (doc ""))
        ))

@(define BR (a-id (a-id "Brand" (xref "<global>" "Brand"))))

@title{Brands}

Brands are a mostly internal language concept, useful for implementing custom datatypes.

@type-spec["Brand" (list "a")]

@function["brander" #:contract (a-arrow (a-app BR (list "a")))]

Creates a new brand.

@method-doc["Brand" "brand" "brand" #:alt-docstrings "" #:contract (a-arrow BR A "a")]

Produce a copy of the value with this brand.

@method-doc["Brand" "brand" "test" #:alt-docstrings "" #:contract (a-arrow BR A B)]

Test if the value has this brand.

