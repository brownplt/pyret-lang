#lang scribble/base
@(require "../../scribble-api.rkt" "../abbrevs.rkt")

@(define left-args (list `("v" ("type" "normal") ("contract" ,(a-id "a")))))
@(define right-args (list `("v" ("type" "normal") ("contract" ,(a-id "b")))))

@(append-gen-docs
  '(module
  "either"
  (path "src/arr/base/either.arr")
  (data-spec
    (name "Either")
    (type-vars (a16 b17))
    (variants ("left" "right"))
    (shared ()))
  
  (constr-spec
    (name "left")
    (members (("v" (type normal) (contract "a"))))
    (with-members ()))
  (fun-spec
    (name "is-left")
    (arity 1)
    (params [list: ])
    (args ("val"))
    (return (a-id "Boolean" (xref "<global>" "Boolean")))
    (contract (a-arrow "Any" (a-id "Boolean" (xref "<global>" "Boolean"))))
    (doc "Checks whether the provided argument is in fact a left"))
  (constr-spec
    (name "right")
    (members (("v" (type normal) (contract "b"))))
    (with-members ()))
  (fun-spec
    (name "is-right")
    (arity 1)
    (params [list: ])
    (args ("val"))
    (return (a-id "Boolean" (xref "<global>" "Boolean")))
    (contract (a-arrow "Any" (a-id "Boolean" (xref "<global>" "Boolean"))))
    (doc "Checks whether the provided argument is in fact a right"))))

@docmodule["either"]{
  @; Ignored type testers
  @section[#:tag "either_DataTypes"]{Data types}
  @data-spec2["Either" (list "a" "b") (list
    @constructor-spec["Either" "left" left-args]
    @constructor-spec["Either" "right" right-args]
  )]

  @nested[#:style 'inset]{
  @constructor-doc["Either" "left" left-args (E-of "a" "b")]
  @constructor-doc["Either" "right" right-args (E-of "a" "b")]

  @function["is-left" #:alt-docstrings ""]
  @function["is-right" #:alt-docstrings ""]
  }
}
