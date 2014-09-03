#lang scribble/base
@(require "../../scribble-api.rkt" "../abbrevs.rkt")

@(append-gen-docs
  '(module "booleans"
    (path "src/js/base/runtime-anf.js")
    (data-spec
      (name "Boolean")
      (variants)
      (shared))
    (fun-spec
      (name "not")
      (arity 1)
      (args ("b"))
      (return (a-id "Boolean" (xref "<global>" "Boolean")))
      (doc "Negates the boolean value"))))

@docmodule["booleans" #:noimport #t #:friendly-title "Boolean"]{
  @type-spec["Boolean" (list)]

  The type of the values @pyret{true} and @pyret{false}.

  @section{Boolean Functions}

  @function["not" #:contract (a-arrow B B) #:return B #:alt-docstrings ""]

  Returns @pyret{true} when given @pyret{false} and vice versa.

}

