#lang scribble/base
@(require "../../scribble-api.rkt" "../abbrevs.rkt")

@(define s-some-args (list `("value" ("type" "normal") ("contract" ,(a-id "a")))))

@(append-gen-docs
'(module
  "option"
  (path "src/arr/base/option.arr")
  (data-spec
    (name "Option")
    (type-vars (a16))
    (variants ("none" "some"))
    (shared ()))
  
  (singleton-spec
    (name "none")
    (with-members
      ((method-spec
        (name "or-else")
        (arity 2)
        (params ())
        (args ("self" "v"))
        (return "a")
        (contract
          (a-arrow (a-id "is-Option" (xref "option" "is-Option")) "a" "a"))
        (doc "Return the default provided value"))
      (method-spec
        (name "and-then")
        (arity 2)
        (params ("b"))
        (args ("self" "$underscore"))
        (return (a-app (a-id "Option" (xref "option" "Option")) "b"))
        (contract
          (a-arrow
            (a-id "is-Option" (xref "option" "is-Option"))
            (a-arrow "a" "b")
            (a-app (a-id "Option" (xref "option" "Option")) "b")))
        (doc "Return none")))))
  (fun-spec
    (name "is-none")
    (arity 1)
    (params [list: ])
    (args ("val"))
    (return (a-id "Boolean" (xref "<global>" "Boolean")))
    (contract (a-arrow "Any" (a-id "Boolean" (xref "<global>" "Boolean"))))
    (doc "Checks whether the provided argument is in fact a none"))
  (constr-spec
    (name "some")
    (members (("value" (type normal) (contract "Any"))))
    (with-members
      ((method-spec
        (name "or-else")
        (arity 2)
        (params ())
        (args ("self" "v"))
        (return "a")
        (contract
          (a-arrow (a-id "is-Option" (xref "option" "is-Option")) "a" "a"))
        (doc "Return self.value, rather than the default"))
      (method-spec
        (name "and-then")
        (arity 2)
        (params ("b"))
        (args ("self" "f"))
        (return (a-app (a-id "Option" (xref "option" "Option")) "b"))
        (contract
          (a-arrow
            (a-id "is-Option" (xref "option" "is-Option"))
            (a-arrow "a" "b")
            (a-app (a-id "Option" (xref "option" "Option")) "b")))
        (doc "Returns the function applied to self.value")))))
  (fun-spec
    (name "is-some")
    (arity 1)
    (params [list: ])
    (args ("val"))
    (return (a-id "Boolean" (xref "<global>" "Boolean")))
    (contract (a-arrow "Any" (a-id "Boolean" (xref "<global>" "Boolean"))))
    (doc "Checks whether the provided argument is in fact a some"))))

@docmodule["option"]{
  @; Ignored type testers
  @section{The Option Datatype}

  @data-spec2["Option" (list "a") (list
    @singleton-spec2["Option" "none"]
    @constructor-spec["Option" "some" s-some-args]
  )]

  @nested[#:style 'inset]{
  @singleton-doc["Option" "none" (O-of "a")]
  @constructor-doc["Option" "some" s-some-args (O-of "a")]

  @function["is-none" #:alt-docstrings ""]
  @function["is-some" #:alt-docstrings ""]
  }

@section{Option Methods}
  @method-doc["Option" "some" "and-then" #:alt-docstrings ""]

  For @pyret-id{none}, returns @pyret-id{none}.  For @pyret-id{some}, applies
  @pyret{f} to the @pyret{value} field and returns a new @pyret-id{some} with the
  updated value.

@examples{
check:
  add1 = lam(n): n + 1 end
  n = none
  n.and-then(add1) is none

  s = some(5)
  s.and-then(add1) is some(6)
end
}

  @method-doc["Option" "some" "or-else" #:alt-docstrings ""]

  For @pyret-id{none}, returns @pyret{v}.  For @pyret-id{some}, returns the
  @pyret{value} field.  Useful for providing default values.

@examples{
check:
  n = none
  n.or-else(42) is 42

  s = some(5)
  s.or-else(10) is 5
end
}
  
  }
