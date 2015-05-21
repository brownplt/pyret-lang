#lang scribble/base
@(require "../../scribble-api.rkt" "../abbrevs.rkt")

@(define s-some-args (list `("value" ("type" "normal") ("contract" ,(a-id "a")))))

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
