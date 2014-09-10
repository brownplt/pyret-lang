#lang scribble/base
@(require "../../scribble-api.rkt" "../abbrevs.rkt")

@(define s-pick-args (list `("elt" ("type" "normal") ("contract" ,(a-id "a"))) `("rest" ("type" "normal") ("contract" ,(a-id "b")))))

@docmodule["pick"]{
  @; Ignored type testers
  @section{The Pick Datatype}

  @data-spec2["Pick" (list "a" "b") (list
    @singleton-spec2["Pick" "pick-none"]
    @constructor-spec["Pick" "pick-some" s-pick-args]
  )]

  @nested[#:style 'inset]{
  @singleton-doc["Pick" "pick-none" (P-of "a" "b")]
  @constructor-doc["Pick" "pick-some" s-pick-args (P-of "a" "b")]

  @function["is-pick-none" #:alt-docstrings ""]
  @function["is-pick-some" #:alt-docstrings ""]
  }
  
  }
