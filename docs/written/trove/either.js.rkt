#lang scribble/base
@(require "../../scribble-api.rkt" "../abbrevs.rkt")

@(define left-args (list `("v" ("type" "normal") ("contract" ,(a-id "a")))))
@(define right-args (list `("v" ("type" "normal") ("contract" ,(a-id "b")))))

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
