#lang scribble/base
@(require "../../../scribble-api.rkt")
@docmodule["\"compiler/desugar-check.arr\""]{
  @; Ignored type testers
  @ignore[(list "is-check-info")]
  @; Unknown: PLEASE DOCUMENT
  @ignore[(list "check-stmts-visitor" "no-checks-visitor" "check-visitor")]
  @section[#:tag "\"compiler/desugar-check.arr\"_DataTypes"]{Data types}
  @data-spec["CheckInfo"]{
    @variants{
      @constr-spec["check-info"]{
        @members{@member-spec["l"] @member-spec["name"] @member-spec["body"]}
        @with-members{}
      }
    }
    @shared{}
  }
  
  @section[#:tag "\"compiler/desugar-check.arr\"_Functions"]{Functions}
  @function["get-checks"]
  @function["create-check-block"]
  @function["make-lam"]
  @function["desugar-check"]
  @function["desugar-no-checks"]
}