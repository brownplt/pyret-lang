#lang scribble/base
@(require "../../scribble-api.rkt")
@docmodule["either"]{
  @section[#:tag "either_ReExports"]{Re-exported values}
  @section[#:tag "either_DataTypes"]{Data types}
  @data-spec["Either" #:params (list "a" "b")]{
    @variants{
      @constr-spec["left"]{
        @members{@member-spec["v" #:contract (a-id "a" (xref "<global>" "a"))]}
        @with-members{}
      }
      @constr-spec["right"]{
        @members{@member-spec["v" #:contract (a-id "b" (xref "<global>" "b"))]}
        @with-members{}
      }
    }
    @shared{}
  }
  @section[#:tag "either_Functions"]{Functions}
  @function[
    "is-left"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-right"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
}