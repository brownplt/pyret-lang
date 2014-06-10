#lang scribble/base
@(require "../../scribble-api.rkt")
@docmodule["either"]{
  @; Ignored type testers
  @ignore[(list "is-left" "is-right")]
  @section[#:tag "either_DataTypes"]{Data types}
  @data-spec["Either" #:params (list "a" "b")]{
    @variants{
      @constr-spec["left"]{@members{@member-spec["v"]} @with-members{}}
      @constr-spec["right"]{@members{@member-spec["v"]} @with-members{}}
    }
    @shared{}
  }
}