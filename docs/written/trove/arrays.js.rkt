#lang scribble/base
@(require "../../scribble-api.rkt")
@docmodule["arrays"]{
  @; Unknown: PLEASE DOCUMENT
  @ignore[(list "array" "is-array")]
  @section[#:tag "arrays_ReExports"]{Re-exported values}
  @section[#:tag "arrays_DataTypes"]{Data types}
  @section[#:tag "arrays_Functions"]{Functions}
  @function[
    "build-array"
    #:params (list "a")
    #:contract
    (a-arrow
      (a-arrow (a-id "Number" (xref "<global>" "Number")) "a")
      (a-id "Number" (xref "<global>" "Number"))
      "Any")
  ]
  @function["array-from-list" #:contract (a-arrow "Any" "Any")]
}