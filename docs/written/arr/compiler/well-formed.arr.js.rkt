#lang scribble/base
@(require "../../../scribble-api.rkt")
@docmodule["\"compiler/well-formed.arr\""]{
  @section[#:tag "\"compiler/well-formed.arr\"_ReExports"]{Re-exported values}
  @section[#:tag "\"compiler/well-formed.arr\"_DataTypes"]{Data types}
  @section[#:tag "\"compiler/well-formed.arr\"_Functions"]{Functions}
  @function[
    "check-well-formed"
    #:contract
    (a-arrow
      "Any"
      (a-app
        (a-compound
          (a-dot "C" "CompileResult")
          (xref "\"compiler/compile-structs.arr\"" "CompileResult"))
        (a-compound (a-dot "A" "Program") (xref "ast" "Program"))
        "Any"))
  ]
}