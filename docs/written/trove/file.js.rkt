#lang scribble/base
@(require "../../scribble-api.rkt")
@docmodule["file"]{
  @section[#:tag "file_ReExports"]{Re-exported values}
  @section[#:tag "file_DataTypes"]{Data types}
  @section[#:tag "file_Functions"]{Functions}
  @function[
    "input-file"
    #:contract (a-arrow (a-id "String" (xref "<global>" "String")) "Any")
  ]
  @function[
    "output-file"
    #:contract
    (a-arrow
      (a-id "String" (xref "<global>" "String"))
      (a-id "Bool" (xref "<global>" "Bool"))
      "Any")
  ]
  @function[
    "file-exists"
    #:contract (a-arrow (a-id "String" (xref "<global>" "String")) "Any")
  ]
  @function["file-to-string" #:contract (a-arrow "Any" "Any")]
}