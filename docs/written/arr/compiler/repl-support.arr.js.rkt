#lang scribble/base
@(require "../../../scribble-api.rkt")
@docmodule["\"compiler/repl-support.arr\""]{
  @section[#:tag "\"compiler/repl-support.arr\"_ReExports"]{Re-exported values}
  @section[#:tag "\"compiler/repl-support.arr\"_DataTypes"]{Data types}
  @section[#:tag "\"compiler/repl-support.arr\"_Functions"]{Functions}
  @function[
    "add-global-binding"
    #:contract
    (a-arrow
      (a-compound
        (a-dot "C" "CompileEnvironment")
        (xref "\"compiler/compile-structs.arr\"" "CompileEnvironment"))
      (a-id "String" (xref "<global>" "String"))
      "Any")
  ]
  @function[
    "make-provide-all"
    #:contract
    (a-arrow (a-compound (a-dot "A" "Program") (xref "ast" "Program")) "Any")
  ]
}