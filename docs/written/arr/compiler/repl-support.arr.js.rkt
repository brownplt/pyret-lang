#lang scribble/base
@(require "../../../scribble-api.rkt")
@docmodule["\"compiler/repl-support.arr\""]{
  @; Unknown: PLEASE DOCUMENT
  @ignore[(list "bad-imports")]
  @section[#:tag "\"compiler/repl-support.arr\"_Functions"]{Functions}
  @function[
    "drop-module-bindings"
    #:examples
    '@{
      @; drop-module-bindings(C.compile-env([list: 
      @;       C.builtin-id("x"),
      @;       C.module-bindings("list", [list: ])
      @;     ],
      @;     [list: C.type-id("Number"), C.type-module-bindings("lists", [list: "List"])])) is
      @;   C.compile-env([list: C.builtin-id("x")], [list: C.type-id("Number")])
      
    }
  ]
  @function["add-global-binding"]
  @function["add-global-type-binding"]
  @function["make-safe-imports"]
  @function["make-provide-for-repl"]
  @function["make-provide-for-repl-main"]
}