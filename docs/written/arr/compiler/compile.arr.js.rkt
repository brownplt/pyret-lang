#lang scribble/base
@(require "../../../scribble-api.rkt")
@docmodule["\"compiler/compile.arr\""]{
  @section[#:tag "\"compiler/compile.arr\"_ReExports"]{Re-exported values}
  @section[#:tag "\"compiler/compile.arr\"_DataTypes"]{Data types}
  @data-spec["CompilationPhase"]{
    @variants{
      @singleton-spec["start"]{@with-members{}}
      @constr-spec["phase"]{
        @members{
          @member-spec[
            "name"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec["result" #:contract "Any"]
          @member-spec[
            "prev"
            #:contract
            (a-id
              "CompilationPhase"
              (xref "\"compiler/compile.arr\"" "CompilationPhase"))
          ]
        }
        @with-members{}
      }
    }
    @shared{
      @method-spec[
        "tolist"
        #:contract
        (a-arrow
          (a-id
            "CompilationPhase"
            (xref "\"compiler/compile.arr\"" "CompilationPhase"))
          "Any")
      ]
    }
  }
  @section[#:tag "\"compiler/compile.arr\"_Functions"]{Functions}
  @function[
    "is-start"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-phase"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "compile-js-ast"
    #:contract
    (a-arrow
      "Any"
      "Any"
      "Any"
      "Any"
      "Any"
      (a-id
        "CompilationPhase"
        (xref "\"compiler/compile.arr\"" "CompilationPhase")))
  ]
  @function[
    "compile-js"
    #:contract
    (a-arrow
      "Any"
      "Any"
      "Any"
      "Any"
      "Any"
      "Any"
      (a-app
        (a-id
          "CompilationPhase"
          (xref "\"compiler/compile.arr\"" "CompilationPhase"))
        (a-app
          (a-compound
            (a-dot "C" "CompileResult")
            (xref "\"compiler/compile-structs.arr\"" "CompileResult"))
          (a-compound
            (a-dot "P" "CompiledCodePrinter")
            (xref "\"compiler/js-of-pyret.arr\"" "CompiledCodePrinter"))
          "Any")))
  ]
  @function[
    "compile-runnable-js"
    #:contract
    (a-arrow
      "Any"
      "Any"
      "Any"
      "Any"
      "Any"
      (a-app
        (a-compound
          (a-dot "C" "CompileResult")
          (xref "\"compiler/compile-structs.arr\"" "CompileResult"))
        (a-compound
          (a-dot "P" "CompiledCodePrinter")
          (xref "\"compiler/js-of-pyret.arr\"" "CompiledCodePrinter"))
        "Any"))
  ]
  @function[
    "compile-runnable-js-file"
    #:contract
    (a-arrow
      "Any"
      "Any"
      "Any"
      "Any"
      (a-app
        (a-compound
          (a-dot "C" "CompileResult")
          (xref "\"compiler/compile-structs.arr\"" "CompileResult"))
        (a-compound
          (a-dot "P" "CompiledCodePrinter")
          (xref "\"compiler/js-of-pyret.arr\"" "CompiledCodePrinter"))
        "Any"))
  ]
  @function[
    "compile-standalone-js-file"
    #:contract
    (a-arrow
      "Any"
      "Any"
      "Any"
      "Any"
      (a-app
        (a-compound
          (a-dot "C" "CompileResult")
          (xref "\"compiler/compile-structs.arr\"" "CompileResult"))
        (a-compound
          (a-dot "P" "CompiledCodePrinter")
          (xref "\"compiler/js-of-pyret.arr\"" "CompiledCodePrinter"))
        "Any"))
  ]
  @function[
    "compile-standalone-js"
    #:contract
    (a-arrow
      "Any"
      "Any"
      "Any"
      "Any"
      (a-app
        (a-compound
          (a-dot "C" "CompileResult")
          (xref "\"compiler/compile-structs.arr\"" "CompileResult"))
        (a-id "String" (xref "<global>" "String"))
        "Any"))
  ]
}