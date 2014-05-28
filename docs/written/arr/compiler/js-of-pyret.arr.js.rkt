#lang scribble/base
@(require "../../../scribble-api.rkt")
@docmodule["\"compiler/js-of-pyret.arr\""]{
  @section[#:tag "\"compiler/js-of-pyret.arr\"_ReExports"]{Re-exported values}
  @section[#:tag "\"compiler/js-of-pyret.arr\"_DataTypes"]{Data types}
  @data-spec["CompiledCodePrinter"]{
    @variants{
      @constr-spec["ccp"]{
        @members{@member-spec["compiled" #:contract (a-dot "J" "JExpr")]}
        @with-members{
          @method-spec[
            "pyret-to-js-standalone"
            #:contract
            (a-arrow
              (a-id
                "CompiledCodePrinter"
                (xref "\"compiler/js-of-pyret.arr\"" "CompiledCodePrinter"))
              (a-id "String" (xref "<global>" "String")))
          ]
          @method-spec[
            "pyret-to-js-pretty"
            #:contract
            (a-arrow
              (a-id
                "CompiledCodePrinter"
                (xref "\"compiler/js-of-pyret.arr\"" "CompiledCodePrinter"))
              (a-id "String" (xref "<global>" "String")))
          ]
          @method-spec[
            "pyret-to-js-runnable"
            #:contract
            (a-arrow
              (a-id
                "CompiledCodePrinter"
                (xref "\"compiler/js-of-pyret.arr\"" "CompiledCodePrinter"))
              (a-id "String" (xref "<global>" "String")))
          ]
          @method-spec[
            "print-js-runnable"
            #:contract
            (a-arrow
              (a-id
                "CompiledCodePrinter"
                (xref "\"compiler/js-of-pyret.arr\"" "CompiledCodePrinter"))
              "Any"
              "Any")
          ]
        }
      }
    }
    @shared{}
  }
  @section[#:tag "\"compiler/js-of-pyret.arr\"_Functions"]{Functions}
  @function[
    "is-ccp"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "make-compiled-pyret"
    #:contract
    (a-arrow
      "Any"
      "Any"
      (a-id
        "CompiledCodePrinter"
        (xref "\"compiler/js-of-pyret.arr\"" "CompiledCodePrinter")))
  ]
  @function[
    "make-unsafe-compiled-pyret"
    #:contract
    (a-arrow
      "Any"
      "Any"
      (a-id
        "CompiledCodePrinter"
        (xref "\"compiler/js-of-pyret.arr\"" "CompiledCodePrinter")))
  ]
  @function[
    "trace-make-compiled-pyret"
    #:contract (a-arrow "Any" "Any" "Any" "Any" "Any")
  ]
}