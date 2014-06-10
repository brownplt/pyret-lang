#lang scribble/base
@(require "../../../scribble-api.rkt")
@docmodule["\"compiler/js-of-pyret.arr\""]{
  @; Ignored type testers
  @ignore[(list "is-ccp")]
  @section[#:tag "\"compiler/js-of-pyret.arr\"_DataTypes"]{Data types}
  @data-spec["CompiledCodePrinter"]{
    @variants{
      @constr-spec["ccp"]{
        @members{@member-spec["compiled"]}
        @with-members{
          @method-spec[
            "pyret-to-js-standalone"
            ;; N.B. Pyret contract: (CompiledCodePrinter -> String)
            
          ]
          @method-spec[
            "pyret-to-js-pretty"
            ;; N.B. Pyret contract: (CompiledCodePrinter -> String)
            
          ]
          @method-spec[
            "pyret-to-js-runnable"
            ;; N.B. Pyret contract: (CompiledCodePrinter -> String)
            
          ]
          @method-spec[
            "print-js-runnable"
            ;; N.B. Pyret contract: (CompiledCodePrinter, Any -> Any)
            
          ]
        }
      }
    }
    @shared{}
  }
  
  @section[#:tag "\"compiler/js-of-pyret.arr\"_Functions"]{Functions}
  @function["make-compiled-pyret"]
  @function["make-unsafe-compiled-pyret"]
  @function["trace-make-compiled-pyret"]
}