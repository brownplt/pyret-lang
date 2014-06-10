#lang scribble/base
@(require "../../../scribble-api.rkt")
@docmodule["\"compiler/compile.arr\""]{
  @; Ignored type testers
  @ignore[(list "is-start" "is-phase")]
  @section[#:tag "\"compiler/compile.arr\"_DataTypes"]{Data types}
  @data-spec["CompilationPhase"]{
    @variants{
      @singleton-spec["start"]{@with-members{}}
      @constr-spec["phase"]{
        @members{
          @member-spec["name"]
          @member-spec["result"]
          @member-spec["prev"]
        }
        @with-members{}
      }
    }
    @shared{
      @method-spec[
        "tolist"
        ;; N.B. Pyret contract: (CompilationPhase -> Any)
        
      ]
    }
  }
  
  @section[#:tag "\"compiler/compile.arr\"_Functions"]{Functions}
  @function["compile-js-ast"]
  @function["compile-js"]
  @function["compile-runnable-js"]
  @function["compile-runnable-js-file"]
  @function["compile-standalone-js-file"]
  @function["compile-standalone-js"]
}