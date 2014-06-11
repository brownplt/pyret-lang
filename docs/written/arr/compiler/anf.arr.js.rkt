#lang scribble/base
@(require "../../../scribble-api.rkt")
@docmodule["\"compiler/anf.arr\""]{
  @; Ignored type testers
  @ignore[(list "is-k-cont" "is-k-id")]
  @section[#:tag "\"compiler/anf.arr\"_ReExports"]{Re-exported values}
  @re-export["names" (from (xref "ast" "global-names"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["ast" "global-names"]}
  }
  @section[#:tag "\"compiler/anf.arr\"_DataTypes"]{Data types}
  @data-spec["ANFCont"]{
    @variants{
      @constr-spec["k-cont"]{
        @members{@member-spec["k"]}
        @with-members{
          @method-spec[
            "apply"
            ;; N.B. Pyret contract: (ANFCont, Loc68, N6.ALettable -> Any)
            
          ]
        }
      }
      @constr-spec["k-id"]{
        @members{@member-spec["name"]}
        @with-members{
          @method-spec[
            "apply"
            ;; N.B. Pyret contract: (ANFCont, Loc68, N6.ALettable -> Any)
            
          ]
        }
      }
    }
    @shared{}
  }
  
  @section[#:tag "\"compiler/anf.arr\"_Functions"]{Functions}
  @function["mk-id"]
  @function["anf-term"]
  @function["bind"]
  @function["anf-bind"]
  @function["anf-name"]
  @function["anf-name-rec"]
  @function["anf-program"]
  @function["anf-import"]
  @function["anf-block"]
  @function["anf"]
}