#lang scribble/base
@(require "../../scribble-api.rkt")
@docmodule["option"]{
  @; Ignored type testers
  @ignore[(list "is-none" "is-some")]
  @section[#:tag "option_DataTypes"]{Data types}
  @data-spec["Option"]{
    @variants{
      @singleton-spec["none"]{
        @with-members{
          @method-spec[
            "or-else"
            ;; N.B. Pyret contract: (Option, Any -> Any)
            
          ]
          @method-spec[
            "and-then"
            ;; N.B. Pyret contract: (Option, Any -> Any)
            
          ]
        }
      }
      @constr-spec["some"]{
        @members{@member-spec["value"]}
        @with-members{
          @method-spec[
            "or-else"
            ;; N.B. Pyret contract: (Option, Any -> Any)
            
          ]
          @method-spec[
            "and-then"
            ;; N.B. Pyret contract: (Option, Any -> Any)
            
          ]
        }
      }
    }
    @shared{}
  }
}