#lang scribble/base
@(require "../../scribble-api.rkt")
@docmodule["option"]{
  @section[#:tag "option_ReExports"]{Re-exported values}
  @section[#:tag "option_DataTypes"]{Data types}
  @data-spec["Option"]{
    @variants{
      @singleton-spec["none"]{
        @with-members{
          @method-spec[
            "or-else"
            #:contract
            (a-arrow (a-id "Option" (xref "option" "Option")) "Any" "Any")
          ]
          @method-spec[
            "and-then"
            #:contract
            (a-arrow (a-id "Option" (xref "option" "Option")) "Any" "Any")
          ]
        }
      }
      @constr-spec["some"]{
        @members{@member-spec["value" #:contract "Any"]}
        @with-members{
          @method-spec[
            "or-else"
            #:contract
            (a-arrow (a-id "Option" (xref "option" "Option")) "Any" "Any")
          ]
          @method-spec[
            "and-then"
            #:contract
            (a-arrow (a-id "Option" (xref "option" "Option")) "Any" "Any")
          ]
        }
      }
    }
    @shared{}
  }
  @section[#:tag "option_Functions"]{Functions}
  @function[
    "is-none"
    #:contract (a-arrow "Any" (a-id "Boolean" (xref "<global>" "Boolean")))
  ]
  @function[
    "is-some"
    #:contract (a-arrow "Any" (a-id "Boolean" (xref "<global>" "Boolean")))
  ]
}
