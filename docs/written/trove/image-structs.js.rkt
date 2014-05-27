#lang scribble/base
@(require "../../scribble-api.rkt")
@docmodule["image-structs"]{
  @section[#:tag "image-structs_ReExports"]{Re-exported values}
  @section[#:tag "image-structs_DataTypes"]{Data types}
  @data-spec["Color"]{
    @variants{
      @constr-spec["color"]{
        @members{
          @member-spec[
            "red"
            #:contract (a-id "Number" (xref "<global>" "Number"))
          ]
          @member-spec[
            "green"
            #:contract (a-id "Number" (xref "<global>" "Number"))
          ]
          @member-spec[
            "blue"
            #:contract (a-id "Number" (xref "<global>" "Number"))
          ]
          @member-spec[
            "alpha"
            #:contract (a-id "Number" (xref "<global>" "Number"))
          ]
        }
        @with-members{}
      }
    }
    @shared{}
  }
  @section[#:tag "image-structs_Functions"]{Functions}
  @function[
    "is-color"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
}