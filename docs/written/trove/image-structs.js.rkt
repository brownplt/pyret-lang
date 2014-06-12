#lang scribble/base
@(require "../../scribble-api.rkt")
@docmodule["image-structs"]{
  @; Ignored type testers
  @ignore[(list "is-color")]
  @section[#:tag "image-structs_DataTypes"]{Data types}
  @data-spec["Color"]{
    @variants{
      @constr-spec["color"]{
        @members{
          @member-spec["red"]
          @member-spec["green"]
          @member-spec["blue"]
          @member-spec["alpha"]
        }
        @with-members{}
      }
    }
    @shared{}
  }
}