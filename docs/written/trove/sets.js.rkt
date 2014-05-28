#lang scribble/base
@(require "../../scribble-api.rkt")
@docmodule["sets"]{
  @; Unknown: PLEASE DOCUMENT
  @ignore[(list "list-set" "tree-set" "empty-list-set" "empty-tree-set")]
  @section[#:tag "sets_ReExports"]{Re-exported values}
  @section[#:tag "sets_DataTypes"]{Data types}
  @data-spec["Set"]{
    @variants{
      @constr-spec["list-set"]{
        @members{
          @member-spec["elems" #:contract (a-id "List" (xref "lists" "List"))]
        }
        @with-members{
          @method-spec[
            "member"
            #:contract
            (a-arrow
              (a-id "Set" (xref "sets" "Set"))
              "Any"
              (a-id "Bool" (xref "<global>" "Bool")))
          ]
          @method-spec[
            "add"
            #:contract
            (a-arrow
              (a-id "Set" (xref "sets" "Set"))
              "Any"
              (a-id "Set" (xref "sets" "Set")))
          ]
          @method-spec[
            "remove"
            #:contract
            (a-arrow
              (a-id "Set" (xref "sets" "Set"))
              "Any"
              (a-id "Set" (xref "sets" "Set")))
          ]
          @method-spec[
            "to-list"
            #:contract
            (a-arrow
              (a-id "Set" (xref "sets" "Set"))
              (a-id "List" (xref "lists" "List")))
          ]
          @method-spec[
            "union"
            #:contract
            (a-arrow
              (a-id "Set" (xref "sets" "Set"))
              (a-id "Set" (xref "sets" "Set"))
              (a-id "Set" (xref "sets" "Set")))
          ]
          @method-spec[
            "intersect"
            #:contract
            (a-arrow
              (a-id "Set" (xref "sets" "Set"))
              (a-id "Set" (xref "sets" "Set"))
              (a-id "Set" (xref "sets" "Set")))
          ]
          @method-spec[
            "difference"
            #:contract
            (a-arrow
              (a-id "Set" (xref "sets" "Set"))
              (a-id "Set" (xref "sets" "Set"))
              (a-id "Set" (xref "sets" "Set")))
          ]
        }
      }
      @constr-spec["tree-set"]{
        @members{
          @member-spec[
            "elems"
            #:contract (a-id "AVLTree" (xref "sets" "AVLTree"))
          ]
        }
        @with-members{
          @method-spec[
            "member"
            #:contract
            (a-arrow
              (a-id "Set" (xref "sets" "Set"))
              "Any"
              (a-id "Bool" (xref "<global>" "Bool")))
          ]
          @method-spec[
            "add"
            #:contract
            (a-arrow
              (a-id "Set" (xref "sets" "Set"))
              "Any"
              (a-id "Set" (xref "sets" "Set")))
          ]
          @method-spec[
            "remove"
            #:contract
            (a-arrow
              (a-id "Set" (xref "sets" "Set"))
              "Any"
              (a-id "Set" (xref "sets" "Set")))
          ]
          @method-spec[
            "to-list"
            #:contract
            (a-arrow
              (a-id "Set" (xref "sets" "Set"))
              (a-id "List" (xref "lists" "List")))
          ]
          @method-spec[
            "union"
            #:contract (a-arrow (a-id "Set" (xref "sets" "Set")) "Any" "Any")
          ]
          @method-spec[
            "intersect"
            #:contract (a-arrow (a-id "Set" (xref "sets" "Set")) "Any" "Any")
          ]
          @method-spec[
            "difference"
            #:contract
            (a-arrow
              (a-id "Set" (xref "sets" "Set"))
              (a-id "Set" (xref "sets" "Set"))
              (a-id "Set" (xref "sets" "Set")))
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "symmetric_difference"
        #:contract
        (a-arrow
          (a-id "Set" (xref "sets" "Set"))
          (a-id "Set" (xref "sets" "Set"))
          (a-id "Set" (xref "sets" "Set")))
      ]
      @method-spec[
        "_equals"
        #:contract (a-arrow (a-id "Set" (xref "sets" "Set")) "Any" "Any")
      ]
    }
  }
  @section[#:tag "sets_Functions"]{Functions}
  @function[
    "set"
    #:contract
    (a-arrow
      (a-id "List" (xref "lists" "List"))
      (a-id "Set" (xref "sets" "Set")))
  ]
}