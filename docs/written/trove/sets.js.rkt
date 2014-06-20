#lang scribble/base
@(require "../../scribble-api.rkt")

@(define (no-descs args)
   (map (lambda (iden) `(,iden "")) args))

@docmodule["sets"]{
  @; Unknown: PLEASE DOCUMENT
  @ignore[(list "list-set" "tree-set" "empty-list-set" "empty-tree-set")]
  @section[#:tag "sets_ReExports"]{Re-exported values}
  @section[#:tag "sets_DataTypes"]{Data types}
  @data-spec["Set"]{
    @variants{}
    @shared{
      @method-spec[
       "tostring"
        #:args (no-descs '("self"))
        #:contract (a-arrow (a-id "Set" (xref "sets" "Set")) "Any")
      ]
      @method-spec[
        "_torepr"
        #:args (no-descs '("self"))
        #:contract (a-arrow (a-id "Set" (xref "sets" "Set")) "Any")
      ]
      @method-spec[
        "member"
        #:args (no-descs '("self" "elem"))
        #:contract
        (a-arrow
          (a-id "Set" (xref "sets" "Set"))
          "Any"
          (a-id "Bool" (xref "<global>" "Bool")))
      ]
      @method-spec[
        "add"
        #:args (no-descs '("self" "elem"))
        #:contract
        (a-arrow
          (a-id "Set" (xref "sets" "Set"))
          "Any"
          (a-id "Set" (xref "sets" "Set")))
      ]
      @method-spec[
        "remove"
        #:args (no-descs '("self" "elem"))
        #:contract
        (a-arrow
          (a-id "Set" (xref "sets" "Set"))
          "Any"
          (a-id "Set" (xref "sets" "Set")))
      ]
      @method-spec[
        "to-list"
        #:args (no-descs '("self"))
        #:contract
        (a-arrow
          (a-id "Set" (xref "sets" "Set"))
          (a-id "List" (xref "lists" "List")))
      ]
      @method-spec[
        "union"
        #:args (no-descs '("self" "other"))
        #:contract
        (a-arrow
          (a-id "Set" (xref "sets" "Set"))
          (a-id "Set" (xref "sets" "Set"))
          (a-id "Set" (xref "sets" "Set")))
      ]{
        @; In addition to doc string
        The union of two sets is a set containing all elements that are contained in
        at least one of the two original sets.

        @;image{http://upload.wikimedia.org/wikipedia/commons/3/30/Venn0111.svg}
      }
      @method-spec[
        "intersect"
        #:args (no-descs '("self" "other"))
        #:contract
        (a-arrow
          (a-id "Set" (xref "sets" "Set"))
          (a-id "Set" (xref "sets" "Set"))
          (a-id "Set" (xref "sets" "Set")))
      ]{
        @; In addition to doc string
        The intersection of two sets is a set containing all elements that are contained in
        both of the two original sets.

        @;image{http://upload.wikimedia.org/wikipedia/commons/9/99/Venn0001.svg}
      }
      @method-spec[
        "difference"
        #:args (no-descs '("self" "other"))
        #:contract
        (a-arrow
          (a-id "Set" (xref "sets" "Set"))
          (a-id "Set" (xref "sets" "Set"))
          (a-id "Set" (xref "sets" "Set")))
      ]{
        @; In addition to doc string
        The difference of two sets is a set containing all elements that are contained in
        the first set, but not contained in the second set.

        @;image{http://upload.wikimedia.org/wikipedia/commons/e/e6/Venn0100.svg}
      }
      @method-spec[
        "symmetric_difference"
        #:args (no-descs '("self" "other"))
        #:contract
        (a-arrow
          (a-id "Set" (xref "sets" "Set"))
          (a-id "Set" (xref "sets" "Set"))
          (a-id "Set" (xref "sets" "Set")))
      ]{
        @; In addition to doc string
        The symmetric difference of two sets is a set containing all elements that are
        contained one of the two sets, but not contained in both sets.

        @;image{http://upload.wikimedia.org/wikipedia/commons/4/46/Venn0110.svg}
      }
      @method-spec[
        "_equals"
        #:contract (a-arrow (a-id "Set" (xref "sets" "Set")) "Any" "Any")
      ]
    }
  }
  @section[#:tag "sets_Functions"]{Functions}
  @function["list-to-list-set"]
  @function["list-to-tree-set"]
}
