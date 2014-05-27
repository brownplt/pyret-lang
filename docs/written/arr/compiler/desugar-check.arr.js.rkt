#lang scribble/base
@(require "../../../scribble-api.rkt")
@docmodule["\"compiler/desugar-check.arr\""]{
  @; Unknown: PLEASE DOCUMENT
  @ignore[(list "check-stmts-visitor" "no-checks-visitor" "check-visitor")]
  @section[#:tag "\"compiler/desugar-check.arr\"_ReExports"]{Re-exported values}
  @section[#:tag "\"compiler/desugar-check.arr\"_DataTypes"]{Data types}
  @data-spec["CheckInfo"]{
    @variants{
      @constr-spec["check-info"]{
        @members{
          @member-spec[
            "l"
            #:contract
            (a-compound (a-dot "SL" "Srcloc") (xref "srcloc" "Srcloc"))
          ]
          @member-spec[
            "name"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec[
            "body"
            #:contract (a-compound (a-dot "A" "Expr") (xref "ast" "Expr"))
          ]
        }
        @with-members{}
      }
    }
    @shared{}
  }
  @section[#:tag "\"compiler/desugar-check.arr\"_Functions"]{Functions}
  @function[
    "is-check-info"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function["get-checks" #:contract (a-arrow "Any" "Any")]
  @function["create-check-block" #:contract (a-arrow "Any" "Any" "Any")]
  @function["make-lam" #:contract (a-arrow "Any" "Any" "Any" "Any")]
  @function["desugar-check" #:contract (a-arrow "Any" "Any")]
  @function["desugar-no-checks" #:contract (a-arrow "Any" "Any")]
}