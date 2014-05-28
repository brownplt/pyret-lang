#lang scribble/base
@(require "../../../scribble-api.rkt")
@docmodule["\"compiler/anf.arr\""]{
  @section[#:tag "\"compiler/anf.arr\"_ReExports"]{Re-exported values}
  @re-export["names" (from (xref "ast" "global-names"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["ast" "global-names"]}
  }
  @section[#:tag "\"compiler/anf.arr\"_DataTypes"]{Data types}
  @data-spec["ANFCont"]{
    @variants{
      @constr-spec["k-cont"]{
        @members{
          @member-spec[
            "k"
            #:contract
            (a-arrow
              (a-compound
                (a-dot "N" "ALettable")
                (xref "\"compiler/ast-anf.arr\"" "ALettable"))
              (a-compound
                (a-dot "N" "AExpr")
                (xref "\"compiler/ast-anf.arr\"" "AExpr")))
          ]
        }
        @with-members{
          @method-spec[
            "apply"
            #:contract
            (a-arrow
              (a-id "ANFCont" (xref "\"compiler/anf.arr\"" "ANFCont"))
              (a-id "Loc" (xref "<global>" "Loc"))
              (a-compound
                (a-dot "N" "ALettable")
                (xref "\"compiler/ast-anf.arr\"" "ALettable"))
              "Any")
          ]
        }
      }
      @constr-spec["k-id"]{
        @members{
          @member-spec["name" #:contract (a-id "Name" (xref "<global>" "Name"))]
        }
        @with-members{
          @method-spec[
            "apply"
            #:contract
            (a-arrow
              (a-id "ANFCont" (xref "\"compiler/anf.arr\"" "ANFCont"))
              (a-id "Loc" (xref "<global>" "Loc"))
              (a-compound
                (a-dot "N" "ALettable")
                (xref "\"compiler/ast-anf.arr\"" "ALettable"))
              "Any")
          ]
        }
      }
    }
    @shared{}
  }
  @section[#:tag "\"compiler/anf.arr\"_Functions"]{Functions}
  @function["mk-id" #:contract (a-arrow "Any" "Any" "Any")]
  @function[
    "is-k-cont"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-k-id"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "anf-term"
    #:contract
    (a-arrow
      (a-compound (a-dot "A" "Expr") (xref "ast" "Expr"))
      (a-compound (a-dot "N" "AExpr") (xref "\"compiler/ast-anf.arr\"" "AExpr")))
  ]
  @function["bind" #:contract (a-arrow "Any" "Any" "Any")]
  @function["anf-bind" #:contract (a-arrow "Any" "Any")]
  @function[
    "anf-name"
    #:contract
    (a-arrow
      (a-compound (a-dot "A" "Expr") (xref "ast" "Expr"))
      (a-id "String" (xref "<global>" "String"))
      (a-arrow
        (a-compound (a-dot "N" "AVal") (xref "\"compiler/ast-anf.arr\"" "AVal"))
        (a-compound
          (a-dot "N" "AExpr")
          (xref "\"compiler/ast-anf.arr\"" "AExpr")))
      (a-compound (a-dot "N" "AExpr") (xref "\"compiler/ast-anf.arr\"" "AExpr")))
  ]
  @function[
    "anf-name-rec"
    #:contract
    (a-arrow
      (a-app
        (a-id "List" (xref "lists" "List"))
        (a-compound (a-dot "A" "Expr") (xref "ast" "Expr")))
      (a-id "String" (xref "<global>" "String"))
      (a-arrow
        (a-app
          (a-id "List" (xref "lists" "List"))
          (a-compound
            (a-dot "N" "AVal")
            (xref "\"compiler/ast-anf.arr\"" "AVal")))
        (a-compound
          (a-dot "N" "AExpr")
          (xref "\"compiler/ast-anf.arr\"" "AExpr")))
      (a-compound (a-dot "N" "AExpr") (xref "\"compiler/ast-anf.arr\"" "AExpr")))
  ]
  @function[
    "anf-program"
    #:contract
    (a-arrow (a-compound (a-dot "A" "Program") (xref "ast" "Program")) "Any")
  ]
  @function[
    "anf-import"
    #:contract
    (a-arrow (a-compound (a-dot "A" "Import") (xref "ast" "Import")) "Any")
  ]
  @function[
    "anf-block"
    #:contract
    (a-arrow
      (a-app
        (a-id "List" (xref "lists" "List"))
        (a-compound (a-dot "A" "Expr") (xref "ast" "Expr")))
      (a-id "ANFCont" (xref "\"compiler/anf.arr\"" "ANFCont"))
      "Any")
  ]
  @function[
    "anf"
    #:contract
    (a-arrow
      (a-compound (a-dot "A" "Expr") (xref "ast" "Expr"))
      (a-id "ANFCont" (xref "\"compiler/anf.arr\"" "ANFCont"))
      (a-compound (a-dot "N" "AExpr") (xref "\"compiler/ast-anf.arr\"" "AExpr")))
  ]
}