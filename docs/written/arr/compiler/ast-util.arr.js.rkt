#lang scribble/base
@(require "../../../scribble-api.rkt")
@docmodule["\"compiler/ast-util.arr\""]{
  @; Unknown: PLEASE DOCUMENT
  @ignore[
    (list
      "flatten-single-blocks"
      "merge-nested-blocks"
      "binding-handlers"
      "inline-lams"
      "letrec-visitor")
  ]
  @section[#:tag "\"compiler/ast-util.arr\"_ReExports"]{Re-exported values}
  @section[#:tag "\"compiler/ast-util.arr\"_DataTypes"]{Data types}
  @data-spec["BindingInfo"]{
    @variants{
      @constr-spec["b-prim"]{
        @members{
          @member-spec[
            "name"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
        }
        @with-members{}
      }
      @constr-spec["b-dict"]{
        @members{
          @member-spec[
            "dict"
            #:contract
            (a-compound
              (a-dot "SD" "StringDict")
              (xref "string-dict" "StringDict"))
          ]
        }
        @with-members{}
      }
      @constr-spec["b-exp"]{
        @members{
          @member-spec[
            "exp"
            #:contract (a-compound (a-dot "A" "Expr") (xref "ast" "Expr"))
          ]
        }
        @with-members{}
      }
      @constr-spec["b-dot"]{
        @members{
          @member-spec[
            "base"
            #:contract
            (a-id
              "BindingInfo"
              (xref "\"compiler/ast-util.arr\"" "BindingInfo"))
          ]
          @member-spec[
            "name"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
        }
        @with-members{}
      }
      @singleton-spec["b-unknown"]{@with-members{}}
    }
    @shared{}
  }
  @data-spec["Binding"]{
    @variants{
      @constr-spec["e-bind"]{
        @members{
          @member-spec["loc" #:contract (a-id "Loc" (xref "<global>" "Loc"))]
          @member-spec["mut" #:contract (a-id "Bool" (xref "<global>" "Bool"))]
          @member-spec[
            "info"
            #:contract
            (a-id
              "BindingInfo"
              (xref "\"compiler/ast-util.arr\"" "BindingInfo"))
          ]
        }
        @with-members{}
      }
    }
    @shared{}
  }
  @section[#:tag "\"compiler/ast-util.arr\"_Functions"]{Functions}
  @function["ok-last" #:contract (a-arrow "Any" "Any")]
  @function["checkers" #:contract (a-arrow "Any" "Any")]
  @function[
    "append-nothing-if-necessary"
    #:contract
    (a-arrow
      (a-compound (a-dot "A" "Program") (xref "ast" "Program"))
      (a-app
        (a-id "Option" (xref "option" "Option"))
        (a-compound (a-dot "A" "Program") (xref "ast" "Program"))))
  ]
  @function["count-apps" #:contract (a-arrow "Any" "Any")]
  @function[
    "is-b-prim"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-b-dict"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-b-exp"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-b-dot"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-b-unknown"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-e-bind"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "bind-exp"
    #:contract
    (a-arrow
      (a-compound (a-dot "A" "Expr") (xref "ast" "Expr"))
      "Any"
      (a-app
        (a-id "Option" (xref "option" "Option"))
        (a-id "Binding" (xref "\"compiler/ast-util.arr\"" "Binding"))))
  ]
  @function[
    "bind-or-unknown"
    #:contract
    (a-arrow
      (a-compound (a-dot "A" "Expr") (xref "ast" "Expr"))
      "Any"
      (a-id "BindingInfo" (xref "\"compiler/ast-util.arr\"" "BindingInfo")))
  ]
  @function["binding-env-from-env" #:contract (a-arrow "Any" "Any")]
  @function[
    "default-env-map-visitor"
    #:params (list "a")
    #:contract
    (a-arrow
      "a"
      (a-record
        (a-field "s-letrec-bind" (a-arrow (a-dot "A" "LetrecBind") "a" "a"))
        (a-field "s-let-bind" (a-arrow (a-dot "A" "LetBind") "a" "a"))
        (a-field "s-bind" (a-arrow (a-dot "A" "Bind") "a" "a"))
        (a-field "s-header" (a-arrow (a-dot "A" "Header") "a" "a")))
      "Any")
  ]
  @function[
    "default-env-iter-visitor"
    #:params (list "a")
    #:contract
    (a-arrow
      "a"
      (a-record
        (a-field "s-letrec-bind" (a-arrow (a-dot "A" "LetrecBind") "a" "a"))
        (a-field "s-let-bind" (a-arrow (a-dot "A" "LetBind") "a" "a"))
        (a-field "s-bind" (a-arrow (a-dot "A" "Bind") "a" "a"))
        (a-field "s-header" (a-arrow (a-dot "A" "Header") "a" "a")))
      "Any")
  ]
  @function["binding-env-map-visitor" #:contract (a-arrow "Any" "Any")]
  @function["binding-env-iter-visitor" #:contract (a-arrow "Any" "Any")]
  @function["link-list-visitor" #:contract (a-arrow "Any" "Any")]
  @function["bad-assignments" #:contract (a-arrow "Any" "Any" "Any")]
  @function["check-unbound" #:contract (a-arrow "Any" "Any" "Any")]{
    @; let 
    @;     p = PP.surface-parse(_, "test"),
    @;     unbound1 = check-unbound(CS.no-builtins, p("x")):
    @;   unbound1.length() is 1
    @; end
    
  }
  @function["value-delays-exec-of" #:contract (a-arrow "Any" "Any" "Any")]
}