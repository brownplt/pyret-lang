#lang scribble/base
@(require "../../../scribble-api.rkt")
@docmodule["\"compiler/ast-split.arr\""]{
  @; Unknown: PLEASE DOCUMENT
  @ignore[(list "INDENT")]
  @section[#:tag "\"compiler/ast-split.arr\"_ReExports"]{Re-exported values}
  @re-export["names" (from (xref "ast" "global-names"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["ast" "global-names"]}
  }
  @section[#:tag "\"compiler/ast-split.arr\"_DataTypes"]{Data types}
  @data-spec["ConcatList" #:params (list "a")]{
    @variants{
      @singleton-spec["concat-empty"]{
        @with-members{
          @method-spec[
            "to-list-acc"
            #:contract
            (a-arrow
              (a-id
                "ConcatList"
                (xref "\"compiler/ast-split.arr\"" "ConcatList"))
              "Any"
              "Any")
          ]
          @method-spec[
            "map"
            #:contract
            (a-arrow
              (a-id
                "ConcatList"
                (xref "\"compiler/ast-split.arr\"" "ConcatList"))
              "Any"
              "Any")
          ]
        }
      }
      @constr-spec["concat-singleton"]{
        @members{@member-spec["element" #:contract "Any"]}
        @with-members{
          @method-spec[
            "to-list-acc"
            #:contract
            (a-arrow
              (a-id
                "ConcatList"
                (xref "\"compiler/ast-split.arr\"" "ConcatList"))
              "Any"
              "Any")
          ]
          @method-spec[
            "map"
            #:contract
            (a-arrow
              (a-id
                "ConcatList"
                (xref "\"compiler/ast-split.arr\"" "ConcatList"))
              "Any"
              "Any")
          ]
        }
      }
      @constr-spec["concat-append"]{
        @members{
          @member-spec[
            "left"
            #:contract
            (a-app
              (a-id
                "ConcatList"
                (xref "\"compiler/ast-split.arr\"" "ConcatList"))
              (a-id "a" (xref "<global>" "a")))
          ]
          @member-spec[
            "right"
            #:contract
            (a-app
              (a-id
                "ConcatList"
                (xref "\"compiler/ast-split.arr\"" "ConcatList"))
              (a-id "a" (xref "<global>" "a")))
          ]
        }
        @with-members{
          @method-spec[
            "to-list-acc"
            #:contract
            (a-arrow
              (a-id
                "ConcatList"
                (xref "\"compiler/ast-split.arr\"" "ConcatList"))
              (a-id "List" (xref "lists" "List"))
              "Any")
          ]
          @method-spec[
            "map"
            #:contract
            (a-arrow
              (a-id
                "ConcatList"
                (xref "\"compiler/ast-split.arr\"" "ConcatList"))
              "Any"
              "Any")
          ]
        }
      }
    }
    @shared{
      @method-spec[
        "_plus"
        #:contract
        (a-arrow
          (a-id "ConcatList" (xref "\"compiler/ast-split.arr\"" "ConcatList"))
          (a-id "ConcatList" (xref "\"compiler/ast-split.arr\"" "ConcatList"))
          "Any")
      ]
      @method-spec[
        "to-list"
        #:contract
        (a-arrow
          (a-id "ConcatList" (xref "\"compiler/ast-split.arr\"" "ConcatList"))
          "Any")
      ]
    }
  }
  @data-spec["Helper"]{
    @variants{
      @constr-spec["helper"]{
        @members{
          @member-spec["name" #:contract (a-id "Name" (xref "<global>" "Name"))]
          @member-spec[
            "args"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "Name" (xref "<global>" "Name")))
          ]
          @member-spec[
            "body"
            #:contract
            (a-compound
              (a-dot "N" "AExpr")
              (xref "\"compiler/ast-anf.arr\"" "AExpr"))
          ]
        }
        @with-members{
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id "Helper" (xref "\"compiler/ast-split.arr\"" "Helper"))
              "Any")
          ]
        }
      }
    }
    @shared{}
  }
  @data-spec["SplitResult"]{
    @variants{
      @constr-spec["split-result"]{
        @members{
          @member-spec[
            "helpers"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "Helper" (xref "\"compiler/ast-split.arr\"" "Helper")))
          ]
          @member-spec[
            "body"
            #:contract
            (a-compound
              (a-dot "N" "AExpr")
              (xref "\"compiler/ast-anf.arr\"" "AExpr"))
          ]
          @member-spec[
            "freevars"
            #:contract
            (a-app
              (a-id "Set" (xref "<global>" "Set"))
              (a-id "Name" (xref "<global>" "Name")))
          ]
        }
        @with-members{
          @method-spec[
            "tosource"
            #:contract
            (a-arrow
              (a-id
                "SplitResult"
                (xref "\"compiler/ast-split.arr\"" "SplitResult"))
              "Any")
          ]
        }
      }
    }
    @shared{}
  }
  @data-spec["SplitResultInt"]{
    @variants{
      @constr-spec["split-result-int-e"]{
        @members{
          @member-spec[
            "helpers"
            #:contract
            (a-app
              (a-id
                "ConcatList"
                (xref "\"compiler/ast-split.arr\"" "ConcatList"))
              (a-id "Helper" (xref "\"compiler/ast-split.arr\"" "Helper")))
          ]
          @member-spec[
            "body"
            #:contract
            (a-compound
              (a-dot "N" "AExpr")
              (xref "\"compiler/ast-anf.arr\"" "AExpr"))
          ]
          @member-spec[
            "freevars"
            #:contract
            (a-app
              (a-id "Set" (xref "<global>" "Set"))
              (a-id "String" (xref "<global>" "String")))
          ]
        }
        @with-members{}
      }
      @constr-spec["split-result-int-l"]{
        @members{
          @member-spec[
            "helpers"
            #:contract
            (a-app
              (a-id
                "ConcatList"
                (xref "\"compiler/ast-split.arr\"" "ConcatList"))
              (a-id "Helper" (xref "\"compiler/ast-split.arr\"" "Helper")))
          ]
          @member-spec[
            "body"
            #:contract
            (a-compound
              (a-dot "N" "ALettable")
              (xref "\"compiler/ast-anf.arr\"" "ALettable"))
          ]
          @member-spec[
            "freevars"
            #:contract
            (a-app
              (a-id "Set" (xref "<global>" "Set"))
              (a-id "String" (xref "<global>" "String")))
          ]
        }
        @with-members{}
      }
    }
    @shared{}
  }
  @section[#:tag "\"compiler/ast-split.arr\"_Functions"]{Functions}
  @function[
    "is-concat-empty"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-concat-singleton"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-concat-append"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-helper"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "freevars-helper"
    #:contract
    (a-arrow (a-id "Helper" (xref "\"compiler/ast-split.arr\"" "Helper")) "Any")
  ]
  @function[
    "is-split-result"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "freevars-split-result"
    #:contract
    (a-arrow
      (a-id "SplitResult" (xref "\"compiler/ast-split.arr\"" "SplitResult"))
      "Any")
  ]
  @function[
    "is-split-result-int-e"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-split-result-int-l"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "unions"
    #:params (list "a")
    #:contract
    (a-arrow
      (a-app
        (a-id "List" (xref "lists" "List"))
        (a-app (a-id "Set" (xref "<global>" "Set")) "a"))
      (a-app (a-id "Set" (xref "<global>" "Set")) "a"))
  ]
  @function[
    "ast-split"
    #:contract
    (a-arrow
      (a-compound (a-dot "N" "AExpr") (xref "\"compiler/ast-anf.arr\"" "AExpr"))
      (a-id "SplitResult" (xref "\"compiler/ast-split.arr\"" "SplitResult")))
  ]
  @function[
    "ast-split-expr"
    #:contract
    (a-arrow
      (a-compound (a-dot "N" "AExpr") (xref "\"compiler/ast-anf.arr\"" "AExpr"))
      (a-id
        "SplitResultInt"
        (xref "\"compiler/ast-split.arr\"" "SplitResultInt")))
  ]
  @function[
    "ast-split-lettable"
    #:contract
    (a-arrow
      (a-compound
        (a-dot "N" "ALettable")
        (xref "\"compiler/ast-anf.arr\"" "ALettable"))
      (a-id
        "is-split-result-int-l"
        (xref "\"compiler/ast-split.arr\"" "is-split-result-int-l")))
  ]
  @function["param" #:contract (a-arrow "Any" "Any" "Any")]
}