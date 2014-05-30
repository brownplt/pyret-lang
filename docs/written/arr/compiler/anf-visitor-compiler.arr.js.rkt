#lang scribble/base
@(require "../../../scribble-api.rkt")
@docmodule["\"compiler/anf-visitor-compiler.arr\""]{
  @; Unknown: PLEASE DOCUMENT
  @ignore[
    (list
      "get-field-loc"
      "throw-uninitialized"
      "source-name"
      "undefined"
      "compiler-visitor"
      "remove-useless-if-visitor")
  ]
  @section[#:tag "\"compiler/anf-visitor-compiler.arr\"_ReExports"]{
    Re-exported values
  }
  @re-export["j-fun" (from (xref "\"compiler/js-ast.arr\"" "j-fun"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-fun"]}
  }
  @re-export["j-var" (from (xref "\"compiler/js-ast.arr\"" "j-var"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-var"]}
  }
  @re-export["j-id" (from (xref "\"compiler/js-ast.arr\"" "j-id"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-id"]}
  }
  @re-export["j-method" (from (xref "\"compiler/js-ast.arr\"" "j-method"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-method"]}
  }
  @re-export["j-block" (from (xref "\"compiler/js-ast.arr\"" "j-block"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-block"]}
  }
  @re-export["j-true" (from (xref "\"compiler/js-ast.arr\"" "j-true"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-true"]}
  }
  @re-export["j-false" (from (xref "\"compiler/js-ast.arr\"" "j-false"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-false"]}
  }
  @re-export["j-num" (from (xref "\"compiler/js-ast.arr\"" "j-num"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-num"]}
  }
  @re-export["j-str" (from (xref "\"compiler/js-ast.arr\"" "j-str"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-str"]}
  }
  @re-export["j-return" (from (xref "\"compiler/js-ast.arr\"" "j-return"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-return"]}
  }
  @re-export["j-assign" (from (xref "\"compiler/js-ast.arr\"" "j-assign"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-assign"]}
  }
  @re-export["j-if" (from (xref "\"compiler/js-ast.arr\"" "j-if"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-if"]}
  }
  @re-export["j-if1" (from (xref "\"compiler/js-ast.arr\"" "j-if1"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-if1"]}
  }
  @re-export["j-app" (from (xref "\"compiler/js-ast.arr\"" "j-app"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-app"]}
  }
  @re-export["j-list" (from (xref "\"compiler/js-ast.arr\"" "j-list"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-list"]}
  }
  @re-export["j-obj" (from (xref "\"compiler/js-ast.arr\"" "j-obj"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-obj"]}
  }
  @re-export["j-dot" (from (xref "\"compiler/js-ast.arr\"" "j-dot"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-dot"]}
  }
  @re-export["j-bracket" (from (xref "\"compiler/js-ast.arr\"" "j-bracket"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-bracket"]}
  }
  @re-export["j-field" (from (xref "\"compiler/js-ast.arr\"" "j-field"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-field"]}
  }
  @re-export[
    "j-dot-assign"
    (from (xref "\"compiler/js-ast.arr\"" "j-dot-assign"))
  ]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-dot-assign"]}
  }
  @re-export[
    "j-bracket-assign"
    (from (xref "\"compiler/js-ast.arr\"" "j-bracket-assign"))
  ]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-bracket-assign"]}
  }
  @re-export[
    "j-try-catch"
    (from (xref "\"compiler/js-ast.arr\"" "j-try-catch"))
  ]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-try-catch"]}
  }
  @re-export["j-throw" (from (xref "\"compiler/js-ast.arr\"" "j-throw"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-throw"]}
  }
  @re-export["j-expr" (from (xref "\"compiler/js-ast.arr\"" "j-expr"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-expr"]}
  }
  @re-export["j-binop" (from (xref "\"compiler/js-ast.arr\"" "j-binop"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-binop"]}
  }
  @re-export["j-eq" (from (xref "\"compiler/js-ast.arr\"" "j-eq"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-eq"]}
  }
  @re-export["j-neq" (from (xref "\"compiler/js-ast.arr\"" "j-neq"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-neq"]}
  }
  @re-export["j-unop" (from (xref "\"compiler/js-ast.arr\"" "j-unop"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-unop"]}
  }
  @re-export["j-decr" (from (xref "\"compiler/js-ast.arr\"" "j-decr"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-decr"]}
  }
  @re-export["j-incr" (from (xref "\"compiler/js-ast.arr\"" "j-incr"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-incr"]}
  }
  @re-export["j-ternary" (from (xref "\"compiler/js-ast.arr\"" "j-ternary"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-ternary"]}
  }
  @re-export["j-null" (from (xref "\"compiler/js-ast.arr\"" "j-null"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-null"]}
  }
  @re-export["j-parens" (from (xref "\"compiler/js-ast.arr\"" "j-parens"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["\"compiler/js-ast.arr\"" "j-parens"]}
  }
  @re-export["Loc" (from (xref "srcloc" "Srcloc"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["srcloc" "Srcloc"]}
  }
  @section[#:tag "\"compiler/anf-visitor-compiler.arr\"_DataTypes"]{Data types}
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
                (xref "\"compiler/anf-visitor-compiler.arr\"" "ConcatList"))
              "Any"
              "Any")
          ]
          @method-spec[
            "map"
            #:contract
            (a-arrow
              (a-id
                "ConcatList"
                (xref "\"compiler/anf-visitor-compiler.arr\"" "ConcatList"))
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
                (xref "\"compiler/anf-visitor-compiler.arr\"" "ConcatList"))
              "Any"
              "Any")
          ]
          @method-spec[
            "map"
            #:contract
            (a-arrow
              (a-id
                "ConcatList"
                (xref "\"compiler/anf-visitor-compiler.arr\"" "ConcatList"))
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
                (xref "\"compiler/anf-visitor-compiler.arr\"" "ConcatList"))
              (a-id "a" (xref "<global>" "a")))
          ]
          @member-spec[
            "right"
            #:contract
            (a-app
              (a-id
                "ConcatList"
                (xref "\"compiler/anf-visitor-compiler.arr\"" "ConcatList"))
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
                (xref "\"compiler/anf-visitor-compiler.arr\"" "ConcatList"))
              (a-id "List" (xref "lists" "List"))
              "Any")
          ]
          @method-spec[
            "map"
            #:contract
            (a-arrow
              (a-id
                "ConcatList"
                (xref "\"compiler/anf-visitor-compiler.arr\"" "ConcatList"))
              "Any"
              "Any")
          ]
        }
      }
      @constr-spec["concat-cons"]{
        @members{
          @member-spec["first" #:contract (a-id "a" (xref "<global>" "a"))]
          @member-spec[
            "rest"
            #:contract
            (a-app
              (a-id
                "ConcatList"
                (xref "\"compiler/anf-visitor-compiler.arr\"" "ConcatList"))
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
                (xref "\"compiler/anf-visitor-compiler.arr\"" "ConcatList"))
              "Any"
              "Any")
          ]
          @method-spec[
            "map"
            #:contract
            (a-arrow
              (a-id
                "ConcatList"
                (xref "\"compiler/anf-visitor-compiler.arr\"" "ConcatList"))
              "Any"
              "Any")
          ]
        }
      }
      @constr-spec["concat-snoc"]{
        @members{
          @member-spec[
            "head"
            #:contract
            (a-app
              (a-id
                "ConcatList"
                (xref "\"compiler/anf-visitor-compiler.arr\"" "ConcatList"))
              (a-id "a" (xref "<global>" "a")))
          ]
          @member-spec["last" #:contract (a-id "a" (xref "<global>" "a"))]
        }
        @with-members{
          @method-spec[
            "to-list-acc"
            #:contract
            (a-arrow
              (a-id
                "ConcatList"
                (xref "\"compiler/anf-visitor-compiler.arr\"" "ConcatList"))
              "Any"
              "Any")
          ]
          @method-spec[
            "map"
            #:contract
            (a-arrow
              (a-id
                "ConcatList"
                (xref "\"compiler/anf-visitor-compiler.arr\"" "ConcatList"))
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
          (a-id
            "ConcatList"
            (xref "\"compiler/anf-visitor-compiler.arr\"" "ConcatList"))
          (a-id
            "ConcatList"
            (xref "\"compiler/anf-visitor-compiler.arr\"" "ConcatList"))
          "Any")
      ]
      @method-spec[
        "to-list"
        #:contract
        (a-arrow
          (a-id
            "ConcatList"
            (xref "\"compiler/anf-visitor-compiler.arr\"" "ConcatList"))
          "Any")
      ]
    }
  }
  @section[#:tag "\"compiler/anf-visitor-compiler.arr\"_Functions"]{Functions}
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
    "is-concat-cons"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-concat-snoc"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "js-id-of"
    #:contract (a-arrow (a-id "String" (xref "<global>" "String")) "Any")
  ]
  @function["compiler-name" #:contract (a-arrow "Any" "Any")]
  @function["obj-of-loc" #:contract (a-arrow "Any" "Any")]
  @function["get-field" #:contract (a-arrow "Any" "Any" "Any" "Any")]
  @function["raise-id-exn" #:contract (a-arrow "Any" "Any" "Any")]
  @function["add-stack-frame" #:contract (a-arrow "Any" "Any" "Any")]
  @function["rt-field" #:contract (a-arrow "Any" "Any")]
  @function["rt-method" #:contract (a-arrow "Any" "Any" "Any")]
  @function["app" #:contract (a-arrow "Any" "Any" "Any" "Any")]
  @function["thunk-app" #:contract (a-arrow "Any" "Any")]
  @function["thunk-app-stmt" #:contract (a-arrow "Any" "Any")]
  @function[
    "helper-name"
    #:contract (a-arrow (a-id "String" (xref "<global>" "String")) "Any")
  ]
  @function[
    "compile-helper"
    #:contract
    (a-arrow
      "Any"
      (a-compound
        (a-dot "S" "Helper")
        (xref "\"compiler/ast-split.arr\"" "Helper"))
      (a-compound (a-dot "J" "JStmt") (xref "\"compiler/js-ast.arr\"" "JStmt")))
  ]
  @function[
    "compile-tail-app"
    #:contract (a-arrow "Any" "Any" "Any" "Any" "Any")
  ]
  @function[
    "compile-split-app"
    #:contract
    (a-arrow
      "Any"
      (a-id "Loc" (xref "srcloc" "Srcloc"))
      (a-id "Boolean" (xref "<global>" "Boolean"))
      (a-compound (a-dot "N" "AVal") (xref "\"compiler/ast-anf.arr\"" "AVal"))
      (a-app
        (a-id "List" (xref "lists" "List"))
        (a-compound (a-dot "A" "Name") (xref "ast" "Name")))
      (a-compound (a-dot "A" "Name") (xref "ast" "Name"))
      (a-app
        (a-id "List" (xref "lists" "List"))
        (a-compound (a-dot "A" "AVal") (xref "ast" "AVal")))
      "Any")
  ]
  @function["arity-check" #:contract (a-arrow "Any" "Any" "Any" "Any")]
  @function["mk-abbrevs" #:contract (a-arrow "Any" "Any")]
  @function[
    "compile-program"
    #:contract (a-arrow "Any" "Any" "Any" "Any" "Any" "Any")
  ]
  @function["splitting-compiler" #:contract (a-arrow "Any" "Any")]
  @function["non-splitting-compiler" #:contract (a-arrow "Any" "Any")]
}