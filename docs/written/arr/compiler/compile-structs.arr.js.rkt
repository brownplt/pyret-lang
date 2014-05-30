#lang scribble/base
@(require "../../../scribble-api.rkt")
@docmodule["\"compiler/compile-structs.arr\""]{
  @; Unknown: PLEASE DOCUMENT
  @ignore[
    (list
      "runtime-builtins"
      "no-builtins"
      "minimal-builtins"
      "bootstrap-builtins"
      "standard-builtins")
  ]
  @section[#:tag "\"compiler/compile-structs.arr\"_ReExports"]{
    Re-exported values
  }
  @section[#:tag "\"compiler/compile-structs.arr\"_DataTypes"]{Data types}
  @data-spec["PyretDialect"]{
    @variants{
      @singleton-spec["Pyret"]{@with-members{}}
      @singleton-spec["Bootstrap"]{@with-members{}}
    }
    @shared{}
  }
  @data-spec["CompileEnvironment"]{
    @variants{
      @constr-spec["compile-env"]{
        @members{
          @member-spec[
            "bindings"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id
                "CompileBinding"
                (xref "\"compiler/compile-structs.arr\"" "CompileBinding")))
          ]
        }
        @with-members{}
      }
    }
    @shared{}
  }
  @data-spec["CompileResult" #:params (list "C")]{
    @variants{
      @constr-spec["ok"]{
        @members{
          @member-spec["code" #:contract (a-id "C" (xref "<global>" "C"))]
        }
        @with-members{}
      }
      @constr-spec["err"]{
        @members{
          @member-spec[
            "problems"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id
                "CompileError"
                (xref "\"compiler/compile-structs.arr\"" "CompileError")))
          ]
        }
        @with-members{}
      }
    }
    @shared{}
  }
  @data-spec["CompileError"]{
    @variants{
      @constr-spec["wf-err"]{
        @members{
          @member-spec[
            "msg"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec[
            "loc"
            #:contract (a-compound (a-dot "A" "Loc") (xref "ast" "Loc"))
          ]
        }
        @with-members{
          @method-spec[
            "tostring"
            #:contract
            (a-arrow
              (a-id
                "CompileError"
                (xref "\"compiler/compile-structs.arr\"" "CompileError"))
              "Any")
          ]
        }
      }
      @constr-spec["wf-err-split"]{
        @members{
          @member-spec[
            "msg"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec[
            "loc"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-compound (a-dot "A" "Loc") (xref "ast" "Loc")))
          ]
        }
        @with-members{
          @method-spec[
            "tostring"
            #:contract
            (a-arrow
              (a-id
                "CompileError"
                (xref "\"compiler/compile-structs.arr\"" "CompileError"))
              "Any")
          ]
        }
      }
      @constr-spec["reserved-name"]{
        @members{
          @member-spec["loc" #:contract (a-id "Loc" (xref "<global>" "Loc"))]
          @member-spec[
            "id"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
        }
        @with-members{
          @method-spec[
            "tostring"
            #:contract
            (a-arrow
              (a-id
                "CompileError"
                (xref "\"compiler/compile-structs.arr\"" "CompileError"))
              "Any")
          ]
        }
      }
      @constr-spec["zero-fraction"]{
        @members{
          @member-spec["loc" #:contract "Any"]
          @member-spec["numerator" #:contract "Any"]
        }
        @with-members{
          @method-spec[
            "tostring"
            #:contract
            (a-arrow
              (a-id
                "CompileError"
                (xref "\"compiler/compile-structs.arr\"" "CompileError"))
              "Any")
          ]
        }
      }
      @constr-spec["unbound-id"]{
        @members{
          @member-spec[
            "id"
            #:contract (a-compound (a-dot "A" "Expr") (xref "ast" "Expr"))
          ]
        }
        @with-members{
          @method-spec[
            "tostring"
            #:contract
            (a-arrow
              (a-id
                "CompileError"
                (xref "\"compiler/compile-structs.arr\"" "CompileError"))
              "Any")
          ]
        }
      }
      @constr-spec["unbound-var"]{
        @members{
          @member-spec[
            "id"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec["loc" #:contract (a-id "Loc" (xref "<global>" "Loc"))]
        }
        @with-members{
          @method-spec[
            "tostring"
            #:contract
            (a-arrow
              (a-id
                "CompileError"
                (xref "\"compiler/compile-structs.arr\"" "CompileError"))
              "Any")
          ]
        }
      }
      @constr-spec["pointless-var"]{
        @members{
          @member-spec["loc" #:contract (a-id "Loc" (xref "<global>" "Loc"))]
        }
        @with-members{
          @method-spec[
            "tostring"
            #:contract
            (a-arrow
              (a-id
                "CompileError"
                (xref "\"compiler/compile-structs.arr\"" "CompileError"))
              "Any")
          ]
        }
      }
      @constr-spec["pointless-shadow"]{
        @members{
          @member-spec["loc" #:contract (a-id "Loc" (xref "<global>" "Loc"))]
        }
        @with-members{
          @method-spec[
            "tostring"
            #:contract
            (a-arrow
              (a-id
                "CompileError"
                (xref "\"compiler/compile-structs.arr\"" "CompileError"))
              "Any")
          ]
        }
      }
      @constr-spec["bad-assignment"]{
        @members{
          @member-spec[
            "id"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec["loc" #:contract (a-id "Loc" (xref "<global>" "Loc"))]
          @member-spec[
            "prev-loc"
            #:contract (a-id "Loc" (xref "<global>" "Loc"))
          ]
        }
        @with-members{
          @method-spec[
            "tostring"
            #:contract
            (a-arrow
              (a-id
                "CompileError"
                (xref "\"compiler/compile-structs.arr\"" "CompileError"))
              "Any")
          ]
        }
      }
      @constr-spec["mixed-id-var"]{
        @members{
          @member-spec[
            "id"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec[
            "var-loc"
            #:contract (a-id "Loc" (xref "<global>" "Loc"))
          ]
          @member-spec["id-loc" #:contract (a-id "Loc" (xref "<global>" "Loc"))]
        }
        @with-members{
          @method-spec[
            "tostring"
            #:contract
            (a-arrow
              (a-id
                "CompileError"
                (xref "\"compiler/compile-structs.arr\"" "CompileError"))
              "Any")
          ]
        }
      }
      @constr-spec["shadow-id"]{
        @members{
          @member-spec[
            "id"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec[
            "new-loc"
            #:contract (a-id "Loc" (xref "<global>" "Loc"))
          ]
          @member-spec[
            "old-loc"
            #:contract (a-id "Loc" (xref "<global>" "Loc"))
          ]
        }
        @with-members{
          @method-spec[
            "tostring"
            #:contract
            (a-arrow
              (a-id
                "CompileError"
                (xref "\"compiler/compile-structs.arr\"" "CompileError"))
              "Any")
          ]
        }
      }
      @constr-spec["duplicate-id"]{
        @members{
          @member-spec[
            "id"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec[
            "new-loc"
            #:contract (a-id "Loc" (xref "<global>" "Loc"))
          ]
          @member-spec[
            "old-loc"
            #:contract (a-id "Loc" (xref "<global>" "Loc"))
          ]
        }
        @with-members{
          @method-spec[
            "tostring"
            #:contract
            (a-arrow
              (a-id
                "CompileError"
                (xref "\"compiler/compile-structs.arr\"" "CompileError"))
              "Any")
          ]
        }
      }
    }
    @shared{}
  }
  @data-spec["CompileBinding"]{
    @variants{
      @constr-spec["builtin-id"]{
        @members{
          @member-spec[
            "id"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
        }
        @with-members{}
      }
      @constr-spec["module-bindings"]{
        @members{
          @member-spec[
            "name"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec[
            "bindings"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "String" (xref "<global>" "String")))
          ]
        }
        @with-members{}
      }
    }
    @shared{}
  }
  @section[#:tag "\"compiler/compile-structs.arr\"_Functions"]{Functions}
  @function[
    "is-Pyret"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-Bootstrap"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-compile-env"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-ok"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-err"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-wf-err"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-wf-err-split"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-reserved-name"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-zero-fraction"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-unbound-id"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-unbound-var"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-pointless-var"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-pointless-shadow"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-bad-assignment"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-mixed-id-var"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-shadow-id"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-duplicate-id"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-builtin-id"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-module-bindings"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
}