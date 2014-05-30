#lang scribble/base
@(require "../../scribble-api.rkt")
@docmodule["error"]{
  @section[#:tag "error_ReExports"]{Re-exported values}
  @section[#:tag "error_DataTypes"]{Data types}
  @data-spec["RuntimeError"]{
    @variants{
      @constr-spec["message-exception"]{
        @members{
          @member-spec[
            "message"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
        }
        @with-members{
          @method-spec[
            "tostring"
            #:contract
            (a-arrow (a-id "RuntimeError" (xref "error" "RuntimeError")) "Any")
          ]
        }
      }
      @constr-spec["no-branches-matched"]{
        @members{
          @member-spec["loc" #:contract "Any"]
          @member-spec[
            "expression"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
        }
        @with-members{
          @method-spec[
            "tostring"
            #:contract
            (a-arrow (a-id "RuntimeError" (xref "error" "RuntimeError")) "Any")
          ]
        }
      }
      @constr-spec["internal-error"]{
        @members{
          @member-spec["message" #:contract "Any"]
          @member-spec["info-args" #:contract "Any"]
        }
        @with-members{
          @method-spec[
            "tostring"
            #:contract
            (a-arrow (a-id "RuntimeError" (xref "error" "RuntimeError")) "Any")
          ]
        }
      }
      @constr-spec["field-not-found"]{
        @members{
          @member-spec["loc" #:contract "Any"]
          @member-spec["obj" #:contract "Any"]
          @member-spec[
            "field"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
        }
        @with-members{
          @method-spec[
            "tostring"
            #:contract
            (a-arrow (a-id "RuntimeError" (xref "error" "RuntimeError")) "Any")
          ]
        }
      }
      @constr-spec["lookup-non-object"]{
        @members{
          @member-spec["loc" #:contract "Any"]
          @member-spec["non-obj" #:contract "Any"]
          @member-spec[
            "field"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
        }
        @with-members{
          @method-spec[
            "tostring"
            #:contract
            (a-arrow (a-id "RuntimeError" (xref "error" "RuntimeError")) "Any")
          ]
        }
      }
      @constr-spec["non-boolean-condition"]{
        @members{
          @member-spec["loc" #:contract "Any"]
          @member-spec["typ" #:contract "Any"]
          @member-spec["value" #:contract "Any"]
        }
        @with-members{
          @method-spec[
            "tostring"
            #:contract
            (a-arrow (a-id "RuntimeError" (xref "error" "RuntimeError")) "Any")
          ]
        }
      }
      @constr-spec["non-boolean-op"]{
        @members{
          @member-spec["loc" #:contract "Any"]
          @member-spec["position" #:contract "Any"]
          @member-spec["typ" #:contract "Any"]
          @member-spec["value" #:contract "Any"]
        }
        @with-members{
          @method-spec[
            "tostring"
            #:contract
            (a-arrow (a-id "RuntimeError" (xref "error" "RuntimeError")) "Any")
          ]
        }
      }
      @constr-spec["generic-type-mismatch"]{
        @members{
          @member-spec["val" #:contract "Any"]
          @member-spec[
            "typ"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
        }
        @with-members{
          @method-spec[
            "tostring"
            #:contract
            (a-arrow (a-id "RuntimeError" (xref "error" "RuntimeError")) "Any")
          ]
        }
      }
      @constr-spec["outside-numeric-range"]{
        @members{
          @member-spec["val" #:contract "Any"]
          @member-spec["low" #:contract "Any"]
          @member-spec["high" #:contract "Any"]
        }
        @with-members{
          @method-spec[
            "tostring"
            #:contract
            (a-arrow (a-id "RuntimeError" (xref "error" "RuntimeError")) "Any")
          ]
        }
      }
      @constr-spec["plus-error"]{
        @members{
          @member-spec["val1" #:contract "Any"]
          @member-spec["val2" #:contract "Any"]
        }
        @with-members{
          @method-spec[
            "tostring"
            #:contract
            (a-arrow (a-id "RuntimeError" (xref "error" "RuntimeError")) "Any")
          ]
        }
      }
      @constr-spec["numeric-binop-error"]{
        @members{
          @member-spec["val1" #:contract "Any"]
          @member-spec["val2" #:contract "Any"]
          @member-spec["opname" #:contract "Any"]
          @member-spec["methodname" #:contract "Any"]
        }
        @with-members{
          @method-spec[
            "tostring"
            #:contract
            (a-arrow (a-id "RuntimeError" (xref "error" "RuntimeError")) "Any")
          ]
        }
      }
      @constr-spec["arity-mismatch"]{
        @members{
          @member-spec["fun-loc" #:contract "Any"]
          @member-spec["expected-arity" #:contract "Any"]
          @member-spec["args" #:contract "Any"]
        }
        @with-members{
          @method-spec[
            "tostring"
            #:contract
            (a-arrow (a-id "RuntimeError" (xref "error" "RuntimeError")) "Any")
          ]
        }
      }
      @constr-spec["non-function-app"]{
        @members{
          @member-spec["loc" #:contract "Any"]
          @member-spec["non-fun-val" #:contract "Any"]
          @member-spec["args" #:contract "Any"]
        }
        @with-members{
          @method-spec[
            "tostring"
            #:contract
            (a-arrow (a-id "RuntimeError" (xref "error" "RuntimeError")) "Any")
          ]
        }
      }
      @constr-spec["bad-app"]{
        @members{
          @member-spec["loc" #:contract "Any"]
          @member-spec[
            "fun-name"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec[
            "message"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec[
            "arg-position"
            #:contract (a-id "Number" (xref "<global>" "Number"))
          ]
          @member-spec["arg-val" #:contract "Any"]
        }
        @with-members{}
      }
      @constr-spec["uninitialized-id"]{
        @members{
          @member-spec["loc" #:contract "Any"]
          @member-spec[
            "name"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
        }
        @with-members{
          @method-spec[
            "tostring"
            #:contract
            (a-arrow (a-id "RuntimeError" (xref "error" "RuntimeError")) "Any")
          ]
        }
      }
      @constr-spec["module-load-failure"]{
        @members{
          @member-spec[
            "names"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "String" (xref "<global>" "String")))
          ]
        }
        @with-members{
          @method-spec[
            "tostring"
            #:contract
            (a-arrow (a-id "RuntimeError" (xref "error" "RuntimeError")) "Any")
          ]
        }
      }
      @constr-spec["invalid-array-index"]{
        @members{
          @member-spec[
            "method-name"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec[
            "array"
            #:contract (a-id "Array" (xref "<global>" "Array"))
          ]
          @member-spec[
            "index"
            #:contract (a-id "Number" (xref "<global>" "Number"))
          ]
          @member-spec[
            "reason"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
        }
        @with-members{
          @method-spec[
            "tostring"
            #:contract
            (a-arrow (a-id "RuntimeError" (xref "error" "RuntimeError")) "Any")
          ]
        }
      }
      @singleton-spec["user-break"]{@with-members{}}
    }
    @shared{}
  }
  @data-spec["ParseError"]{
    @variants{
      @constr-spec["parse-error-next-token"]{
        @members{
          @member-spec["loc" #:contract (a-id "Loc" (xref "<global>" "Loc"))]
          @member-spec[
            "next-token"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
        }
        @with-members{
          @method-spec[
            "tostring"
            #:contract
            (a-arrow (a-id "ParseError" (xref "error" "ParseError")) "Any")
          ]
        }
      }
      @constr-spec["parse-error-eof"]{
        @members{
          @member-spec["loc" #:contract (a-id "Loc" (xref "<global>" "Loc"))]
        }
        @with-members{
          @method-spec[
            "tostring"
            #:contract
            (a-arrow (a-id "ParseError" (xref "error" "ParseError")) "Any")
          ]
        }
      }
      @constr-spec["empty-block"]{
        @members{
          @member-spec["loc" #:contract (a-id "Loc" (xref "<global>" "Loc"))]
        }
        @with-members{
          @method-spec[
            "tostring"
            #:contract
            (a-arrow (a-id "ParseError" (xref "error" "ParseError")) "Any")
          ]
        }
      }
      @constr-spec["bad-block-stmt"]{
        @members{
          @member-spec["loc" #:contract (a-id "Loc" (xref "<global>" "Loc"))]
        }
        @with-members{
          @method-spec[
            "tostring"
            #:contract
            (a-arrow (a-id "ParseError" (xref "error" "ParseError")) "Any")
          ]
        }
      }
      @constr-spec["bad-check-block-stmt"]{
        @members{
          @member-spec["loc" #:contract (a-id "Loc" (xref "<global>" "Loc"))]
        }
        @with-members{
          @method-spec[
            "tostring"
            #:contract
            (a-arrow (a-id "ParseError" (xref "error" "ParseError")) "Any")
          ]
        }
      }
      @constr-spec["fun-missing-colon"]{
        @members{
          @member-spec["loc" #:contract (a-id "Loc" (xref "<global>" "Loc"))]
        }
        @with-members{
          @method-spec[
            "tostring"
            #:contract
            (a-arrow (a-id "ParseError" (xref "error" "ParseError")) "Any")
          ]
        }
      }
      @constr-spec["fun-missing-end"]{
        @members{
          @member-spec["loc" #:contract (a-id "Loc" (xref "<global>" "Loc"))]
        }
        @with-members{
          @method-spec[
            "tostring"
            #:contract
            (a-arrow (a-id "ParseError" (xref "error" "ParseError")) "Any")
          ]
        }
      }
      @constr-spec["args-missing-comma"]{
        @members{
          @member-spec["loc" #:contract (a-id "Loc" (xref "<global>" "Loc"))]
        }
        @with-members{
          @method-spec[
            "tostring"
            #:contract
            (a-arrow (a-id "ParseError" (xref "error" "ParseError")) "Any")
          ]
        }
      }
      @constr-spec["app-args-missing-comma"]{
        @members{
          @member-spec["loc" #:contract (a-id "Loc" (xref "<global>" "Loc"))]
        }
        @with-members{
          @method-spec[
            "tostring"
            #:contract
            (a-arrow (a-id "ParseError" (xref "error" "ParseError")) "Any")
          ]
        }
      }
      @constr-spec["missing-end"]{
        @members{@member-spec["loc" #:contract "Any"]}
        @with-members{}
      }
      @constr-spec["missing-comma"]{
        @members{@member-spec["loc" #:contract "Any"]}
        @with-members{}
      }
    }
    @shared{}
  }
  @section[#:tag "error_Functions"]{Functions}
  @function[
    "is-message-exception"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-no-branches-matched"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-internal-error"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-field-not-found"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-lookup-non-object"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-non-boolean-condition"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-non-boolean-op"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-generic-type-mismatch"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-outside-numeric-range"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-plus-error"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-numeric-binop-error"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-arity-mismatch"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-non-function-app"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-bad-app"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-uninitialized-id"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-module-load-failure"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-invalid-array-index"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-user-break"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-parse-error-next-token"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-parse-error-eof"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-empty-block"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-bad-block-stmt"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-bad-check-block-stmt"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-fun-missing-colon"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-fun-missing-end"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-args-missing-comma"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-app-args-missing-comma"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-missing-end"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-missing-comma"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
}