#lang scribble/base
@(require "../../scribble-api.rkt")
@docmodule["error"]{
  @; Ignored type testers
  @ignore[
    (list
      "is-message-exception"
      "is-no-branches-matched"
      "is-internal-error"
      "is-field-not-found"
      "is-lookup-non-object"
      "is-non-boolean-condition"
      "is-non-boolean-op"
      "is-generic-type-mismatch"
      "is-outside-numeric-range"
      "is-plus-error"
      "is-numeric-binop-error"
      "is-arity-mismatch"
      "is-non-function-app"
      "is-bad-app"
      "is-uninitialized-id"
      "is-module-load-failure"
      "is-invalid-array-index"
      "is-user-break"
      "is-parse-error-next-token"
      "is-parse-error-eof"
      "is-empty-block"
      "is-bad-block-stmt"
      "is-bad-check-block-stmt"
      "is-fun-missing-colon"
      "is-fun-missing-end"
      "is-args-missing-comma"
      "is-app-args-missing-comma"
      "is-missing-end"
      "is-missing-comma")
  ]
  @section[#:tag "error_DataTypes"]{Data types}
  @data-spec["RuntimeError"]{
    @variants{
      @constr-spec["message-exception"]{
        @members{@member-spec["message"]}
        @with-members{
          @method-spec[
            "_output"
            ;; N.B. Pyret contract: (RuntimeError -> Any)
            
          ]
        }
      }
      @constr-spec["no-branches-matched"]{
        @members{@member-spec["loc"] @member-spec["expression"]}
        @with-members{
          @method-spec[
            "_output"
            ;; N.B. Pyret contract: (RuntimeError -> Any)
            
          ]
        }
      }
      @constr-spec["internal-error"]{
        @members{@member-spec["message"] @member-spec["info-args"]}
        @with-members{
          @method-spec[
            "_output"
            ;; N.B. Pyret contract: (RuntimeError -> Any)
            
          ]
        }
      }
      @constr-spec["field-not-found"]{
        @members{@member-spec["loc"] @member-spec["obj"] @member-spec["field"]}
        @with-members{
          @method-spec[
            "_output"
            ;; N.B. Pyret contract: (RuntimeError -> Any)
            
          ]
        }
      }
      @constr-spec["lookup-non-object"]{
        @members{
          @member-spec["loc"]
          @member-spec["non-obj"]
          @member-spec["field"]
        }
        @with-members{
          @method-spec[
            "_output"
            ;; N.B. Pyret contract: (RuntimeError -> Any)
            
          ]
        }
      }
      @constr-spec["non-boolean-condition"]{
        @members{@member-spec["loc"] @member-spec["typ"] @member-spec["value"]}
        @with-members{
          @method-spec[
            "_output"
            ;; N.B. Pyret contract: (RuntimeError -> Any)
            
          ]
        }
      }
      @constr-spec["non-boolean-op"]{
        @members{
          @member-spec["loc"]
          @member-spec["position"]
          @member-spec["typ"]
          @member-spec["value"]
        }
        @with-members{
          @method-spec[
            "_output"
            ;; N.B. Pyret contract: (RuntimeError -> Any)
            
          ]
        }
      }
      @constr-spec["generic-type-mismatch"]{
        @members{@member-spec["val"] @member-spec["typ"]}
        @with-members{
          @method-spec[
            "_output"
            ;; N.B. Pyret contract: (RuntimeError -> Any)
            
          ]
        }
      }
      @constr-spec["outside-numeric-range"]{
        @members{@member-spec["val"] @member-spec["low"] @member-spec["high"]}
        @with-members{
          @method-spec[
            "_output"
            ;; N.B. Pyret contract: (RuntimeError -> Any)
            
          ]
        }
      }
      @constr-spec["plus-error"]{
        @members{@member-spec["val1"] @member-spec["val2"]}
        @with-members{
          @method-spec[
            "_output"
            ;; N.B. Pyret contract: (RuntimeError -> Any)
            
          ]
        }
      }
      @constr-spec["numeric-binop-error"]{
        @members{
          @member-spec["val1"]
          @member-spec["val2"]
          @member-spec["opname"]
          @member-spec["methodname"]
        }
        @with-members{
          @method-spec[
            "_output"
            ;; N.B. Pyret contract: (RuntimeError -> Any)
            
          ]
        }
      }
      @constr-spec["arity-mismatch"]{
        @members{
          @member-spec["fun-loc"]
          @member-spec["expected-arity"]
          @member-spec["args"]
        }
        @with-members{
          @method-spec[
            "_output"
            ;; N.B. Pyret contract: (RuntimeError -> Any)
            
          ]
        }
      }
      @constr-spec["non-function-app"]{
        @members{
          @member-spec["loc"]
          @member-spec["non-fun-val"]
          @member-spec["args"]
        }
        @with-members{
          @method-spec[
            "_output"
            ;; N.B. Pyret contract: (RuntimeError -> Any)
            
          ]
        }
      }
      @constr-spec["bad-app"]{
        @members{
          @member-spec["loc"]
          @member-spec["fun-name"]
          @member-spec["message"]
          @member-spec["arg-position"]
          @member-spec["arg-val"]
        }
        @with-members{}
      }
      @constr-spec["uninitialized-id"]{
        @members{@member-spec["loc"] @member-spec["name"]}
        @with-members{
          @method-spec[
            "_output"
            ;; N.B. Pyret contract: (RuntimeError -> Any)
            
          ]
        }
      }
      @constr-spec["module-load-failure"]{
        @members{@member-spec["names"]}
        @with-members{
          @method-spec[
            "_output"
            ;; N.B. Pyret contract: (RuntimeError -> Any)
            
          ]
        }
      }
      @constr-spec["invalid-array-index"]{
        @members{
          @member-spec["method-name"]
          @member-spec["array"]
          @member-spec["index"]
          @member-spec["reason"]
        }
        @with-members{
          @method-spec[
            "_output"
            ;; N.B. Pyret contract: (RuntimeError -> Any)
            
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
        @members{@member-spec["loc"] @member-spec["next-token"]}
        @with-members{
          @method-spec[
            "_output"
            ;; N.B. Pyret contract: (ParseError -> Any)
            
          ]
        }
      }
      @constr-spec["parse-error-eof"]{
        @members{@member-spec["loc"]}
        @with-members{
          @method-spec[
            "_output"
            ;; N.B. Pyret contract: (ParseError -> Any)
            
          ]
        }
      }
      @constr-spec["empty-block"]{
        @members{@member-spec["loc"]}
        @with-members{
          @method-spec[
            "_output"
            ;; N.B. Pyret contract: (ParseError -> Any)
            
          ]
        }
      }
      @constr-spec["bad-block-stmt"]{
        @members{@member-spec["loc"]}
        @with-members{
          @method-spec[
            "_output"
            ;; N.B. Pyret contract: (ParseError -> Any)
            
          ]
        }
      }
      @constr-spec["bad-check-block-stmt"]{
        @members{@member-spec["loc"]}
        @with-members{
          @method-spec[
            "_output"
            ;; N.B. Pyret contract: (ParseError -> Any)
            
          ]
        }
      }
      @constr-spec["fun-missing-colon"]{
        @members{@member-spec["loc"]}
        @with-members{
          @method-spec[
            "_output"
            ;; N.B. Pyret contract: (ParseError -> Any)
            
          ]
        }
      }
      @constr-spec["fun-missing-end"]{
        @members{@member-spec["loc"]}
        @with-members{
          @method-spec[
            "_output"
            ;; N.B. Pyret contract: (ParseError -> Any)
            
          ]
        }
      }
      @constr-spec["args-missing-comma"]{
        @members{@member-spec["loc"]}
        @with-members{
          @method-spec[
            "_output"
            ;; N.B. Pyret contract: (ParseError -> Any)
            
          ]
        }
      }
      @constr-spec["app-args-missing-comma"]{
        @members{@member-spec["loc"]}
        @with-members{
          @method-spec[
            "_output"
            ;; N.B. Pyret contract: (ParseError -> Any)
            
          ]
        }
      }
      @constr-spec["missing-end"]{@members{@member-spec["loc"]} @with-members{}}
      @constr-spec["missing-comma"]{
        @members{@member-spec["loc"]}
        @with-members{}
      }
    }
    @shared{}
  }
}
