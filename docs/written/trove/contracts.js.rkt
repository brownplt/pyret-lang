#lang scribble/base
@(require "../../scribble-api.rkt")
@docmodule["contracts"]{
  @; Ignored type testers
  @ignore[
    (list
      "is-ok"
      "is-fail"
      "is-fail-arg"
      "is-field-failure"
      "is-missing-field"
      "is-type-mismatch"
      "is-predicate-failure"
      "is-record-fields-fail"
      "is-dot-ann-not-present")
  ]
  @section[#:tag "contracts_DataTypes"]{Data types}
  @data-spec["ContractResult"]{
    @variants{
      @singleton-spec["ok"]{@with-members{}}
      @constr-spec["fail"]{
        @members{@member-spec["loc"] @member-spec["reason"]}
        @with-members{
          @method-spec[
            "tostring"
            ;; N.B. Pyret contract: (ContractResult -> Any)
            
          ]
        }
      }
      @constr-spec["fail-arg"]{
        @members{@member-spec["loc"] @member-spec["reason"]}
        @with-members{
          @method-spec[
            "tostring"
            ;; N.B. Pyret contract: (ContractResult -> Any)
            
          ]
        }
      }
    }
    @shared{}
  }
  
  @data-spec["FieldFailure"]{
    @variants{
      @constr-spec["field-failure"]{
        @members{
          @member-spec["loc"]
          @member-spec["field"]
          @member-spec["reason"]
        }
        @with-members{
          @method-spec[
            "tostring"
            ;; N.B. Pyret contract: (FieldFailure -> Any)
            
          ]
        }
      }
      @constr-spec["missing-field"]{
        @members{@member-spec["loc"] @member-spec["field"]}
        @with-members{
          @method-spec[
            "tostring"
            ;; N.B. Pyret contract: (FieldFailure -> Any)
            
          ]
        }
      }
    }
    @shared{}
  }
  
  @data-spec["FailureReason"]{
    @variants{
      @constr-spec["type-mismatch"]{
        @members{@member-spec["val"] @member-spec["name"]}
        @with-members{
          @method-spec[
            "tostring"
            ;; N.B. Pyret contract: (FailureReason -> Any)
            
          ]
        }
      }
      @constr-spec["predicate-failure"]{
        @members{@member-spec["val"] @member-spec["pred-name"]}
        @with-members{
          @method-spec[
            "tostring"
            ;; N.B. Pyret contract: (FailureReason -> Any)
            
          ]
        }
      }
      @constr-spec["record-fields-fail"]{
        @members{@member-spec["val"] @member-spec["field-failures"]}
        @with-members{
          @method-spec[
            "tostring"
            ;; N.B. Pyret contract: (FailureReason -> Any)
            
          ]
        }
      }
      @constr-spec["dot-ann-not-present"]{
        @members{@member-spec["name"] @member-spec["field"]}
        @with-members{
          @method-spec[
            "tostring"
            ;; N.B. Pyret contract: (FailureReason -> Any)
            
          ]
        }
      }
    }
    @shared{}
  }
}