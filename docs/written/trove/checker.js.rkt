#lang scribble/base
@(require "../../scribble-api.rkt")
@docmodule["checker"]{
  @; Ignored type testers
  @ignore[
    (list
      "is-check-block-result"
      "is-success"
      "is-failure-not-equal"
      "is-failure-not-satisfied"
      "is-failure-wrong-exn"
      "is-failure-no-exn")
  ]
  @section[#:tag "checker_DataTypes"]{Data types}
  @data-spec["CheckBlockResult"]{
    @variants{
      @constr-spec["check-block-result"]{
        @members{
          @member-spec["name"]
          @member-spec["loc"]
          @member-spec["test-results"]
        }
        @with-members{}
      }
    }
    @shared{}
  }
  
  @data-spec["TestResult"]{
    @variants{
      @constr-spec["success"]{
        @members{@member-spec["loc"] @member-spec["code"]}
        @with-members{}
      }
      @constr-spec["failure-not-equal"]{
        @members{
          @member-spec["loc"]
          @member-spec["code"]
          @member-spec["left"]
          @member-spec["right"]
        }
        @with-members{
          @method-spec[
            "reason"
            ;; N.B. Pyret contract: (TestResult -> Any)
            
          ]
        }
      }
      @constr-spec["failure-not-satisfied"]{
        @members{
          @member-spec["loc"]
          @member-spec["code"]
          @member-spec["val"]
          @member-spec["pred"]
        }
        @with-members{
          @method-spec[
            "reason"
            ;; N.B. Pyret contract: (TestResult -> Any)
            
          ]
        }
      }
      @constr-spec["failure-wrong-exn"]{
        @members{
          @member-spec["loc"]
          @member-spec["code"]
          @member-spec["exn-expected"]
          @member-spec["actual-exn"]
        }
        @with-members{
          @method-spec[
            "reason"
            ;; N.B. Pyret contract: (TestResult -> Any)
            
          ]
        }
      }
      @constr-spec["failure-no-exn"]{
        @members{
          @member-spec["loc"]
          @member-spec["code"]
          @member-spec["exn-expected"]
        }
        @with-members{
          @method-spec[
            "reason"
            ;; N.B. Pyret contract: (TestResult -> Any)
            
          ]
        }
      }
    }
    @shared{}
  }
  
  @section[#:tag "checker_Functions"]{Functions}
  @function["make-check-context"]
  @function["results-summary"]
  @function["render-check-results"]
}