#lang scribble/base
@(require "../../scribble-api.rkt")
@docmodule["checker"]{
  @section[#:tag "checker_ReExports"]{Re-exported values}
  @re-export["Loc" (from (xref "srcloc" "Srcloc"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["srcloc" "Srcloc"]}
  }
  @section[#:tag "checker_DataTypes"]{Data types}
  @data-spec["CheckBlockResult"]{
    @variants{
      @constr-spec["check-block-result"]{
        @members{
          @member-spec[
            "name"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec["loc" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "test-results"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "TestResult" (xref "checker" "TestResult")))
          ]
        }
        @with-members{}
      }
    }
    @shared{}
  }
  @data-spec["TestResult"]{
    @variants{
      @constr-spec["success"]{
        @members{
          @member-spec["loc" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "code"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
        }
        @with-members{}
      }
      @constr-spec["failure-not-equal"]{
        @members{
          @member-spec["loc" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "code"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec["left" #:contract "Any"]
          @member-spec["right" #:contract "Any"]
        }
        @with-members{
          @method-spec[
            "reason"
            #:contract
            (a-arrow (a-id "TestResult" (xref "checker" "TestResult")) "Any")
          ]
        }
      }
      @constr-spec["failure-not-satisfied"]{
        @members{
          @member-spec["loc" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "code"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec["val" #:contract "Any"]
          @member-spec["pred" #:contract "Any"]
        }
        @with-members{
          @method-spec[
            "reason"
            #:contract
            (a-arrow (a-id "TestResult" (xref "checker" "TestResult")) "Any")
          ]
        }
      }
      @constr-spec["failure-wrong-exn"]{
        @members{
          @member-spec["loc" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "code"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec["exn-expected" #:contract "Any"]
          @member-spec["actual-exn" #:contract "Any"]
        }
        @with-members{
          @method-spec[
            "reason"
            #:contract
            (a-arrow (a-id "TestResult" (xref "checker" "TestResult")) "Any")
          ]
        }
      }
      @constr-spec["failure-no-exn"]{
        @members{
          @member-spec["loc" #:contract (a-id "Loc" (xref "srcloc" "Srcloc"))]
          @member-spec[
            "code"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec["exn-expected" #:contract "Any"]
        }
        @with-members{
          @method-spec[
            "reason"
            #:contract
            (a-arrow (a-id "TestResult" (xref "checker" "TestResult")) "Any")
          ]
        }
      }
    }
    @shared{}
  }
  @section[#:tag "checker_Functions"]{Functions}
  @function[
    "is-check-block-result"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-success"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-failure-not-equal"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-failure-not-satisfied"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-failure-wrong-exn"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-failure-no-exn"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "make-check-context"
    #:contract
    (a-arrow
      (a-id "String" (xref "<global>" "String"))
      (a-id "Boolean" (xref "<global>" "Boolean"))
      "Any")
  ]
  @function[
    "results-summary"
    #:contract
    (a-arrow
      (a-app
        (a-id "List" (xref "lists" "List"))
        (a-id "CheckBlockResult" (xref "checker" "CheckBlockResult")))
      "Any")
  ]
  @function[
    "render-check-results"
    #:contract
    (a-arrow
      (a-app
        (a-id "List" (xref "lists" "List"))
        (a-id "CheckBlockResult" (xref "checker" "CheckBlockResult")))
      "Any")
  ]
}