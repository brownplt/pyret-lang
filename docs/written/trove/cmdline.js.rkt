#lang scribble/base
@(require "../../scribble-api.rkt")
@docmodule["cmdline"]{
  @; Unknown: PLEASE DOCUMENT
  @ignore[(list "file-name" "args")]
  @section[#:tag "cmdline_ReExports"]{Re-exported values}
  @re-export["left" (from (xref "either" "left"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["either" "left"]}
  }
  @re-export["right" (from (xref "either" "right"))]{
    @; N.B. need para here to keep xref inline with text
    @para{See @xref["either" "right"]}
  }
  @section[#:tag "cmdline_DataTypes"]{Data types}
  @data-spec["ParseParam"]{
    @variants{
      @singleton-spec["read-number"]{
        @with-members{
          @method-spec[
            "parse"
            #:contract
            (a-arrow
              (a-id "ParseParam" (xref "cmdline" "ParseParam"))
              (a-id "Number" (xref "<global>" "Number"))
              (a-id "String" (xref "<global>" "String"))
              (a-id "String" (xref "<global>" "String"))
              (a-id "Number" (xref "<global>" "Number")))
          ]
          @method-spec[
            "parse-string"
            #:contract
            (a-arrow (a-id "ParseParam" (xref "cmdline" "ParseParam")) "Any")
          ]
        }
      }
      @singleton-spec["read-bool"]{
        @with-members{
          @method-spec[
            "parse"
            #:contract
            (a-arrow
              (a-id "ParseParam" (xref "cmdline" "ParseParam"))
              (a-id "Number" (xref "<global>" "Number"))
              (a-id "String" (xref "<global>" "String"))
              (a-id "String" (xref "<global>" "String"))
              (a-id "Bool" (xref "<global>" "Bool")))
          ]
          @method-spec[
            "parse-string"
            #:contract
            (a-arrow (a-id "ParseParam" (xref "cmdline" "ParseParam")) "Any")
          ]
        }
      }
      @singleton-spec["read-string"]{
        @with-members{
          @method-spec[
            "parse"
            #:contract
            (a-arrow
              (a-id "ParseParam" (xref "cmdline" "ParseParam"))
              (a-id "Number" (xref "<global>" "Number"))
              (a-id "String" (xref "<global>" "String"))
              (a-id "String" (xref "<global>" "String"))
              (a-id "String" (xref "<global>" "String")))
          ]
          @method-spec[
            "parse-string"
            #:contract
            (a-arrow (a-id "ParseParam" (xref "cmdline" "ParseParam")) "Any")
          ]
        }
      }
      @constr-spec["read-custom"]{
        @members{
          @member-spec[
            "name"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec[
            "parser"
            #:contract (a-id "Function" (xref "<global>" "Function"))
          ]
        }
        @with-members{
          @method-spec[
            "parse"
            #:contract
            (a-arrow
              (a-id "ParseParam" (xref "cmdline" "ParseParam"))
              (a-id "Number" (xref "<global>" "Number"))
              (a-id "String" (xref "<global>" "String"))
              (a-id "String" (xref "<global>" "String"))
              "Any")
          ]
          @method-spec[
            "parse-string"
            #:contract
            (a-arrow (a-id "ParseParam" (xref "cmdline" "ParseParam")) "Any")
          ]
        }
      }
    }
    @shared{}
  }
  @data-spec["ParamRepeat"]{
    @variants{
      @singleton-spec["once"]{
        @with-members{
          @method-spec[
            "tostring"
            #:contract
            (a-arrow (a-id "ParamRepeat" (xref "cmdline" "ParamRepeat")) "Any")
          ]
        }
      }
      @singleton-spec["many"]{
        @with-members{
          @method-spec[
            "tostring"
            #:contract
            (a-arrow (a-id "ParamRepeat" (xref "cmdline" "ParamRepeat")) "Any")
          ]
        }
      }
      @singleton-spec["required-once"]{
        @with-members{
          @method-spec[
            "tostring"
            #:contract
            (a-arrow (a-id "ParamRepeat" (xref "cmdline" "ParamRepeat")) "Any")
          ]
        }
      }
      @singleton-spec["required-many"]{
        @with-members{
          @method-spec[
            "tostring"
            #:contract
            (a-arrow (a-id "ParamRepeat" (xref "cmdline" "ParamRepeat")) "Any")
          ]
        }
      }
    }
    @shared{}
  }
  @data-spec["Param"]{
    @variants{
      @constr-spec["flag"]{
        @members{
          @member-spec[
            "repeated"
            #:contract (a-id "ParamRepeat" (xref "cmdline" "ParamRepeat"))
          ]
          @member-spec[
            "desc"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
        }
        @with-members{}
      }
      @constr-spec["equals-val"]{
        @members{
          @member-spec[
            "parser"
            #:contract (a-id "ParseParam" (xref "cmdline" "ParseParam"))
          ]
          @member-spec[
            "repeated"
            #:contract (a-id "ParamRepeat" (xref "cmdline" "ParamRepeat"))
          ]
          @member-spec[
            "desc"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
        }
        @with-members{}
      }
      @constr-spec["equals-val-default"]{
        @members{
          @member-spec[
            "parser"
            #:contract (a-id "ParseParam" (xref "cmdline" "ParseParam"))
          ]
          @member-spec["default" #:contract "Any"]
          @member-spec[
            "short-name"
            #:contract
            (a-app
              (a-id "Option" (xref "option" "Option"))
              (a-id "String" (xref "<global>" "String")))
          ]
          @member-spec[
            "repeated"
            #:contract (a-id "ParamRepeat" (xref "cmdline" "ParamRepeat"))
          ]
          @member-spec[
            "desc"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
        }
        @with-members{}
      }
      @constr-spec["next-val"]{
        @members{
          @member-spec[
            "parser"
            #:contract (a-id "ParseParam" (xref "cmdline" "ParseParam"))
          ]
          @member-spec[
            "repeated"
            #:contract (a-id "ParamRepeat" (xref "cmdline" "ParamRepeat"))
          ]
          @member-spec[
            "desc"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
        }
        @with-members{}
      }
      @constr-spec["next-val-default"]{
        @members{
          @member-spec[
            "parser"
            #:contract (a-id "ParseParam" (xref "cmdline" "ParseParam"))
          ]
          @member-spec["default" #:contract "Any"]
          @member-spec[
            "short-name"
            #:contract
            (a-app
              (a-id "Option" (xref "option" "Option"))
              (a-id "String" (xref "<global>" "String")))
          ]
          @member-spec[
            "repeated"
            #:contract (a-id "ParamRepeat" (xref "cmdline" "ParamRepeat"))
          ]
          @member-spec[
            "desc"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
        }
        @with-members{}
      }
    }
    @shared{}
  }
  @data-spec["ParsedArguments"]{
    @variants{
      @constr-spec["success"]{
        @members{
          @member-spec[
            "parsed"
            #:contract
            (a-compound
              (a-dot "D" "StringDict")
              (xref "string-dict" "StringDict"))
          ]
          @member-spec[
            "unknown"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-id "String" (xref "<global>" "String")))
          ]
        }
        @with-members{}
      }
      @constr-spec["arg-error"]{
        @members{
          @member-spec[
            "message"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec[
            "partial-results"
            #:contract
            (a-id "ParsedArguments" (xref "cmdline" "ParsedArguments"))
          ]
        }
        @with-members{}
      }
    }
    @shared{}
  }
  @section[#:tag "cmdline_Functions"]{Functions}
  @function[
    "parse-args"
    #:contract
    (a-arrow
      "Any"
      (a-app
        (a-id "List" (xref "lists" "List"))
        (a-id "String" (xref "<global>" "String")))
      (a-id "ParsedArguments" (xref "cmdline" "ParsedArguments")))
  ]
  @function["parse-cmdline" #:contract (a-arrow "Any" "Any")]
  @function[
    "usage-info"
    #:contract
    (a-arrow
      "Any"
      (a-app
        (a-id "List" (xref "lists" "List"))
        (a-id "String" (xref "<global>" "String"))))
  ]
  @function[
    "is-success"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-arg-error"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
}