#lang scribble/base
@(require "../../scribble-api.rkt")
@docmodule["cmdline"]{
  @; Ignored type testers
  @ignore[(list "is-success" "is-arg-error")]
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
            ;; N.B. Pyret contract: (ParseParam, Number, String, String -> Number)
            
          ]
          @method-spec[
            "parse-string"
            ;; N.B. Pyret contract: (ParseParam -> Any)
            
          ]
        }
      }
      @singleton-spec["read-bool"]{
        @with-members{
          @method-spec[
            "parse"
            ;; N.B. Pyret contract: (ParseParam, Number, String, String -> Boolean)
            
          ]
          @method-spec[
            "parse-string"
            ;; N.B. Pyret contract: (ParseParam -> Any)
            
          ]
        }
      }
      @singleton-spec["read-string"]{
        @with-members{
          @method-spec[
            "parse"
            ;; N.B. Pyret contract: (ParseParam, Number, String, String -> String)
            
          ]
          @method-spec[
            "parse-string"
            ;; N.B. Pyret contract: (ParseParam -> Any)
            
          ]
        }
      }
      @constr-spec["read-custom"]{
        @members{@member-spec["name"] @member-spec["parser"]}
        @with-members{
          @method-spec[
            "parse"
            ;; N.B. Pyret contract: (ParseParam, Number, String, String -> Any)
            
          ]
          @method-spec[
            "parse-string"
            ;; N.B. Pyret contract: (ParseParam -> Any)
            
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
            ;; N.B. Pyret contract: (ParamRepeat -> Any)
            
          ]
        }
      }
      @singleton-spec["many"]{
        @with-members{
          @method-spec[
            "tostring"
            ;; N.B. Pyret contract: (ParamRepeat -> Any)
            
          ]
        }
      }
      @singleton-spec["required-once"]{
        @with-members{
          @method-spec[
            "tostring"
            ;; N.B. Pyret contract: (ParamRepeat -> Any)
            
          ]
        }
      }
      @singleton-spec["required-many"]{
        @with-members{
          @method-spec[
            "tostring"
            ;; N.B. Pyret contract: (ParamRepeat -> Any)
            
          ]
        }
      }
    }
    @shared{}
  }
  @data-spec["Param"]{
    @variants{
      @constr-spec["flag"]{
        @members{@member-spec["repeated"] @member-spec["desc"]}
        @with-members{}
      }
      @constr-spec["equals-val"]{
        @members{
          @member-spec["parser"]
          @member-spec["repeated"]
          @member-spec["desc"]
        }
        @with-members{}
      }
      @constr-spec["equals-val-default"]{
        @members{
          @member-spec["parser"]
          @member-spec["default"]
          @member-spec["short-name"]
          @member-spec["repeated"]
          @member-spec["desc"]
        }
        @with-members{}
      }
      @constr-spec["next-val"]{
        @members{
          @member-spec["parser"]
          @member-spec["repeated"]
          @member-spec["desc"]
        }
        @with-members{}
      }
      @constr-spec["next-val-default"]{
        @members{
          @member-spec["parser"]
          @member-spec["default"]
          @member-spec["short-name"]
          @member-spec["repeated"]
          @member-spec["desc"]
        }
        @with-members{}
      }
    }
    @shared{}
  }
  @data-spec["ParsedArguments"]{
    @variants{
      @constr-spec["success"]{
        @members{@member-spec["parsed"] @member-spec["unknown"]}
        @with-members{}
      }
      @constr-spec["arg-error"]{
        @members{@member-spec["message"] @member-spec["partial-results"]}
        @with-members{}
      }
    }
    @shared{}
  }
  @section[#:tag "cmdline_Functions"]{Functions}
  @function["parse-args"]
  @function["parse-cmdline"]
  @function["usage-info"]
}