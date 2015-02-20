#lang scribble/base
@(require "../../scribble-api.rkt")
@docmodule["pprint"]{
  @; Ignored type testers
  @ignore[(list "is-mt-doc")]
  @; Unknown: PLEASE DOCUMENT
  @ignore[
    (list
      "mt-doc"
      "hardline"
      "lparen"
      "rparen"
      "lbrace"
      "rbrace"
      "lbrack"
      "rbrack"
      "langle"
      "rangle"
      "comma"
      "commabreak")
  ]
  @section[#:tag "pprint_DataTypes"]{Data types}
  @data-spec["PPrintDoc"]{
    @variants{
      @constr-spec["mt-doc"]{
        @members{@member-spec["flat-width"] @member-spec["has-hardline"]}
        @with-members{}
      }
      @constr-spec["str"]{
        @members{
          @member-spec["s"]
          @member-spec["flat-width"]
          @member-spec["has-hardline"]
        }
        @with-members{}
      }
      @constr-spec["hardline"]{
        @members{@member-spec["flat-width"] @member-spec["has-hardline"]}
        @with-members{}
      }
      @constr-spec["blank"]{
        @members{
          @member-spec["n"]
          @member-spec["flat-width"]
          @member-spec["has-hardline"]
        }
        @with-members{}
      }
      @constr-spec["concat"]{
        @members{
          @member-spec["fst"]
          @member-spec["snd"]
          @member-spec["flat-width"]
          @member-spec["has-hardline"]
        }
        @with-members{}
      }
      @constr-spec["nest"]{
        @members{
          @member-spec["indent"]
          @member-spec["d"]
          @member-spec["flat-width"]
          @member-spec["has-hardline"]
        }
        @with-members{}
      }
      @constr-spec["if-flat"]{
        @members{
          @member-spec["flat"]
          @member-spec["vert"]
          @member-spec["flat-width"]
          @member-spec["has-hardline"]
        }
        @with-members{}
      }
      @constr-spec["align"]{
        @members{
          @member-spec["d"]
          @member-spec["flat-width"]
          @member-spec["has-hardline"]
        }
        @with-members{}
      }
      @constr-spec["align-spaces"]{
        @members{
          @member-spec["n"]
          @member-spec["flat-width"]
          @member-spec["has-hardline"]
        }
        @with-members{}
      }
      @constr-spec["group"]{
        @members{
          @member-spec["d"]
          @member-spec["flat-width"]
          @member-spec["has-hardline"]
        }
        @with-members{}
      }
    }
    @shared{
      @method-spec[
        "_plus"
        ;; N.B. Pyret contract: (PPrintDoc, Any -> Any)
        
      ]
      @method-spec[
        "_tostring"
        ;; N.B. Pyret contract: (PPrintDoc -> Any)
        
      ]
      @method-spec[
        "pretty"
        ;; N.B. Pyret contract: (PPrintDoc, Any -> Any)
        
      ]
    }
  }
  @section[#:tag "pprint_Functions"]{Functions}
  @function["str"]
  @function["number"]
  @function["blank"]
  @function["sbreak"]
  @function["concat"]
  @function["nest"]
  @function["if-flat"]
  @function["group"]
  @function["flow"]
  @function["flow-map"]
  @function["vert"]
  @function["parens"]
  @function["braces"]
  @function["brackets"]
  @function["dquote"]
  @function["squote"]
  @function["align"]
  @function["hang"]
  @function["prefix"]
  @function["infix"]
  @function["infix-break"]
  @function["separate"]
  @function["surround"]
  @function["soft-surround"]
  @function["surround-separate"]
  @function["label-align-surround"]
}
