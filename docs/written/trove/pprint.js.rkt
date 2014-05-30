#lang scribble/base
@(require "../../scribble-api.rkt")
@docmodule["pprint"]{
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
  @section[#:tag "pprint_ReExports"]{Re-exported values}
  @section[#:tag "pprint_DataTypes"]{Data types}
  @data-spec["PPrintDoc"]{
    @variants{
      @constr-spec["mt-doc"]{
        @members{
          @member-spec[
            "flat-width"
            #:contract (a-id "Number" (xref "<global>" "Number"))
          ]
          @member-spec[
            "has-hardline"
            #:contract (a-id "Bool" (xref "<global>" "Bool"))
          ]
        }
        @with-members{}
      }
      @constr-spec["str"]{
        @members{
          @member-spec[
            "s"
            #:contract (a-id "String" (xref "<global>" "String"))
          ]
          @member-spec[
            "flat-width"
            #:contract (a-id "Number" (xref "<global>" "Number"))
          ]
          @member-spec[
            "has-hardline"
            #:contract (a-id "Bool" (xref "<global>" "Bool"))
          ]
        }
        @with-members{}
      }
      @constr-spec["hardline"]{
        @members{
          @member-spec[
            "flat-width"
            #:contract (a-id "Number" (xref "<global>" "Number"))
          ]
          @member-spec[
            "has-hardline"
            #:contract (a-id "Bool" (xref "<global>" "Bool"))
          ]
        }
        @with-members{}
      }
      @constr-spec["blank"]{
        @members{
          @member-spec[
            "n"
            #:contract (a-id "Number" (xref "<global>" "Number"))
          ]
          @member-spec[
            "flat-width"
            #:contract (a-id "Number" (xref "<global>" "Number"))
          ]
          @member-spec[
            "has-hardline"
            #:contract (a-id "Bool" (xref "<global>" "Bool"))
          ]
        }
        @with-members{}
      }
      @constr-spec["concat"]{
        @members{
          @member-spec[
            "fst"
            #:contract (a-id "PPrintDoc" (xref "pprint" "PPrintDoc"))
          ]
          @member-spec[
            "snd"
            #:contract (a-id "PPrintDoc" (xref "pprint" "PPrintDoc"))
          ]
          @member-spec[
            "flat-width"
            #:contract (a-id "Number" (xref "<global>" "Number"))
          ]
          @member-spec[
            "has-hardline"
            #:contract (a-id "Bool" (xref "<global>" "Bool"))
          ]
        }
        @with-members{}
      }
      @constr-spec["nest"]{
        @members{
          @member-spec[
            "indent"
            #:contract (a-id "Number" (xref "<global>" "Number"))
          ]
          @member-spec[
            "d"
            #:contract (a-id "PPrintDoc" (xref "pprint" "PPrintDoc"))
          ]
          @member-spec[
            "flat-width"
            #:contract (a-id "Number" (xref "<global>" "Number"))
          ]
          @member-spec[
            "has-hardline"
            #:contract (a-id "Bool" (xref "<global>" "Bool"))
          ]
        }
        @with-members{}
      }
      @constr-spec["if-flat"]{
        @members{
          @member-spec[
            "flat"
            #:contract (a-id "PPrintDoc" (xref "pprint" "PPrintDoc"))
          ]
          @member-spec[
            "vert"
            #:contract (a-id "PPrintDoc" (xref "pprint" "PPrintDoc"))
          ]
          @member-spec[
            "flat-width"
            #:contract (a-id "Number" (xref "<global>" "Number"))
          ]
          @member-spec[
            "has-hardline"
            #:contract (a-id "Bool" (xref "<global>" "Bool"))
          ]
        }
        @with-members{}
      }
      @constr-spec["align"]{
        @members{
          @member-spec[
            "d"
            #:contract (a-id "PPrintDoc" (xref "pprint" "PPrintDoc"))
          ]
          @member-spec[
            "flat-width"
            #:contract (a-id "Number" (xref "<global>" "Number"))
          ]
          @member-spec[
            "has-hardline"
            #:contract (a-id "Bool" (xref "<global>" "Bool"))
          ]
        }
        @with-members{}
      }
      @constr-spec["align-spaces"]{
        @members{
          @member-spec[
            "n"
            #:contract (a-id "Number" (xref "<global>" "Number"))
          ]
          @member-spec[
            "flat-width"
            #:contract (a-id "Number" (xref "<global>" "Number"))
          ]
          @member-spec[
            "has-hardline"
            #:contract (a-id "Bool" (xref "<global>" "Bool"))
          ]
        }
        @with-members{}
      }
      @constr-spec["group"]{
        @members{
          @member-spec[
            "d"
            #:contract (a-id "PPrintDoc" (xref "pprint" "PPrintDoc"))
          ]
          @member-spec[
            "flat-width"
            #:contract (a-id "Number" (xref "<global>" "Number"))
          ]
          @member-spec[
            "has-hardline"
            #:contract (a-id "Bool" (xref "<global>" "Bool"))
          ]
        }
        @with-members{}
      }
    }
    @shared{
      @method-spec[
        "_plus"
        #:contract
        (a-arrow (a-id "PPrintDoc" (xref "pprint" "PPrintDoc")) "Any" "Any")
      ]
      @method-spec[
        "tostring"
        #:contract
        (a-arrow (a-id "PPrintDoc" (xref "pprint" "PPrintDoc")) "Any")
      ]
      @method-spec[
        "pretty"
        #:contract
        (a-arrow (a-id "PPrintDoc" (xref "pprint" "PPrintDoc")) "Any" "Any")
      ]
    }
  }
  @section[#:tag "pprint_Functions"]{Functions}
  @function[
    "is-mt-doc"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function["str" #:contract (a-arrow "Any" "Any")]
  @function[
    "number"
    #:contract (a-arrow (a-id "Number" (xref "<global>" "Number")) "Any")
  ]
  @function["blank" #:contract (a-arrow "Any" "Any")]
  @function["sbreak" #:contract (a-arrow "Any" "Any")]
  @function["concat" #:contract (a-arrow "Any" "Any" "Any")]
  @function["nest" #:contract (a-arrow "Any" "Any" "Any")]
  @function["if-flat" #:contract (a-arrow "Any" "Any" "Any")]
  @function["group" #:contract (a-arrow "Any" "Any")]
  @function["flow" #:contract (a-arrow "Any" "Any")]
  @function["flow-map" #:contract (a-arrow "Any" "Any" "Any" "Any")]
  @function["vert" #:contract (a-arrow "Any" "Any")]
  @function["parens" #:contract (a-arrow "Any" "Any")]
  @function["braces" #:contract (a-arrow "Any" "Any")]
  @function["brackets" #:contract (a-arrow "Any" "Any")]
  @function["dquote" #:contract (a-arrow "Any" "Any")]
  @function["squote" #:contract (a-arrow "Any" "Any")]
  @function["align" #:contract (a-arrow "Any" "Any")]
  @function["hang" #:contract (a-arrow "Any" "Any" "Any")]
  @function["prefix" #:contract (a-arrow "Any" "Any" "Any" "Any" "Any")]
  @function[
    "infix"
    #:contract
    (a-arrow
      (a-id "Number" (xref "<global>" "Number"))
      (a-id "Number" (xref "<global>" "Number"))
      (a-id "PPrintDoc" (xref "pprint" "PPrintDoc"))
      (a-id "PPrintDoc" (xref "pprint" "PPrintDoc"))
      (a-id "PPrintDoc" (xref "pprint" "PPrintDoc"))
      "Any")
  ]
  @function[
    "infix-break"
    #:contract
    (a-arrow
      (a-id "Number" (xref "<global>" "Number"))
      (a-id "Number" (xref "<global>" "Number"))
      (a-id "PPrintDoc" (xref "pprint" "PPrintDoc"))
      (a-id "PPrintDoc" (xref "pprint" "PPrintDoc"))
      (a-id "PPrintDoc" (xref "pprint" "PPrintDoc"))
      "Any")
  ]
  @function[
    "separate"
    #:contract
    (a-arrow
      (a-id "PPrintDoc" (xref "pprint" "PPrintDoc"))
      (a-compound (a-dot "lists" "List") (xref "lists" "List"))
      "Any")
  ]
  @function[
    "surround"
    #:contract
    (a-arrow
      (a-id "Number" (xref "<global>" "Number"))
      (a-id "Number" (xref "<global>" "Number"))
      (a-id "PPrintDoc" (xref "pprint" "PPrintDoc"))
      (a-id "PPrintDoc" (xref "pprint" "PPrintDoc"))
      (a-id "PPrintDoc" (xref "pprint" "PPrintDoc"))
      "Any")
  ]
  @function[
    "soft-surround"
    #:contract
    (a-arrow
      (a-id "Number" (xref "<global>" "Number"))
      (a-id "Number" (xref "<global>" "Number"))
      (a-id "PPrintDoc" (xref "pprint" "PPrintDoc"))
      (a-id "PPrintDoc" (xref "pprint" "PPrintDoc"))
      (a-id "PPrintDoc" (xref "pprint" "PPrintDoc"))
      "Any")
  ]
  @function[
    "surround-separate"
    #:contract
    (a-arrow
      (a-id "Number" (xref "<global>" "Number"))
      (a-id "Number" (xref "<global>" "Number"))
      (a-id "PPrintDoc" (xref "pprint" "PPrintDoc"))
      (a-id "PPrintDoc" (xref "pprint" "PPrintDoc"))
      (a-id "PPrintDoc" (xref "pprint" "PPrintDoc"))
      (a-id "PPrintDoc" (xref "pprint" "PPrintDoc"))
      (a-compound (a-dot "lists" "List") (xref "lists" "List"))
      "Any")
  ]
  @function[
    "label-align-surround"
    #:contract (a-arrow "Any" "Any" "Any" "Any" "Any" "Any")
  ]
}