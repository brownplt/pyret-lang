#lang scribble/base
@(require "../../scribble-api.rkt")

@(append-gen-docs
'(module
  "pprint"
  (path "src/arr/trove/pprint.arr")
  (data-spec
    (name "PPrintDoc")
    (type-vars ())
    (variants
      ("mt-doc"
      "str"
      "hardline"
      "blank"
      "concat"
      "nest"
      "if-flat"
      "align"
      "align-spaces"
      "group"))
    (shared
      ((method-spec
        (name "_plus")
        (arity 2)
        (params ())
        (args ("self" "other"))
        (return "Any")
        (contract
          (a-arrow (a-id "PPrintDoc" (xref "pprint" "PPrintDoc")) "Any" "Any")))
      (method-spec
        (name "_output")
        (arity 1)
        (params ())
        (args ("self"))
        (return "Any")
        (contract
          (a-arrow (a-id "PPrintDoc" (xref "pprint" "PPrintDoc")) "Any")))
      (method-spec
        (name "pretty")
        (arity 2)
        (params ())
        (args ("self" "width"))
        (return "Any")
        (contract
          (a-arrow (a-id "PPrintDoc" (xref "pprint" "PPrintDoc")) "Any" "Any"))))))
  (unknown-item
    (name "mt-doc")
    ;; ~mt-doc18(0, false)
    )
  (fun-spec
    (name "is-mt-doc")
    (arity 1)
    (params [list: ])
    (args ("val"))
    (return (a-id "Boolean" (xref "<global>" "Boolean")))
    (contract (a-arrow "Any" (a-id "Boolean" (xref "<global>" "Boolean"))))
    (doc "Checks whether the provided argument is in fact a mt-doc"))
  (fun-spec
    (name "str")
    (arity 1)
    (params [list: ])
    (args ("s"))
    (return "Any")
    (contract (a-arrow "Any" "Any")))
  (fun-spec
    (name "number")
    (arity 1)
    (params [list: ])
    (args ("n"))
    (return "Any")
    (contract (a-arrow (a-id "Number" (xref "<global>" "Number")) "Any")))
  (unknown-item
    (name "hardline")
    ;; ~hardline22(0, true)
    )
  (fun-spec
    (name "blank")
    (arity 1)
    (params [list: ])
    (args ("n"))
    (return "Any")
    (contract (a-arrow "Any" "Any")))
  (fun-spec
    (name "sbreak")
    (arity 1)
    (params [list: ])
    (args ("n"))
    (return "Any")
    (contract (a-arrow "Any" "Any")))
  (fun-spec
    (name "concat")
    (arity 2)
    (params [list: ])
    (args ("fst" "snd"))
    (return "Any")
    (contract (a-arrow "Any" "Any" "Any")))
  (fun-spec
    (name "nest")
    (arity 2)
    (params [list: ])
    (args ("n" "d"))
    (return "Any")
    (contract (a-arrow "Any" "Any" "Any")))
  (fun-spec
    (name "if-flat")
    (arity 2)
    (params [list: ])
    (args ("flat" "vert"))
    (return "Any")
    (contract (a-arrow "Any" "Any" "Any")))
  (fun-spec
    (name "group")
    (arity 1)
    (params [list: ])
    (args ("d"))
    (return "Any")
    (contract (a-arrow "Any" "Any")))
  (fun-spec
    (name "flow")
    (arity 1)
    (params [list: ])
    (args ("items"))
    (return "Any")
    (contract (a-arrow "Any" "Any")))
  (fun-spec
    (name "flow-map")
    (arity 3)
    (params [list: ])
    (args ("sep" "f" "items"))
    (return "Any")
    (contract (a-arrow "Any" "Any" "Any" "Any")))
  (fun-spec
    (name "vert")
    (arity 1)
    (params [list: ])
    (args ("items"))
    (return "Any")
    (contract (a-arrow "Any" "Any")))
  (fun-spec
    (name "parens")
    (arity 1)
    (params [list: ])
    (args ("d"))
    (return "Any")
    (contract (a-arrow "Any" "Any")))
  (fun-spec
    (name "braces")
    (arity 1)
    (params [list: ])
    (args ("d"))
    (return "Any")
    (contract (a-arrow "Any" "Any")))
  (fun-spec
    (name "brackets")
    (arity 1)
    (params [list: ])
    (args ("d"))
    (return "Any")
    (contract (a-arrow "Any" "Any")))
  (fun-spec
    (name "dquote")
    (arity 1)
    (params [list: ])
    (args ("s"))
    (return "Any")
    (contract (a-arrow "Any" "Any")))
  (fun-spec
    (name "squote")
    (arity 1)
    (params [list: ])
    (args ("s"))
    (return "Any")
    (contract (a-arrow "Any" "Any")))
  (fun-spec
    (name "align")
    (arity 1)
    (params [list: ])
    (args ("d"))
    (return "Any")
    (contract (a-arrow "Any" "Any")))
  (fun-spec
    (name "hang")
    (arity 2)
    (params [list: ])
    (args ("i" "d"))
    (return "Any")
    (contract (a-arrow "Any" "Any" "Any")))
  (fun-spec
    (name "prefix")
    (arity 4)
    (params [list: ])
    (args ("n" "b" "x" "y"))
    (return "Any")
    (contract (a-arrow "Any" "Any" "Any" "Any" "Any")))
  (fun-spec
    (name "infix")
    (arity 5)
    (params [list: ])
    (args ("n" "b" "op" "x" "y"))
    (return "Any")
    (contract
      (a-arrow
        (a-id "Number" (xref "<global>" "Number"))
        (a-id "Number" (xref "<global>" "Number"))
        (a-id "PPrintDoc" (xref "pprint" "PPrintDoc"))
        (a-id "PPrintDoc" (xref "pprint" "PPrintDoc"))
        (a-id "PPrintDoc" (xref "pprint" "PPrintDoc"))
        "Any")))
  (fun-spec
    (name "infix-break")
    (arity 5)
    (params [list: ])
    (args ("n" "b" "op" "x" "y"))
    (return "Any")
    (contract
      (a-arrow
        (a-id "Number" (xref "<global>" "Number"))
        (a-id "Number" (xref "<global>" "Number"))
        (a-id "PPrintDoc" (xref "pprint" "PPrintDoc"))
        (a-id "PPrintDoc" (xref "pprint" "PPrintDoc"))
        (a-id "PPrintDoc" (xref "pprint" "PPrintDoc"))
        "Any")))
  (fun-spec
    (name "separate")
    (arity 2)
    (params [list: ])
    (args ("sep" "docs"))
    (return "Any")
    (contract
      (a-arrow
        (a-id "PPrintDoc" (xref "pprint" "PPrintDoc"))
        (a-dot "lists" "List")
        "Any")))
  (fun-spec
    (name "surround")
    (arity 5)
    (params [list: ])
    (args ("n" "b" "open" "contents" "close"))
    (return "Any")
    (contract
      (a-arrow
        (a-id "Number" (xref "<global>" "Number"))
        (a-id "Number" (xref "<global>" "Number"))
        (a-id "PPrintDoc" (xref "pprint" "PPrintDoc"))
        (a-id "PPrintDoc" (xref "pprint" "PPrintDoc"))
        (a-id "PPrintDoc" (xref "pprint" "PPrintDoc"))
        "Any")))
  (fun-spec
    (name "soft-surround")
    (arity 5)
    (params [list: ])
    (args ("n" "b" "open" "contents" "close"))
    (return "Any")
    (contract
      (a-arrow
        (a-id "Number" (xref "<global>" "Number"))
        (a-id "Number" (xref "<global>" "Number"))
        (a-id "PPrintDoc" (xref "pprint" "PPrintDoc"))
        (a-id "PPrintDoc" (xref "pprint" "PPrintDoc"))
        (a-id "PPrintDoc" (xref "pprint" "PPrintDoc"))
        "Any")))
  (fun-spec
    (name "surround-separate")
    (arity 7)
    (params [list: ])
    (args ("n" "b" "void" "open" "sep" "close" "docs"))
    (return "Any")
    (contract
      (a-arrow
        (a-id "Number" (xref "<global>" "Number"))
        (a-id "Number" (xref "<global>" "Number"))
        (a-id "PPrintDoc" (xref "pprint" "PPrintDoc"))
        (a-id "PPrintDoc" (xref "pprint" "PPrintDoc"))
        (a-id "PPrintDoc" (xref "pprint" "PPrintDoc"))
        (a-id "PPrintDoc" (xref "pprint" "PPrintDoc"))
        (a-dot "lists" "List")
        "Any")))
  (fun-spec
    (name "label-align-surround")
    (arity 5)
    (params [list: ])
    (args ("label" "open" "sep" "contents" "close"))
    (return "Any")
    (contract (a-arrow "Any" "Any" "Any" "Any" "Any" "Any")))
  (unknown-item
    (name "lparen")
    ;; str187("(")
    )
  (unknown-item
    (name "rparen")
    ;; str187(")")
    )
  (unknown-item
    (name "lbrace")
    ;; str187("{")
    )
  (unknown-item
    (name "rbrace")
    ;; str187("}")
    )
  (unknown-item
    (name "lbrack")
    ;; str187("[")
    )
  (unknown-item
    (name "rbrack")
    ;; str187("]")
    )
  (unknown-item
    (name "langle")
    ;; str187("<")
    )
  (unknown-item
    (name "rangle")
    ;; str187(">")
    )
  (unknown-item
    (name "comma")
    ;; str187(",")
    )
  (unknown-item
    (name "commabreak")
    ;; comma199 + ~sbreak200(1)
    )))
  


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
