#lang scribble/base
@(require "../../../scribble-api.rkt")
@docmodule["\"compiler/desugar.arr\""]{
  @; Unknown: PLEASE DOCUMENT
  @ignore[(list "names" "mt-d-env")]
  @section[#:tag "\"compiler/desugar.arr\"_ReExports"]{Re-exported values}
  @section[#:tag "\"compiler/desugar.arr\"_DataTypes"]{Data types}
  @data-spec["DesugarEnv"]{
    @variants{
      @constr-spec["d-env"]{
        @members{
          @member-spec[
            "ids"
            #:contract
            (a-app
              (a-id "Set" (xref "<global>" "Set"))
              (a-id "String" (xref "<global>" "String")))
          ]
          @member-spec[
            "vars"
            #:contract
            (a-app
              (a-id "Set" (xref "<global>" "Set"))
              (a-id "String" (xref "<global>" "String")))
          ]
          @member-spec[
            "letrecs"
            #:contract
            (a-app
              (a-id "Set" (xref "<global>" "Set"))
              (a-id "String" (xref "<global>" "String")))
          ]
        }
        @with-members{}
      }
    }
    @shared{}
  }
  @data-spec["Pair"]{
    @variants{
      @constr-spec["pair"]{
        @members{
          @member-spec["left" #:contract "Any"]
          @member-spec["right" #:contract "Any"]
        }
        @with-members{}
      }
    }
    @shared{}
  }
  @section[#:tag "\"compiler/desugar.arr\"_Functions"]{Functions}
  @function[
    "is-d-env"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-pair"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function["g" #:contract (a-arrow "Any" "Any")]
  @function["gid" #:contract (a-arrow "Any" "Any" "Any")]
  @function[
    "check-bool"
    #:contract (a-arrow "Any" "Any" "Any" "Any" "Any" "Any")
  ]
  @function["no-branches-exn" #:contract (a-arrow "Any" "Any" "Any")]
  @function["make-message-exception" #:contract (a-arrow "Any" "Any" "Any")]
  @function["make-message-exception-e" #:contract (a-arrow "Any" "Any" "Any")]
  @function["bool-exn" #:contract (a-arrow "Any" "Any" "Any" "Any")]
  @function["bool-op-exn" #:contract (a-arrow "Any" "Any" "Any" "Any" "Any")]
  @function[
    "desugar-ann"
    #:contract
    (a-arrow
      (a-compound (a-dot "A" "Ann") (xref "ast" "Ann"))
      (a-compound (a-dot "A" "Ann") (xref "ast" "Ann")))
  ]
  @function[
    "desugar"
    #:contract
    (a-arrow
      (a-compound (a-dot "A" "Program") (xref "ast" "Program"))
      (a-compound
        (a-dot "C" "CompileEnvironment")
        (xref "\"compiler/compile-structs.arr\"" "CompileEnvironment"))
      "Any")
  ]
  @function["mk-bind" #:contract (a-arrow "Any" "Any" "Any")]
  @function["mk-id" #:contract (a-arrow "Any" "Any" "Any")]
  @function["make-torepr" #:contract (a-arrow "Any" "Any" "Any" "Any" "Any")]
  @function["make-match" #:contract (a-arrow "Any" "Any" "Any" "Any")]
  @function["get-arith-op" #:contract (a-arrow "Any" "Any")]
  @function[
    "desugar-if"
    #:contract
    (a-arrow
      "Any"
      "Any"
      (a-compound (a-dot "A" "Expr") (xref "ast" "Expr"))
      "Any")
  ]
  @function["desugar-case-branch" #:contract (a-arrow "Any" "Any")]
  @function[
    "desugar-cases"
    #:contract (a-arrow "Any" "Any" "Any" "Any" "Any" "Any")
    #:examples
    '@{
      @; let 
      @;     d = A.dummy-loc,
      @;     prog =
      @;       desugar-cases(d,
      @;         A.a-blank,
      @;         A.s-num(d, 1),
      @;         [list: 
      @;           A.s-data-field(d,
      @;             A.s-str(d, "empty"),
      @;             A.s-lam(d, [list: ], [list: ], A.a-blank, "", A.s-num(d, 5), none))
      @;         ],
      @;         A.s-num(d, 4)),
      @;     id = prog.binds.first.b:
      @;   prog satisfies
      @;     A.equiv-ast(_,
      @;       A.s-let-expr(d,
      @;         [list: A.s-let-bind(d, id, A.s-num(d, 1))],
      @;         A.s-app(d,
      @;           A.s-dot(d, A.s-id(d, id.id), "_match"),
      @;           [list: 
      @;             A.s-obj(d,
      @;               [list: 
      @;                 A.s-data-field(d,
      @;                   A.s-str(d, "empty"),
      @;                   A.s-lam(d,
      @;                     [list: ],
      @;                     [list: ],
      @;                     A.a-blank,
      @;                     "",
      @;                     A.s-num(d, 5),
      @;                     none))
      @;               ]),
      @;             A.s-lam(d, [list: ], [list: ], A.a-blank, "", A.s-num(d, 4), none)
      @;           ])))
      @; end
      
    }
  ]
  @function["desugar-variant-member" #:contract (a-arrow "Any" "Any")]
  @function["desugar-member" #:contract (a-arrow "Any" "Any")]
  @function["is-underscore" #:contract (a-arrow "Any" "Any")]
  @function["ds-curry-args" #:contract (a-arrow "Any" "Any" "Any")]
  @function[
    "ds-curry-nullary"
    #:contract (a-arrow "Any" "Any" "Any" "Any" "Any")
    #:examples
    '@{
      @; nothing
      
    }
  ]
  @function["ds-curry-binop" #:contract (a-arrow "Any" "Any" "Any" "Any" "Any")]
  @function[
    "ds-curry"
    #:contract (a-arrow "Any" "Any" "Any" "Any")
    #:examples
    '@{
      @; let 
      @;     d = A.dummy-loc,
      @;     n = A.s-global,
      @;     id = lam(s): A.s-id(d, A.s-global(s)) end,
      @;     under = A.s-id(d, A.s-underscore(d)),
      @;     ds-ed = ds-curry(d, id("f"), [list: under, id("x")]):
      @;   ds-ed satisfies A.is-s-lam
      @;   ds-ed.args.length() is 1
      @;   let  ds-ed2 = ds-curry(d, id("f"), [list: under, under]):
      @;     ds-ed2 satisfies A.is-s-lam
      @;     ds-ed2.args.length() is 2
      @;     let  ds-ed3 = ds-curry(d, id("f"), [list: id("x"), id("y")]):
      @;       ds-ed3 satisfies
      @;         A.equiv-ast(_, A.s-app(d, id("f"), [list: id("x"), id("y")]))
      @;       let  ds-ed4 = ds-curry(d, A.s-dot(d, under, "f"), [list: id("x")]):
      @;         ds-ed4 satisfies A.is-s-lam
      @;         ds-ed4.args.length() is 1
      @;       end
      @;     end
      @;   end
      @; end
      
    }
  ]
  @function[
    "desugar-opt"
    #:params (list "T")
    #:contract
    (a-arrow
      (a-arrow "T" "T")
      (a-app (a-id "Option" (xref "option" "Option")) "T")
      "Any")
  ]
  @function[
    "desugar-bind"
    #:contract
    (a-arrow (a-compound (a-dot "A" "Bind") (xref "ast" "Bind")) "Any")
  ]
  @function[
    "desugar-expr"
    #:contract
    (a-arrow (a-compound (a-dot "A" "Expr") (xref "ast" "Expr")) "Any")
    #:examples
    '@{
      @; let 
      @;     d = A.dummy-loc,
      @;     unglobal =
      @;       A.default-map-visitor.{
      @;         s-global(self, s): A.s-name(d, s) end,
      @;         s-atom(self, base, serial): A.s-name(d, base) end
      @;       },
      @;     p = lam(str): PP.surface-parse(str, "test").block end,
      @;     ds = lam(prog): desugar-expr(prog).visit(unglobal) end,
      @;     id = lam(s): A.s-id(d, A.s-name(d, s)) end,
      @;     one = A.s-num(d, 1),
      @;     two = A.s-num(d, 2),
      @;     equiv = lam(e): A.equiv-ast(_, e) end,
      @;     pretty = lam(prog): prog.tosource().pretty(80).join-str("\n") end,
      @;     if-else = "if true: 5 else: 6 end",
      @;     ask-otherwise = "ask: | true then: 5 | otherwise: 6 end":
      @;   p(if-else) ^ pretty is if-else
      @;   p(ask-otherwise) ^ pretty is ask-otherwise
      @;   let  prog2 = p("[list: 1,2,1 + 2]"):
      @;     ds(prog2) satisfies
      @;       equiv(A.s-block(d,
      @;           [list: 
      @;             A.s-app(d,
      @;               A.s-dot(d, A.s-id(d, A.s-name(d, "list")), "make"),
      @;               [list: 
      @;                 A.s-array(d,
      @;                   [list: one, two, A.s-app(d, id("_plus"), [list: one, two])])
      @;               ])
      @;           ]))
      @;     let  prog3 = p("for map(elt from l): elt + 1 end"):
      @;       ds(prog3) satisfies equiv(p("map(lam(elt): _plus(elt, 1) end, l)"))
      @;     end
      @;   end
      @; end
      
    }
  ]
}