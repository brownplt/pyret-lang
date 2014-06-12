#lang scribble/base
@(require "../../../scribble-api.rkt")
@docmodule["\"compiler/desugar.arr\""]{
  @; Ignored type testers
  @ignore[(list "is-d-env" "is-pair" "is-underscore")]
  @; Unknown: PLEASE DOCUMENT
  @ignore[(list "names" "mt-d-env")]
  @section[#:tag "\"compiler/desugar.arr\"_DataTypes"]{Data types}
  @data-spec["DesugarEnv"]{
    @variants{
      @constr-spec["d-env"]{
        @members{
          @member-spec["ids"]
          @member-spec["vars"]
          @member-spec["letrecs"]
        }
        @with-members{}
      }
    }
    @shared{}
  }
  
  @data-spec["Pair"]{
    @variants{
      @constr-spec["pair"]{
        @members{@member-spec["left"] @member-spec["right"]}
        @with-members{}
      }
    }
    @shared{}
  }
  
  @section[#:tag "\"compiler/desugar.arr\"_Functions"]{Functions}
  @function["g"]
  @function["gid"]
  @function["check-bool"]
  @function["no-branches-exn"]
  @function["make-message-exception"]
  @function["make-message-exception-e"]
  @function["bool-exn"]
  @function["bool-op-exn"]
  @function["desugar-afield"]
  @function["desugar-ann"]
  @function["desugar"]
  @function["mk-bind"]
  @function["mk-id"]
  @function["make-torepr"]
  @function["make-match"]
  @function["get-arith-op"]
  @function["desugar-if"]
  @function["desugar-case-branch"]
  @function[
    "desugar-cases"
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
      @;   prog.visit(A.dummy-loc-visitor) is
      @;     A.s-let-expr(d,
      @;       [list: A.s-let-bind(d, id, A.s-num(d, 1))],
      @;       A.s-app(d,
      @;         A.s-dot(d, A.s-id(d, id.id), "_match"),
      @;         [list: 
      @;           A.s-obj(d,
      @;             [list: 
      @;               A.s-data-field(d,
      @;                 A.s-str(d, "empty"),
      @;                 A.s-lam(d,
      @;                   [list: ],
      @;                   [list: ],
      @;                   A.a-blank,
      @;                   "",
      @;                   A.s-num(d, 5),
      @;                   none))
      @;             ]),
      @;           A.s-lam(d, [list: ], [list: ], A.a-blank, "", A.s-num(d, 4), none)
      @;         ]))
      @; end
      
    }
  ]
  @function["desugar-variant-member"]
  @function["desugar-member"]
  @function["ds-curry-args"]
  @function[
    "ds-curry-nullary"
    #:examples
    '@{
      @; nothing
      
    }
  ]
  @function["ds-curry-binop"]
  @function[
    "ds-curry"
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
      @;       ds-ed3.visit(A.dummy-loc-visitor) is
      @;         A.s-app(d, id("f"), [list: id("x"), id("y")])
      @;       let  ds-ed4 = ds-curry(d, A.s-dot(d, under, "f"), [list: id("x")]):
      @;         ds-ed4 satisfies A.is-s-lam
      @;         ds-ed4.args.length() is 1
      @;       end
      @;     end
      @;   end
      @; end
      
    }
  ]
  @function["desugar-opt" #:params (list "T")]
  @function["desugar-bind"]
  @function[
    "desugar-expr"
    #:examples
    '@{
      @; let 
      @;     d = A.dummy-loc,
      @;     unglobal =
      @;       A.default-map-visitor.{
      @;         s-global(self, s): A.s-name(d, s) end,
      @;         s-atom(self, base, serial): A.s-name(d, base) end
      @;       },
      @;     p =
      @;       lam(str):
      @;         PP.surface-parse(str, "test").block.visit(A.dummy-loc-visitor)
      @;       end,
      @;     ds =
      @;       lam(prog):
      @;         desugar-expr(prog).visit(unglobal).visit(A.dummy-loc-visitor)
      @;       end,
      @;     id = lam(s): A.s-id(d, A.s-name(d, s)) end,
      @;     one = A.s-num(d, 1),
      @;     two = A.s-num(d, 2),
      @;     pretty = lam(prog): prog.tosource().pretty(80).join-str("\n") end,
      @;     if-else = "if true: 5 else: 6 end",
      @;     ask-otherwise = "ask: | true then: 5 | otherwise: 6 end":
      @;   p(if-else) ^ pretty is if-else
      @;   p(ask-otherwise) ^ pretty is ask-otherwise
      @;   let  prog2 = p("[list: 1,2,1 + 2]"):
      @;     ds(prog2) is
      @;       A.s-block(d,
      @;         [list: 
      @;           A.s-app(d,
      @;             A.s-dot(d, A.s-id(d, A.s-name(d, "list")), "make"),
      @;             [list: 
      @;               A.s-array(d,
      @;                 [list: one, two, A.s-app(d, id("_plus"), [list: one, two])])
      @;             ])
      @;         ])
      @;     let  prog3 = p("for map(elt from l): elt + 1 end"):
      @;       ds(prog3) is p("map(lam(elt): _plus(elt, 1) end, l)")
      @;     end
      @;   end
      @; end
      
    }
  ]
}