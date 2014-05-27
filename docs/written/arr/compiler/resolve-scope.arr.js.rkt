#lang scribble/base
@(require "../../../scribble-api.rkt")
@docmodule["\"compiler/resolve-scope.arr\""]{
  @; Unknown: PLEASE DOCUMENT
  @ignore[(list "desugar-scope-visitor" "names")]
  @section[#:tag "\"compiler/resolve-scope.arr\"_ReExports"]{Re-exported values}
  @section[#:tag "\"compiler/resolve-scope.arr\"_DataTypes"]{Data types}
  @data-spec["NameResolution"]{
    @variants{
      @constr-spec["resolved"]{
        @members{
          @member-spec[
            "ast"
            #:contract (a-compound (a-dot "A" "Program") (xref "ast" "Program"))
          ]
          @member-spec[
            "shadowed"
            #:contract
            (a-app
              (a-id "List" (xref "lists" "List"))
              (a-compound
                (a-dot "C" "CompileError")
                (xref "\"compiler/compile-structs.arr\"" "CompileError")))
          ]
          @member-spec[
            "bindings"
            #:contract
            (a-compound
              (a-dot "SD" "StringDict")
              (xref "string-dict" "StringDict"))
          ]
        }
        @with-members{}
      }
    }
    @shared{}
  }
  @data-spec["ScopeBinding"]{
    @variants{
      @constr-spec["letrec-bind"]{
        @members{
          @member-spec["loc" #:contract "Any"]
          @member-spec[
            "atom"
            #:contract (a-compound (a-dot "A" "Name") (xref "ast" "Name"))
          ]
          @member-spec[
            "expr"
            #:contract
            (a-app
              (a-id "Option" (xref "option" "Option"))
              (a-compound (a-dot "A" "Expr") (xref "ast" "Expr")))
          ]
        }
        @with-members{}
      }
      @constr-spec["let-bind"]{
        @members{
          @member-spec["loc" #:contract "Any"]
          @member-spec[
            "atom"
            #:contract (a-compound (a-dot "A" "Name") (xref "ast" "Name"))
          ]
          @member-spec[
            "expr"
            #:contract
            (a-app
              (a-id "Option" (xref "option" "Option"))
              (a-compound (a-dot "A" "Expr") (xref "ast" "Expr")))
          ]
        }
        @with-members{}
      }
      @constr-spec["var-bind"]{
        @members{
          @member-spec["loc" #:contract "Any"]
          @member-spec[
            "atom"
            #:contract (a-compound (a-dot "A" "Name") (xref "ast" "Name"))
          ]
          @member-spec[
            "expr"
            #:contract
            (a-app
              (a-id "Option" (xref "option" "Option"))
              (a-compound (a-dot "A" "Expr") (xref "ast" "Expr")))
          ]
        }
        @with-members{}
      }
      @constr-spec["global-bind"]{
        @members{
          @member-spec["loc" #:contract "Any"]
          @member-spec[
            "atom"
            #:contract (a-compound (a-dot "A" "Name") (xref "ast" "Name"))
          ]
          @member-spec[
            "expr"
            #:contract
            (a-app
              (a-id "Option" (xref "option" "Option"))
              (a-compound (a-dot "A" "Expr") (xref "ast" "Expr")))
          ]
        }
        @with-members{}
      }
    }
    @shared{}
  }
  @section[#:tag "\"compiler/resolve-scope.arr\"_Functions"]{Functions}
  @function[
    "is-resolved"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function["mk-bind" #:contract (a-arrow "Any" "Any" "Any")]
  @function["mk-id" #:contract (a-arrow "Any" "Any" "Any")]
  @function[
    "resolve-provide"
    #:contract
    (a-arrow
      (a-compound (a-dot "A" "Provide") (xref "ast" "Provide"))
      (a-compound (a-dot "A" "Expr") (xref "ast" "Expr"))
      "Any")
  ]
  @function[
    "resolve-imports"
    #:contract
    (a-arrow
      (a-app
        (a-id "List" (xref "lists" "List"))
        (a-compound (a-dot "A" "Import") (xref "ast" "Import")))
      "Any")
  ]
  @function[
    "desugar-scope-block"
    #:contract
    (a-arrow
      "Any"
      "Any"
      "Any"
      (a-app
        (a-id "List" (xref "lists" "List"))
        (a-id "Expr" (xref "<global>" "Expr"))))
  ]{
    @; let 
    @;     p = fun(str): PP.surface-parse(str, "test").block end,
    @;     d = A.dummy-loc,
    @;     b = fun(s): A.s-bind(d, false, A.s-name(d, s), A.a-blank) end,
    @;     id = fun(s): A.s-id(d, A.s-name(d, s)) end,
    @;     bk = fun(e): A.s-block(d, [list: e]) end,
    @;     bs =
    @;       fun(str):
    @;         A.s-block(d, desugar-scope-block(p(str).stmts, [list: ], [list: ]))
    @;       end,
    @;     n = none,
    @;     thunk = fun(e): A.s-lam(d, [list: ], [list: ], A.a-blank, "", bk(e), n) end,
    @;     compare1 =
    @;       A.s-let-expr(d,
    @;         [list: 
    @;           A.s-let-bind(d, b("x"), A.s-num(d, 15)),
    @;           A.s-let-bind(d, b("y"), A.s-num(d, 10))
    @;         ],
    @;         id("y")):
    @;   desugar-scope-block(p("x = 15 y = 10 y").stmts, [list: ], [list: ]).first satisfies
    @;     A.equiv-ast(_, compare1)
    @;   desugar-scope-block(p("x = 55 var y = 10 y").stmts, [list: ], [list: ]).first satisfies
    @;     A.equiv-ast(_,
    @;       A.s-let-expr(d,
    @;         [list: 
    @;           A.s-let-bind(d, b("x"), A.s-num(d, 55)),
    @;           A.s-var-bind(d, b("y"), A.s-num(d, 10))
    @;         ],
    @;         id("y")))
    @;   bs("x = 7 print(2) var y = 10 y") satisfies
    @;     A.equiv-ast(_,
    @;       A.s-block(d,
    @;         [list: 
    @;           A.s-let-expr(d,
    @;             [list: A.s-let-bind(d, b("x"), A.s-num(d, 7))],
    @;             A.s-block(d,
    @;               [list: 
    @;                 A.s-app(d, id("print"), [list: A.s-num(d, 2)]),
    @;                 A.s-let-expr(d,
    @;                   [list: A.s-var-bind(d, b("y"), A.s-num(d, 10))],
    @;                   id("y"))
    @;               ]))
    @;         ]))
    @;   let  prog = bs("fun f(): 4 end fun g(): 5 end f()"):
    @;     prog satisfies
    @;       A.equiv-ast(_,
    @;         A.s-block(d,
    @;           [list: 
    @;             A.s-letrec(d,
    @;               [list: 
    @;                 A.s-letrec-bind(d, b("f"), thunk(A.s-num(d, 4))),
    @;                 A.s-letrec-bind(d, b("g"), thunk(A.s-num(d, 5)))
    @;               ],
    @;               A.s-app(d, id("f"), [list: ]))
    @;           ]))
    @;     let 
    @;         p-s = fun(e): A.s-app(d, id("print"), [list: e]) end,
    @;         pretty = fun(e): e.tosource().pretty(80).join-str("\n") end,
    @;         prog2 =
    @;           bs("print(1) fun f(): 4 end fun g(): 5 end fun h(): 6 end x = 3 print(x)"):
    @;       prog2 satisfies
    @;         A.equiv-ast(_,
    @;           A.s-block(d,
    @;             [list: 
    @;               p-s(A.s-num(d, 1)),
    @;               A.s-letrec(d,
    @;                 [list: 
    @;                   A.s-letrec-bind(d, b("f"), thunk(A.s-num(d, 4))),
    @;                   A.s-letrec-bind(d, b("g"), thunk(A.s-num(d, 5))),
    @;                   A.s-letrec-bind(d, b("h"), thunk(A.s-num(d, 6)))
    @;                 ],
    @;                 A.s-block(d,
    @;                   [list: 
    @;                     A.s-let-expr(d,
    @;                       [list: A.s-let-bind(d, b("x"), A.s-num(d, 3))],
    @;                       p-s(id("x")))
    @;                   ]))
    @;             ]))
    @;       desugar-scope-block([list: prog2], [list: ], [list: ]).first satisfies
    @;         A.equiv-ast(_, prog2)
    @;       for each2(
    @;           p1 from desugar-scope-block(prog2.stmts, [list: ], [list: ]),
    @;           p2 from prog2.stmts
    @;       ) -> Any:
    @;         p1 satisfies A.equiv-ast(_, p2)
    @;       end
    @;       let  prog3 = bs("print(x) x := 3 print(x)"):
    @;         prog3 satisfies
    @;           A.equiv-ast(_,
    @;             A.s-block(d,
    @;               [list: 
    @;                 p-s(id("x")),
    @;                 A.s-assign(d, A.s-name(d, "x"), A.s-num(d, 3)),
    @;                 p-s(id("x"))
    @;               ]))
    @;         let  prog4 = bs("var x = 10 fun f(): 4 end f()"):
    @;           prog4 satisfies
    @;             A.equiv-ast(_,
    @;               A.s-block(d,
    @;                 [list: 
    @;                   A.s-let-expr(d,
    @;                     [list: A.s-var-bind(d, b("x"), A.s-num(d, 10))],
    @;                     A.s-block(d,
    @;                       [list: 
    @;                         A.s-letrec(d,
    @;                           [list: 
    @;                             A.s-letrec-bind(d, b("f"), thunk(A.s-num(d, 4)))
    @;                           ],
    @;                           A.s-app(d, id("f"), [list: ]))
    @;                       ]))
    @;                 ]))
    @;         end
    @;       end
    @;     end
    @;   end
    @; end
    
  }
  @function[
    "wrap-env-imports"
    #:contract
    (a-arrow
      "Any"
      (a-compound (a-dot "A" "Expr") (xref "ast" "Expr"))
      (a-compound
        (a-dot "C" "CompileEnvironment")
        (xref "\"compiler/compile-structs.arr\"" "CompileEnvironment"))
      "Any")
  ]
  @function[
    "desugar-scope"
    #:contract
    (a-arrow
      (a-compound (a-dot "A" "Program") (xref "ast" "Program"))
      (a-compound
        (a-dot "C" "CompileEnvironment")
        (xref "\"compiler/compile-structs.arr\"" "CompileEnvironment"))
      "Any")
  ]{
    @; let 
    @;     d = A.dummy-loc,
    @;     b = fun(s): A.s-bind(d, false, A.s-name(d, s), A.a-blank) end,
    @;     id = fun(s): A.s-id(d, A.s-name(d, s)) end,
    @;     checks =
    @;       A.s-data-field(d,
    @;         A.s-str(d, "checks"),
    @;         A.s-app(d, A.s-dot(d, U.checkers(d), "results"), [list: ])),
    @;     str = A.s-str(d, _),
    @;     ds = desugar-scope(_, C.minimal-builtins),
    @;     compare1 =
    @;       A.s-program(d,
    @;         A.s-provide-none(d),
    @;         [list: ],
    @;         A.s-block(d,
    @;           [list: 
    @;             A.s-block(d,
    @;               [list: 
    @;                 A.s-let-expr(d,
    @;                   [list: A.s-let-bind(d, b("x"), A.s-num(d, 10))],
    @;                   A.s-obj(d,
    @;                     [list: 
    @;                       A.s-data-field(d, str("answer"), id("nothing")),
    @;                       A.s-data-field(d, str("provide"), id("x")),
    @;                       checks
    @;                     ]))
    @;               ])
    @;           ])):
    @;   ds(PP.surface-parse("provide x end x = 10 nothing", "test")) satisfies
    @;     A.equiv-ast-prog(_, compare1)
    @;   let 
    @;       compare2 =
    @;         A.s-program(d,
    @;           A.s-provide-none(d),
    @;           [list: 
    @;             A.s-import(d, A.s-file-import(d, "./foo.arr"), A.s-name(d, "F"))
    @;           ],
    @;           A.s-block(d,
    @;             [list: 
    @;               A.s-block(d,
    @;                 [list: 
    @;                   A.s-let-expr(d,
    @;                     [list: A.s-let-bind(d, b("x"), A.s-num(d, 10))],
    @;                     A.s-obj(d,
    @;                       [list: 
    @;                         A.s-data-field(d,
    @;                           str("answer"),
    @;                           A.s-app(d, id("F"), [list: id("x")])),
    @;                         A.s-data-field(d, str("provide"), id("x")),
    @;                         checks
    @;                       ]))
    @;                 ])
    @;             ])):
    @;     ds(PP.surface-parse("provide x end import 'foo.arr' as F x = 10 F(x)",
    @;         "test")) satisfies
    @;       A.equiv-ast-prog(_, compare2)
    @;   end
    @; end
    
  }
  @function[
    "is-letrec-bind"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-let-bind"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-var-bind"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "is-global-bind"
    #:contract (a-arrow "Any" (a-id "Bool" (xref "<global>" "Bool")))
  ]
  @function[
    "scope-env-from-env"
    #:contract
    (a-arrow
      (a-compound
        (a-dot "C" "CompileEnvironment")
        (xref "\"compiler/compile-structs.arr\"" "CompileEnvironment"))
      "Any")
  ]{
    @; scope-env-from-env(C.compile-env([list: C.builtin-id("x")])).get("x") is
    @;   let-bind(S.builtin("pyret-builtin"), names.s-global("x"), none)
    
  }
  @function[
    "resolve-names"
    #:contract
    (a-arrow
      (a-compound (a-dot "A" "Program") (xref "ast" "Program"))
      (a-compound
        (a-dot "C" "CompileEnvironment")
        (xref "\"compiler/compile-structs.arr\"" "CompileEnvironment"))
      "Any")
  ]
}