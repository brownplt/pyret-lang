#lang scribble/base
@(require "../../../scribble-api.rkt")
@docmodule["\"compiler/resolve-scope.arr\""]{
  @; Ignored type testers
  @ignore[
    (list
      "is-resolved"
      "is-letrec-bind"
      "is-let-bind"
      "is-var-bind"
      "is-global-bind"
      "is-global-type-bind"
      "is-let-type-bind"
      "is-type-var-bind")
  ]
  @; Unknown: PLEASE DOCUMENT
  @ignore[(list "desugar-scope-visitor" "names")]
  @section[#:tag "\"compiler/resolve-scope.arr\"_DataTypes"]{Data types}
  @data-spec["NameResolution"]{
    @variants{
      @constr-spec["resolved"]{
        @members{
          @member-spec["ast"]
          @member-spec["errors"]
          @member-spec["bindings"]
          @member-spec["type-bindings"]
        }
        @with-members{}
      }
    }
    @shared{}
  }
  
  @data-spec["ScopeBinding"]{
    @variants{
      @constr-spec["letrec-bind"]{
        @members{@member-spec["loc"] @member-spec["atom"] @member-spec["expr"]}
        @with-members{}
      }
      @constr-spec["let-bind"]{
        @members{@member-spec["loc"] @member-spec["atom"] @member-spec["expr"]}
        @with-members{}
      }
      @constr-spec["var-bind"]{
        @members{@member-spec["loc"] @member-spec["atom"] @member-spec["expr"]}
        @with-members{}
      }
      @constr-spec["global-bind"]{
        @members{@member-spec["loc"] @member-spec["atom"] @member-spec["expr"]}
        @with-members{}
      }
    }
    @shared{}
  }
  
  @data-spec["TypeBinding"]{
    @variants{
      @constr-spec["global-type-bind"]{
        @members{@member-spec["loc"] @member-spec["atom"] @member-spec["ann"]}
        @with-members{}
      }
      @constr-spec["let-type-bind"]{
        @members{@member-spec["loc"] @member-spec["atom"] @member-spec["ann"]}
        @with-members{}
      }
      @constr-spec["type-var-bind"]{
        @members{@member-spec["loc"] @member-spec["atom"] @member-spec["ann"]}
        @with-members{}
      }
    }
    @shared{}
  }
  
  @section[#:tag "\"compiler/resolve-scope.arr\"_Functions"]{Functions}
  @function["mk-bind"]
  @function["mk-id"]
  @function["resolve-provide"]
  @function["resolve-type-provide"]
  @function["resolve-imports"]
  @function["desugar-toplevel-types"]
  @function[
    "desugar-scope-block"
    #:examples
    '@{
      @; let 
      @;     p = lam(str): PP.surface-parse(str, "test").block end,
      @;     d = A.dummy-loc,
      @;     b = lam(s): A.s-bind(d, false, A.s-name(d, s), A.a-blank) end,
      @;     id = lam(s): A.s-id(d, A.s-name(d, s)) end,
      @;     bk = lam(e): A.s-block(d, [list: e]) end,
      @;     bs =
      @;       lam(str):
      @;         A.s-block(d,
      @;           desugar-scope-block(p(str).stmts, [list: ], [list: ], [list: ]))
      @;           .visit(A.dummy-loc-visitor)
      @;       end,
      @;     n = none,
      @;     thunk = lam(e): A.s-lam(d, [list: ], [list: ], A.a-blank, "", bk(e), n) end,
      @;     compare1 =
      @;       A.s-let-expr(d,
      @;         [list: 
      @;           A.s-let-bind(d, b("x"), A.s-num(d, 15)),
      @;           A.s-let-bind(d, b("y"), A.s-num(d, 10))
      @;         ],
      @;         id("y")):
      @;   desugar-scope-block(p("x = 15 y = 10 y").stmts, empty, empty, empty).first
      @;     .visit(A.dummy-loc-visitor) is
      @;     compare1
      @;   desugar-scope-block(p("x = 55 var y = 10 y").stmts, empty, empty, empty).first
      @;     .visit(A.dummy-loc-visitor) is
      @;     A.s-let-expr(d,
      @;       [list: 
      @;         A.s-let-bind(d, b("x"), A.s-num(d, 55)),
      @;         A.s-var-bind(d, b("y"), A.s-num(d, 10))
      @;       ],
      @;       id("y"))
      @;   bs("x = 7 print(2) var y = 10 y") is
      @;     A.s-block(d,
      @;       [list: 
      @;         A.s-let-expr(d,
      @;           [list: A.s-let-bind(d, b("x"), A.s-num(d, 7))],
      @;           A.s-block(d,
      @;             [list: 
      @;               A.s-app(d, id("print"), [list: A.s-num(d, 2)]),
      @;               A.s-let-expr(d,
      @;                 [list: A.s-var-bind(d, b("y"), A.s-num(d, 10))],
      @;                 id("y"))
      @;             ]))
      @;       ])
      @;   let  prog = bs("fun f(): 4 end fun g(): 5 end f()"):
      @;     prog is
      @;       A.s-block(d,
      @;         [list: 
      @;           A.s-letrec(d,
      @;             [list: 
      @;               A.s-letrec-bind(d, b("f"), thunk(A.s-num(d, 4))),
      @;               A.s-letrec-bind(d, b("g"), thunk(A.s-num(d, 5)))
      @;             ],
      @;             A.s-app(d, id("f"), [list: ]))
      @;         ])
      @;     let 
      @;         p-s = lam(e): A.s-app(d, id("print"), [list: e]) end,
      @;         pretty = lam(e): e.tosource().pretty(80).join-str("\n") end,
      @;         prog2 =
      @;           bs("print(1) fun f(): 4 end fun g(): 5 end fun h(): 6 end x = 3 print(x)"):
      @;       prog2 is
      @;         A.s-block(d,
      @;           [list: 
      @;             p-s(A.s-num(d, 1)),
      @;             A.s-letrec(d,
      @;               [list: 
      @;                 A.s-letrec-bind(d, b("f"), thunk(A.s-num(d, 4))),
      @;                 A.s-letrec-bind(d, b("g"), thunk(A.s-num(d, 5))),
      @;                 A.s-letrec-bind(d, b("h"), thunk(A.s-num(d, 6)))
      @;               ],
      @;               A.s-block(d,
      @;                 [list: 
      @;                   A.s-let-expr(d,
      @;                     [list: A.s-let-bind(d, b("x"), A.s-num(d, 3))],
      @;                     p-s(id("x")))
      @;                 ]))
      @;           ])
      @;       desugar-scope-block([list: prog2], [list: ], [list: ], [list: ]).first is
      @;         prog2
      @;       for each2(
      @;           p1 from desugar-scope-block(prog2.stmts, [list: ], [list: ], [list: ]),
      @;           p2 from prog2.stmts
      @;       ) -> Any:
      @;         p1.visit(A.dummy-loc-visitor) is p2
      @;       end
      @;       let  prog3 = bs("print(x) x := 3 print(x)"):
      @;         prog3 is
      @;           A.s-block(d,
      @;             [list: 
      @;               p-s(id("x")),
      @;               A.s-assign(d, A.s-name(d, "x"), A.s-num(d, 3)),
      @;               p-s(id("x"))
      @;             ])
      @;         let  prog4 = bs("var x = 10 fun f(): 4 end f()"):
      @;           prog4 is
      @;             A.s-block(d,
      @;               [list: 
      @;                 A.s-let-expr(d,
      @;                   [list: A.s-var-bind(d, b("x"), A.s-num(d, 10))],
      @;                   A.s-block(d,
      @;                     [list: 
      @;                       A.s-letrec(d,
      @;                         [list: A.s-letrec-bind(d, b("f"), thunk(A.s-num(d, 4)))],
      @;                         A.s-app(d, id("f"), [list: ]))
      @;                     ]))
      @;               ])
      @;         end
      @;       end
      @;     end
      @;   end
      @; end
      
    }
  ]
  @function["wrap-env-imports"]
  @function[
    "desugar-scope"
    #:examples
    '@{
      @; let 
      @;     d = A.dummy-loc,
      @;     b = lam(s): A.s-bind(d, false, A.s-name(d, s), A.a-blank) end,
      @;     id = lam(s): A.s-id(d, A.s-name(d, s)) end,
      @;     checks = A.s-app(d, A.s-dot(d, U.checkers(d), "results"), [list: ]),
      @;     str = A.s-str(d, _),
      @;     ds =
      @;       lam(prog):
      @;         desugar-scope(prog, C.minimal-builtins).visit(A.dummy-loc-visitor)
      @;       end,
      @;     compare1 =
      @;       A.s-program(d,
      @;         A.s-provide-none(d),
      @;         A.s-provide-types-none(d),
      @;         [list: ],
      @;         A.s-type-let-expr(d,
      @;           [list: ],
      @;           A.s-block(d,
      @;             [list: 
      @;               A.s-block(d,
      @;                 [list: 
      @;                   A.s-let-expr(d,
      @;                     [list: A.s-let-bind(d, b("x"), A.s-num(d, 10))],
      @;                     A.s-module(d, id("nothing"), id("x"), [list: ], checks))
      @;                 ])
      @;             ]))):
      @;   ds(PP.surface-parse("provide x end x = 10 nothing", "test")) is compare1
      @;   let 
      @;       compare2 =
      @;         A.s-program(d,
      @;           A.s-provide-none(d),
      @;           A.s-provide-types-none(d),
      @;           [list: 
      @;             A.s-import(d, A.s-file-import(d, "./foo.arr"), A.s-name(d, "F"))
      @;           ],
      @;           A.s-type-let-expr(d,
      @;             [list: ],
      @;             A.s-block(d,
      @;               [list: 
      @;                 A.s-block(d,
      @;                   [list: 
      @;                     A.s-let-expr(d,
      @;                       [list: A.s-let-bind(d, b("x"), A.s-num(d, 10))],
      @;                       A.s-module(d,
      @;                         A.s-app(d, id("F"), [list: id("x")]),
      @;                         id("x"),
      @;                         [list: ],
      @;                         checks))
      @;                   ])
      @;               ]))):
      @;     ds(PP.surface-parse("provide x end import 'foo.arr' as F x = 10 F(x)",
      @;         "test")) is
      @;       compare2
      @;   end
      @; end
      
    }
  ]
  @function[
    "scope-env-from-env"
    #:examples
    '@{
      @; scope-env-from-env(C.compile-env([list: C.builtin-id("x")], [list: ])).get("x") is
      @;   let-bind(S.builtin("pyret-builtin"), names.s-global("x"), none)
      
    }
  ]
  @function["type-env-from-env"]
  @function["resolve-names"]
}