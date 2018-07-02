include string-dict
include either
include file("../../../src/arr/desugar/ds-structs.arr")
include file("../../../src/arr/desugar/ds-parse.arr")
include file("../../../src/arr/desugar/ds-environment.arr")
include file("../../../src/arr/desugar/ds-match.arr")
include file("../../../src/arr/desugar/ds-desugar.arr")
include file("../../../src/arr/desugar/ds-resugar.arr")


check "main":
  rules = parse-ds-rules(
    ```
    sugar or:
    | (or a:Expr b) => (fresh [x] (let (bind x a) (if x x b)))
    end
    sugar bind:
    | (bind x a) => {bind x a}
    end
    sugar let:
    | (let {bind x a} body) => (apply (lambda x body) a)
    end
    sugar ors:
    | (ors [a]) => a
    | (ors [a bs_{i} ...i]) =>
      (fresh [x] (lets [(bind x a)] (if x x (ors [bs_{i} ...i]))))
    end
    sugar lets:
    | (lets [{bind x_{i} a_{i}} ...i] body) =>
      (apply (lambda [x_{i} ...i] body) [a_{i} ...i])
    end
    ```)

  e = parse-ast("(or p q)")
  resugar(desugar(rules, e)) is some(e)

  e1 = parse-ast("(bind 1 (+))")
  resugar(desugar(rules, e1)) is some(e1)

  e2 = parse-ast("(let (bind x (+ 1 2)) (- x 3))")
  resugar(desugar(rules, e2)) is some(e2)

  e3 = parse-ast("(ors [p q])")
  resugar(desugar(rules, e3)) is some(e3)
end

check "match":
  match-pattern(
    parse-ast("(hello {some jack} [[1 2] [3 4]])"),
    parse-pattern("(hello @l {some j} [[a b] ...i])"))
    is left({environment(
        [string-dict: "j", g-var(naked-var("jack")), "l", term-dummy-loc],
        [string-dict: ],
        [string-dict: "i", [list:
            environment(
              [string-dict: "a", g-prim(e-num(1)), "b", g-prim(e-num(2))],
              [string-dict: ],
              [string-dict: ]),
            environment(
              [string-dict: "a", g-prim(e-num(3)), "b", g-prim(e-num(4))],
              [string-dict: ],
              [string-dict: ])]]);
      p-surf("hello", [list:
          p-pvar("l", [set: ], none),
          p-option(some(p-pvar("j", [set: ], none))),
          p-list(seq-ellipsis-list(
              [list:
                p-list(seq-cons(p-pvar("a", [set: ], none), seq-cons(p-pvar("b", [set: ], none), seq-empty))),
                p-list(seq-cons(p-pvar("a", [set: ], none), seq-cons(p-pvar("b", [set: ], none), seq-empty)))],
              "i"))
        ])
    })
  
  match-pattern(
    g-list([list:
        parse-ast("p"),
        g-tag(parse-pattern("1"), parse-pattern("2"),
          g-tag(parse-pattern("x"), parse-pattern("y"),
            parse-ast("q")))
      ]),
    parse-pattern("[a ...i]"))
    is left({environment(
        [string-dict: ],
        [string-dict: ],
        [string-dict: "i", [list:
            environment(
              [string-dict: "a", parse-ast("p")],
              [string-dict: ],
              [string-dict: ]),
            environment(
              [string-dict: "a", parse-ast("q")],
              [string-dict: ],
              [string-dict: ])]]);
      p-list(seq-ellipsis-list(
          [list:
            parse-pattern("a"),
            p-tag(parse-pattern("1"), parse-pattern("2"),
              p-tag(parse-pattern("x"), parse-pattern("y"),
                parse-pattern("a")))
          ], "i"))})
  
  match-pattern(
    parse-ast("{some foobar}"),
    parse-pattern("(fresh [a] {some a})"))
    is left({environment(
        [string-dict: ],
        [string-dict: "a", naked-var("foobar")],
        [string-dict: ]);
      p-fresh([list: fresh-name("a")], p-option(some(p-var("foobar"))))})
  
  match-pattern(
    parse-ast("(Foo [1 2] [3 4])"), 
    parse-pattern("(Foo [a_{i} ...i] [a_{i} ...i])"))
    satisfies is-right
end


check "desugar":
  rules = parse-ds-rules(
    ```
    sugar or:
    | (or @l a:Expr b) => (fresh [x] (let (bind x a) (if x x b)))
    end
    ```)
  e = parse-ast("(or p q)")
  desugar(rules, e) does-not-raise
end

check "desugar: substitute":
  substitute([string-dict:], environment(
      [string-dict: "l", term-dummy-loc],
      [string-dict: ],
      [string-dict: "i", 
        [list:
          set-pvar(empty-env(), "a", parse-ast("<Foo>")),
          set-pvar(empty-env(), "a", parse-ast("5"))
      ]]),
    parse-pattern("(Bar @l [a_{i} ...i])")) ^ strip-tags
    is parse-ast("<Bar [<Foo> 5]>")
 
  substitute([string-dict:], environment(
      [string-dict: "l", term-dummy-loc],
      [string-dict: ],
      [string-dict:
        "i", 
        [list:
          set-pvar(empty-env(), "a", parse-ast("<Foo>")),
          set-pvar(empty-env(), "a", parse-ast("5"))
      ]]),
    parse-pattern("(Bar @l [a_{i} ...i] [a_{i} ...i])")) ^ strip-tags
    is parse-ast("<Bar [<Foo> 5] [<Foo> 5]>")  
end

check "parse-pattern":
  parse-pattern("3")
    is p-prim(e-num(3))
  parse-pattern("(foo @l 1 2)")
    is p-surf("foo", [list: p-pvar("l", [set: ], none), p-prim(e-num(1)), p-prim(e-num(2))])
  parse-pattern("[[a_{i} b_{i}] ...i]")
    is p-list(seq-ellipsis(p-list(seq-cons(p-pvar("a", [set: "i"], none),
    seq-cons(p-pvar("b", [set: "i"], none), seq-empty))), "i"))
  parse-pattern("[a b ...i]")
    is p-list(seq-cons(p-pvar("a", [set: ], none), seq-ellipsis(p-pvar("b", [set: ], none), "i")))
  parse-pattern("(fresh [b] {c-abc {some a} b})")
    is p-fresh([list: fresh-name("b")], p-aux("c-abc", [list: p-option(some(p-pvar("a", [set: ], none))), p-var("b")]))
  parse-pattern("[[a ...x] [b ...u]]")
    is p-list(seq-cons(p-list(seq-ellipsis(p-pvar("a", [set: ], none), "x")),
      seq-cons(p-list(seq-ellipsis(p-pvar("b", [set: ], none), "u")), seq-empty)))
end

check "parse-ds-rules":
  surf-and = p-surf("and", [list: p-pvar("@toploc", [list-set: ], none)])
  parse-ds-rules("sugar and: | (and) => (and) end")
    is [string-dict: "and", [list: ds-rule-case(surf-and, surf-and)]]
  parse-ds-rules(
    ```
    # ignore me
    sugar or: # ignore this
    | (or @l a:Expr b) => (fresh [x] (let (bind @a x l) (if x x b)))
    end
    # ignore
    ```) is [string-dict:
    "or", [list:
      ds-rule-case(
        p-surf("or", [list:
            p-pvar("l", [set: ], none),
            p-pvar("a", [set: ], some("Expr")),
            p-pvar("b", [set: ], none)]),
        p-fresh([list: fresh-name("x")],
          p-surf("let", [list: 
              p-pvar("l", [set: ], none),
              p-surf("bind", [list: p-pvar("a", [set: ], none), p-var("x"), p-pvar("l", [set: ], none)]),
              p-surf("if", [list:p-pvar("l", [set: ], none), p-var("x"), p-var("x"), p-pvar("b", [set: ], none)])])))]]
end
