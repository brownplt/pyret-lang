import s-exp as S

p = S.read-sexp

check:
  p("()") is empty
  p("(5)") is [list: 5]
  p("(5 4)") is [list: 5, 4]
  p("(a 5)") is [list: "a", 5]
  p("a") is "a"
  p("\"a\"") is [list: "string", "a"]
  p("\"a\"") is [list: "string", "a"]
  p("(a (b c))") is [list: "a", [list: "b", "c"]]
  p("(\"a\" (5 (4) ()) \"b\")") is
    [list:
      [list: "string", "a"],
      [list:
        5,
        [list: 4],
        empty
      ],
      [list: "string", "b"]
    ]

  p("-5") is -5
  p("-4.4") is -4.4
  p("-3.") is -3.0
  p("-abc3.3") is "-abc3.3"

  p("())") raises "Invalid"
  p("('a' 5)") raises "'quote'"
  p("(a") raises "Invalid"
  p(")") raises "Invalid"
  p("('a)") raises "Invalid"
  p("(()") raises "Invalid"
  p("(a')") raises "Invalid"

end


data Value:
  | numV(value :: Number)
  | strV(value :: String)
  | funV(params :: List<String>, body :: Expr, env :: Env)
end

data Env:
  | mt-env
  | an-env(name :: String, val :: Value, env :: Env)
end

data Expr:
  | id(name :: String)
  | num(value :: Number)
  | str(value :: String)
  | bop(op :: Operator, left :: Expr, right :: Expr)
  | cif(cond :: Expr, consq :: Expr, altern :: Expr)
  | elet(name :: String, expr :: Expr, body :: Expr)
  | elam(params :: List<String>, body :: Expr)
  | app(func :: Expr, args :: List<Expr>)
end

data Operator:
  | plus
  | minus
  | append
  | str-eq
end
  
fun parse(prog) -> Expr:
  doc: "Parse an s-expr in Paret's concrete syntax into an Expr."
  
  fun check-params(params :: List<String>) -> List<String>:
    doc: "Ensure that a function has no duplicate parameter names."
    for each(param from params):
      when params.filter(lam(x): x == param end).length() > 1:
        raise("parse: function has duplicate parameter " + param)
      end
    end
    params
  end

  fun convert(sexpr):
    doc: "Convert an s-expression into an Expr."
    if lists.is-List(sexpr):
      head = sexpr.first
      if head == "string":
        str(sexpr.get(1))
      else if head == "if":
        cif(convert(sexpr.get(1)),
        convert(sexpr.get(2)),
            convert(sexpr.get(3)))
      else if head == "let":
        if lists.is-List(sexpr.get(1)):
          elet(sexpr.get(1).get(0),
              convert(sexpr.get(1).get(1)),
              convert(sexpr.get(2)))
        else:
          elet(sexpr.get(1),
              convert(sexpr.get(2)),
              convert(sexpr.get(3)))
        end
      else if head == "fun":
        elam(check-params(sexpr.get(1)), convert(sexpr.get(2)))
      else if head == "+":
        bop(plus, convert(sexpr.get(1)), convert(sexpr.get(2)))
      else if head == "-":
        bop(minus, convert(sexpr.get(1)), convert(sexpr.get(2)))
      else if head == "++":
        bop(append, convert(sexpr.get(1)), convert(sexpr.get(2)))
      else if head == "==":
        bop(str-eq, convert(sexpr.get(1)), convert(sexpr.get(2)))
      else:
        func = convert(head)
        args = map(convert, sexpr.rest)
        app(func, args)
      end
    else if is-number(sexpr):
      num(sexpr)
    else if is-string(sexpr):
      id(sexpr)
    end
  end
  convert(prog)
where:
  c = lam(s): parse(p(s)) end
  c("5") is num(5)
  c("x") is id("x")
  c("\"x\"") is str("x")
  c("(+ 5 5)") is bop(plus, num(5), num(5))
  c("(let x 5 x)") is elet("x", num(5), id("x"))
  c("(let (x 5) x)") is elet("x", num(5), id("x"))
  c("(a b c)") is app(id("a"), [list: id("b"), id("c")])
end

