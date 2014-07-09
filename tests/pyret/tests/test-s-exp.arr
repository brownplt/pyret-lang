import s-exp as S

p = S.read-s-exp
s-list = S.s-list
s-num = S.s-num
s-str = S.s-str
s-sym = S.s-sym

check:
  p("()") is s-list(empty)
  p("(5)") is s-list([list: s-num(5)])
  p("(5 4)") is s-list([list: s-num(5), s-num(4)])
  p("(a 5)") is s-list([list: s-sym("a"), s-num(5)])
  p("a") is s-sym("a")
  p("\"a\"") is s-str("a")
  p("(a (b c))") is
    s-list([list:
      s-sym("a"),
      s-list([list: s-sym("b"), s-sym("c")])
    ])
  p("(\"a\" (5 (4) ()) \"b\")") is
    s-list([list:
      s-str("a"),
      s-list([list:
        s-num(5),
        s-list([list: s-num(4)]),
        s-list(empty)
      ]),
      s-str("b")
    ])

  p("-5") is s-num(-5)
  p("-4.4") is s-num(-4.4)
  p("-3.") is s-num(-3.0)
  # Make sure bignums parse correctly
  p(num-tostring(num-expt(100, 100))) is
    s-num(num-expt(100, 100))
  p("-abc3.3") is s-sym("-abc3.3")

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
  
  fun check-params(maybe-params :: S.S-Exp) -> List<String>:
    doc: "Ensure that a function has no duplicate parameter names."
    cases(S.S-Exp) maybe-params:
      | s-list(params) =>
        for each(param from params):
          when params.filter(lam(x): x == param end).length() > 1:
            raise("parse: function has duplicate parameter " + param)
          end
        end
        params
      | else => raise("Invalid argument list after fun: " + torepr(maybe-params))
    end
  end

  fun convert(sexpr):
    doc: "Convert an s-expression into an Expr."
    cases(S.S-Exp) sexpr:
      | s-list(elts) =>
        if elts.length() < 1: raise("Empty list")
        else:
          head = elts.first
          cases(S.S-Exp) head:
            | s-sym(maybe-keyword) =>
              if maybe-keyword == "if":
                cif(convert(elts.get(1)),
                convert(elts.get(2)),
                    convert(elts.get(3)))
              else if maybe-keyword == "let":
                cases(S.S-Exp) elts.get(1):
                  | s-list(binding) =>
                    cases(S.S-Exp) binding.first:
                      | s-sym(x) => 
                        elet(x, convert(binding.get(1)), convert(elts.get(2)))
                      | else => raise("Invalid identifier in let binding: " + torepr(binding.first))
                    end
                  | s-sym(x) =>
                    elet(x, convert(elts.get(2)), convert(elts.get(3)))
                  | else => raise("Invalid let binding: " + torepr(elts.get(1)))
                end
              else if maybe-keyword == "fun":
                elam(check-params(elts.get(1)), convert(elts.get(2)))
              else if maybe-keyword == "+":
                bop(plus, convert(elts.get(1)), convert(elts.get(2)))
              else if maybe-keyword == "-":
                bop(minus, convert(elts.get(1)), convert(elts.get(2)))
              else if maybe-keyword == "++":
                bop(append, convert(elts.get(1)), convert(elts.get(2)))
              else if maybe-keyword == "==":
                bop(str-eq, convert(elts.get(1)), convert(elts.get(2)))
              else:
                func = convert(head)
                args = map(convert, elts.rest)
                app(func, args)
              end
            | else =>
              func = convert(head)
              args = map(convert, elts.rest)
              app(func, args)
          end
        end
      | s-num(n) => num(n)
      | s-str(s) => str(s)
      | s-sym(x) => id(x)
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

