#lang pyret/whalesong

####### captain teach ########

C = cs173.interp-basic
parse = cs173.interp-basic.parse

fun eval(prog :: String) -> C.Value:
  interp(parse(read-sexpr(prog)))
end

fun interp(prog :: C.Expr) -> C.Value:
  interp-env(prog, C.mt-env)
end

######## solution ###########

fun interp-env(prog :: C.Expr, env :: C.Env) -> C.Value:
  fun lookup(name-id :: String, env2 :: C.Env) -> C.Value:
    cases(C.Env) env2:
      | mt-env => raise("Unbound identifier: " + name-id)
      | an-env(name, val, rest-env) =>
          if name == name-id:
            val
          else:
            lookup(name-id, rest-env)
          end
    end
  end
  
  fun binOp(binop :: C.Operator, left :: C.Expr, right :: C.Expr, env2 :: C.Env) -> C.Value:
    cases(C.Value) interp-env(left, env2):
      | numV(l-value) =>
          cases(C.Value) interp-env(right, env2):
            | numV(r-value) =>
                cases(C.Operator) binop:
                  | plus => C.numV(l-value + r-value)
                  | minus => C.numV(l-value - r-value)
                  | else => raise("Invalid operator for numbers")
                end
            | else => raise("Argument mismatch: both args must be of the same type")
          end
      | strV(l-value) =>
          cases(C.Value) interp-env(right, env2):
            | strV(r-value) =>
                cases(C.Operator) binop:
                    | append => C.strV(l-value + r-value)
                    | str-eq => C.numV(if l-value == r-value: 1 else: 0 end)
                    | else => raise("Invalid operator for strings")
                end
            | else => raise("Argument mismatch: both args must be of the
  		  same type")
          end
      | funV(_, _, _) => raise("Closure is not a valid argument for binary
  	op")
    end
  end

  cases(C.Expr) prog:
    | id(name) => lookup(name, env)
    | num(value) => C.numV(value)
    | str(value) => C.strV(value)
    | bop(op, left, right) => binOp(op, left, right, env)
    | cif(cond, consq, altern) =>
        cases(C.Value) interp-env(cond, env):
          | numV(value) =>
              if value == 0:
                interp-env(consq, env)
              else:
                interp-env(altern, env)
              end
          | else => raise("Test condition must be a number")
        end
    | let(id-name, expr, body) => interp-env(body, C.an-env(id-name,
	interp-env(expr, env), env))
    | lam(params, body) => C.funV(params, body, env)
    | app(func, args) =>
        cases(C.Value) interp-env(func, env):
          | funV(params, body, cl-env) =>
              when (params.length() <> args.length()):
                raise("Wrong number of arguments passed to function!")
              end
              interp-env(body,
                  list.fold2(
                    fun(base-env, param-name, arg-val):
                      C.an-env(param-name, arg-val, base-env)
                    end,
                    cl-env,
                    params,
                    map(
                      fun(arg-expr): interp-env(arg-expr, env) end,
                      args
                    )
                  )
              )
          | else => raise("My dad's not a phone!  And you can only apply lambdas!")
        end
  end
end


### good test block ###

  #number tests
  test-print(eval('2'))

  #string tests
  test-print(eval('"foo"') == C.strV("foo"))

  #addition / subtraction tests
  test-print(eval('(+ 1 2)') == C.numV(3))
  test-print(eval('(- 4.5 6)') == C.numV(-1.5))
  test-print(eval('(+ (- 3 1) (+ 4 3))') == C.numV(9))

  #string append tests
  test-print(eval('(++ "foo" "bar")') == C.strV("foobar"))
  test-print(eval('(++ (++ "f" "oo") "bar")') == C.strV("foobar"))

  #equality tests
  test-print((eval('(== "12" (++ "1" "2"))') <> C.numV(0)) == true)
  test-print((eval('(== "12" (++ "2" "1"))') == C.numV(0)) == true)
  test-print(checkers.check-raises("", fun(): eval('(== 1 2)') end, "", none))
  test-print(checkers.check-raises("", fun(): eval('(== 1 1)') end, "", none))

  #argument mismatch
  test-print(checkers.check-raises("", fun(): eval('(+ 2 "foo")') end, "", none))
  test-print(checkers.check-raises("", fun(): eval('(== "foo" 2)') end, "", none))
  test-print(checkers.check-raises("", fun(): eval('(- (fun () 0) 2)') end, "", none))
  test-print(checkers.check-raises("", fun(): eval('(== "foo" 0)') end, "", none))
  test-print(checkers.check-raises("", fun(): eval('(++ 0 "foo")') end, "", none))

  #let/id tests
  test-print(eval('(let (x 42) x)') == C.numV(42))
  test-print(eval('(let (x (let (y 5) (+ 2 y))) (+ x 3))') == C.numV(10))
  test-print(eval('(let (x 1) (let (x 2) x))') == C.numV(2))
  test-print(eval('(let (x 1) (let (y 2) (let (x 3) (+ x y))))') == C.numV(5))
  test-print(checkers.check-raises("", fun(): eval('nill') end, "", none))

  #arity check
  test-print(checkers.check-raises("", fun(): eval('(let (foo (fun (x y) 42)) (foo 3))') end, "", none))
  test-print(checkers.check-raises("", fun(): eval('(let (foo (fun () 42)) (foo 3))') end, "", none))

  #function tests
  test-print(eval('(let (foo (fun (x) 42)) (foo 3))') == C.numV(42))
  test-print(eval('(let (outer_var 8) (let (foo (fun (x y) 42)) (foo 3 4)))')
    == C.numV(42))
  test-print(eval("(let (x 3) (let (y (fun (z) (+ x z))) (y x)))") == C.numV(6))
  test-print(eval("(let (x (fun (y) y)) (x x))")
    == C.funV(["y"], C.id("y"), C.mt-env))
  test-print(eval("(let (x 2) (let (y (fun () x)) y))")
    == C.funV([], C.id("x"), C.an-env("x", C.numV(2), C.mt-env)))
  test-print(eval("(let (x 2) (let (y (fun () x)) (y)))")
    == C.numV(2))

  #closure test
  test-print(eval('(let (f (fun (x) (fun (y) (+ x y))))
             (let (g (f 1))
                  (+ (g 2) (g 3))))')
    == C.numV(7))
  test-print(checkers.check-raises("", fun(): eval('(let (f (fun (x) 1)) (f x))') end, "", none))
  test-print(checkers.check-raises("", fun(): eval('(let (f (fun (x y) 1)) (f 1 x))') end, "", none))

  #cif test
  test-print(eval('(if 2 1 2)') == C.numV(1))
  test-print(eval('(if 0 1 2)') == C.numV(2))
  test-print(checkers.check-raises("", fun(): eval('(if 0 2 null)') end, "", none))
  test-print(checkers.check-raises("", fun(): eval('(if (fun () 2) 1 2)') end, "", none))
