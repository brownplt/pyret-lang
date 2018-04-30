import valueskeleton as VS
import ds-desugar as DS
import ds-resugar as RS
include ds-structs
import parse-ast, parse-ds-rules from ds-parse
import filter-map from lists
include string-dict

fun attach-tags(tags, e):
  for fold(acc from e, {lhs; rhs} from tags):
    g-tag(lhs, rhs, acc)
  end
end

fun extend-trace(tags, ctx, e, trace):
  link(ctx(g-focus(attach-tags(tags, e))), trace)
end

data Closure:
  | closure(id :: String, body :: Term)
sharing:
  method _output(self): VS.vs-str("<closure>") end
end

fun interp(e :: Term, tags :: List, ctx :: (Term -> Term), trace :: List<Term>, env :: StringDict<Term>) -> {Term; List<Term>} block:
  #print("<<< e: " + tostring(e) + "\n<<< tags: " + tostring(tags) + "\n<<< ctx: " + tostring(ctx(g-option(none))) + "\n<<< trace: " + tostring(trace) + "\n\n")
  fun step-to(val, shadow trace):
    v = g-value(val)
    {v; extend-trace(empty, ctx, v, trace)}
  end

  cases (Term) e:
    | g-tag(lhs, rhs, body) =>
      interp(body, link({lhs; rhs}, tags), ctx, trace, env)
    | g-core(c, args) =>
      ask block:
        | c == "bool" then:   step-to(args.get(1).val.b, trace)
        | c == "num" then:    step-to(args.get(1).val.n, trace)
        | c == "id" then:     step-to(env.get-value(args.get(1).v.name).val, trace)
        | c == "lambda" then: step-to(closure(args.get(1).v.name, args.get(2)), trace)
        | c == "if" then:
          cond-ctx = lam(shadow e): ctx(attach-tags(tags, g-core("if",
            [list: term-dummy-loc, e, args.get(2), args.get(3)]))) end
          {cond; shadow trace} = interp(args.get(1), empty, cond-ctx, trace, env)
          if cond == g-value(true):
            interp(args.get(2), empty, ctx, trace, env)
          else:
            interp(args.get(3), empty, ctx, trace, env)
          end
        | c == "app" then:
          f-ctx = lam(shadow e): ctx(attach-tags(tags, g-core("app",
            [list: term-dummy-loc, e, args.get(2)]))) end
          {f-val; shadow trace} = interp(args.get(1), empty, f-ctx, trace, env)
          arg-ctx = lam(shadow e): ctx(attach-tags(tags, g-core("app",
            [list: term-dummy-loc, f-val, e]))) end
          {arg-val; shadow trace} = interp(args.get(2), empty, arg-ctx, trace, env)
          shadow env = env.set(f-val.val.id, arg-val)
          interp(f-val.val.body, empty, ctx, trace, env)
        | c == "add" then:
          l-ctx = lam(shadow e): ctx(attach-tags(tags, g-core("add",
            [list: term-dummy-loc, e, args.get(2)]))) end
          {l-val; shadow trace} = interp(args.get(1), empty, l-ctx, trace, env)
          r-ctx = lam(shadow e): ctx(attach-tags(tags, g-core("add",
            [list: term-dummy-loc, l-val, e]))) end
          {r-val; shadow trace} = interp(args.get(2), empty, r-ctx, trace, env)
          step-to(l-val.val + r-val.val, trace)
      end
  end
end

fun print-terms(s):
  lam(v) block:
    print(s + ":\n")
    for each(x from v):
      print(">>> " + tostring(x) + "\n")
    end
    print("\n\n\n")
    v
  end
end

fun resugar(x) block:
  #print("------------------------------------------------\n\n")
  ret = RS.resugar(x)
  #print("------------------------------------------------\n\n")
  ret
end

fun eval(e :: Term) -> List<Term> block:
  interp(e, empty, lam(x): x end, empty, [string-dict: ]).{1}.reverse()
    # ^ print-terms("before")
    ^ filter-map(resugar, _)
    # ^ print-terms("after")
    ^ map(strip-tags, _)
end

fun eval-s(s :: String) -> List<Term>:
  eval(parse-ast(s))
end

#|
check:
  eval-s("<bool true>") is [list: parse-ast("<bool true>")]
  eval-s("<if <bool true> <bool false> <bool true>>")
    is [list:
      parse-ast("<if <bool true> <bool false> <bool true>>"),
      parse-ast("<bool false>")]
  eval-s("<if <bool false> <bool false> <bool true>>")
    is [list:
      parse-ast("<if <bool false> <bool false> <bool true>>"),
      parse-ast("<bool true>")]
end
|#

desugaring-rules = parse-ds-rules(```
sugar not:
  | (not x) => (if x (bool false) (bool true))
end

sugar and:
  | (and x y) => (if x y (bool false))
end

sugar binding:
  | (binding x v) => {binding x v}
end

sugar let:
  | (let {binding x v} b) => (app (lambda x b) v)
end

sugar or:
  | (or a b) => (fresh [x] (let (binding x a) (if (id x) (id x) b)))
end

sugar comment:
  | (comment msg x) => x
end
  ```)

fun desugar(e :: Term) -> Term:
  DS.desugar(desugaring-rules, e)
end

fun eval-ds(s :: String):
  eval(desugar(parse-ast(s)))
end

check:
  nothing
  #|
  eval-ds("(bool true)") is [list: parse-ast("(bool true)")]
  eval-ds("(if (bool true) (bool false) (bool true))")
    is [list:
      parse-ast("(if (bool true) (bool false) (bool true))"),
      parse-ast("(bool false)")]
  eval-ds("(if (bool false) (bool false) (bool true))")
    is [list:
      parse-ast("(if (bool false) (bool false) (bool true))"),
      parse-ast("(bool true)")]

  eval-ds("(and (bool false) (bool true))") is [list:
      parse-ast("(and (bool false) (bool true))"),
      parse-ast("(bool false)")]


  eval-ds("(and (bool true) (bool false))") is [list:
      parse-ast("(and (bool true) (bool false))"),
      parse-ast("(bool false)")]

  eval-ds("(and (bool true) (bool true))") is [list:
      parse-ast("(and (bool true) (bool true))"),
      parse-ast("(bool true)")]
|#
end

fun show-seq(s) block:
  for each(x from eval-ds(s)):
    print(">>> " + show-term(x) + "\n")
  end
  print("\n\n")
end

# show-seq("(or (bool false) (bool true))")

show-seq("(if (and (not (bool true)) (not (bool true))) (not (bool false)) (bool false))")

show-seq("(if (not (and (not (bool true)) (not (bool true)))) (not (bool false)) (bool false))")

show-seq("(app (lambda x (not (id x))) (not (bool true)))")

show-seq("(let (binding x (not (bool true))) (and (id x) (bool false)))")

show-seq("(let (binding x (add (num 1) (num 2))) (add (id x) (id x)))")

show-seq("(add (add (num 1) (num 2)) (num 4))")

show-seq("(comment \"'should be 7'\" (add (add (num 1) (num 2)) (num 4)))")
