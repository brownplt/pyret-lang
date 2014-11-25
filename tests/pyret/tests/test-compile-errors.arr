import "compiler/compile.arr" as CM
import "compiler/compile-structs.arr" as CS

compile-str = lam(str):
  CM.compile-js(
          CM.start,
          "Pyret",
          str,
          "test",
          CS.standard-builtins,
          {
            check-mode : true,
            allow-shadowed : false,
            collect-all: false,
            type-check: false,
            ignore-unbound: false
          }
          ).result
end

check:
  fun c(str):
    result = compile-str(str)
    result satisfies CS.is-err
    cases(CS.CompileResult) result:
      | ok(_) => "No error for " + str
      | err(probs) => probs.first
    end
  end

  check "underscores":
    c("a = _") satisfies CS.is-underscore-as-expr
    c("when _: 5 end") satisfies CS.is-underscore-as-expr
    c("if _: 5 else: 10 end") satisfies CS.is-underscore-as-expr
    c("cases(List) _: | empty => true end") satisfies CS.is-underscore-as-expr
    c("cases(List) l: | empty => _ end") satisfies CS.is-underscore-as-expr
    c("check: _ end") satisfies CS.is-underscore-as-expr
    c("fun f(): _ end") satisfies CS.is-underscore-as-expr
    c("lam(): _ end") satisfies CS.is-underscore-as-expr
    c("method(self): _ end") satisfies CS.is-underscore-as-expr
    c("{x: _}") satisfies CS.is-underscore-as-expr
    c("provide _ end") satisfies CS.is-underscore-as-expr

    c("a :: _ = 5") satisfies CS.is-underscore-as-ann
    c("a :: _.List = 5") satisfies CS.is-underscore-as-ann
  end

  check "duplicate data fields":
    c("data Node: node(a, a, a) end") satisfies CS.is-wf-err-split
    c("data Node: node(a, b, a) end") satisfies CS.is-wf-err-split
    c("data Node: node(a :: Number, a :: String) end") satisfies CS.is-wf-err-split
    c("data Node: node(z, a, a, c) end") satisfies CS.is-wf-err-split
  end

  check "underscore data fields":
    c("data Node: node(_) end") satisfies CS.is-wf-err
    c("data Node: node(_, _) end") satisfies CS.is-wf-err
    c("data Node: node(a, _) end") satisfies CS.is-wf-err
    c("data Node: node(_, a) end") satisfies CS.is-wf-err
  end
end
