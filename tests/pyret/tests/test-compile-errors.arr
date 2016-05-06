import "compiler/compile-structs.arr" as CS
import "../test-compile-helper.arr" as C

check:
  fun c(str):
    result = C.compile-str(str)
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
    c("data Node: node(a, a, a) end") satisfies CS.is-duplicate-id
    c("data Node: node(a, b, a) end") satisfies CS.is-duplicate-id
    c("data Node: node(a :: Number, a :: String) end") satisfies CS.is-duplicate-id
    c("data Node: node(z, a, a, c) end") satisfies CS.is-duplicate-id
  end

  check "underscore data fields":
    c("data Node: node(_) end") satisfies CS.is-underscore-as
    c("data Node: node(_, _) end") satisfies CS.is-underscore-as
    c("data Node: node(a, _) end") satisfies CS.is-underscore-as
    c("data Node: node(_, a) end") satisfies CS.is-underscore-as
  end

  check "underscore object fields":
    c("{_: 5}") satisfies CS.is-underscore-as
    c("data D: n() with: _(self): 5 end end") satisfies CS.is-underscore-as
    c("data D: n() sharing: _(self): 5 end end") satisfies CS.is-underscore-as
    c("data D: _() sharing: m(self): 5 end end") satisfies CS.is-underscore-as
    c("data _: d() sharing: m(self): 5 end end") satisfies CS.is-underscore-as
  end

  check "unbound type ids":
    c("link<NotDefined>(1, empty)") satisfies CS.is-unbound-type-id
    c(```
fun test<A>(a :: A):
 a
end

check:
 test<A>(1) is 1
end    
```) satisfies CS.is-unbound-type-id
  end

  check "bound type aliases":
    c(```
type N = Number
type N2 = N

fun test<A>(a :: A):
 a
end

check:
 test<N>(1) is 1
end    
```) satisfies string-contains(_, "No error")
  end
end
