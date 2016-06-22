import file("../../../src/arr/compiler/compile-structs.arr") as CS
import file("../test-compile-helper.arr") as C
import load-lib as L
import either as E

check:
  fun c(str) block:
    errs = C.get-compile-errs(str)
    when is-empty(errs):
      print-error("Expected at least one error for running \n\n " + str + "\n\n" + " but got none ")
    end
    errs.first
    #|
    result = C.run-to-result(str)
    cases(CS.CompileResult) result:
      | ok(_) => "No error for " + str
      | err(probs) => probs.first
    end
    |#
  end
  fun cok(str):
    C.get-compile-errs(str)
  end

  fun c-ok(str):
    result = C.compile-str(str)
    cases(CS.CompileResult) result:
      | ok(_) => true
      | err(_) => false
    end
  end

  check:
    "fun f(): ... x ... end" satisfies c-ok
    c("fun f(): ... ... end") satisfies CS.is-wf-err-split
    c("fun f(): 5 3 end") satisfies CS.is-same-line
    c("fun f(): 5 \n 3 end") satisfies CS.is-block-needed
    "fun f() block: 5 \n 3 end" satisfies c-ok
    "fun f(): 5 \n ... \n 3 end" satisfies c-ok
    "fun f(): 5 \n ... end" satisfies c-ok
    "fun f(): 5 \n ... 3 end" satisfies c-ok
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
    c("data D: n() with: method _(self): 5 end end") satisfies CS.is-underscore-as
    c("data D: n() sharing: method _(self): 5 end end") satisfies CS.is-underscore-as
    c("data D: _() sharing: method m(self): 5 end end") satisfies CS.is-underscore-as
    c("data _: d() sharing: method m(self): 5 end end") satisfies CS.is-underscore-as
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
    cok(```
type N = Number
type N2 = N

fun test<A>(a :: A):
 a
end

check:
 test<N>(1) is 1
end    
```) is empty
  end
end
