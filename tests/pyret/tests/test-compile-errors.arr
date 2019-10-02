import file("../../../src/arr/compiler/compile-structs.arr") as CS
import file("../test-compile-helper.arr") as C

run-str = C.run-str
compile-error = C.compile-error
output = C.output
cmsgs = C.compile-error-messages

fun cok(str):
  C.get-compile-errs(str)
end

fun contain-all(haystack, needles):
  for lists.all(needle from needles):
    for lists.any(hay from haystack):
      string-contains(hay, needle)
    end
  end
end

fun contain-none(haystack, needles):
  for lists.all(needle from needles):
    for lists.all(hay from haystack):
      not(string-contains(hay, needle))
    end
  end
end

check:
  cok("fun f(x): ... x ... end") is empty
  run-str("fun f(): ... ... end") is%(output) compile-error(CS.is-template-same-line)
  run-str("fun f(): block: 5 3 end end") is%(output) compile-error(CS.is-same-line)
  run-str("fun f(): 5 \n 3 end") is%(output) compile-error(CS.is-block-needed)
  cok("fun f() block: num-sqr(5) \n 3 end") is empty
  cok("fun f(): 5 \n ... \n 3 end") is empty
  cok("fun f(): 5 \n ... end") is empty
  cok("fun f(): 5 \n ... 3 end") is empty
end

check "provides":
  run-str("provide _ end") is%(output) compile-error(CS.is-non-object-provide)
  run-str("provide a end") is%(output) compile-error(CS.is-non-object-provide)
end

check "underscores":
  run-str("a = _") is%(output) compile-error(CS.is-underscore-as-expr)
  run-str("when _: 5 end") is%(output) compile-error(CS.is-underscore-as-expr)
  run-str("if _: 5 else: 10 end") is%(output) compile-error(CS.is-underscore-as-expr)
  run-str("cases(List) _: | empty => true end") is%(output) compile-error(CS.is-underscore-as-expr)
  run-str("cases(List) 5: | empty => _ end") is%(output) compile-error(CS.is-underscore-as-expr)
  run-str("check: _ end") is%(output) compile-error(CS.is-wf-err)
  run-str("fun f(): _ end") is%(output) compile-error(CS.is-underscore-as-expr)
  run-str("lam(): _ end") is%(output) compile-error(CS.is-underscore-as-expr)
  run-str("method(self): _ end") is%(output) compile-error(CS.is-underscore-as-expr)
  run-str("{x: _}") is%(output) compile-error(CS.is-underscore-as-expr)

  run-str("a :: _ = 5") is%(output) compile-error(CS.is-underscore-as-ann)
  run-str("a :: _.List = 5") is%(output) compile-error(CS.is-underscore-as-ann)
end

check "shadowing":
  cmsgs("some = 5") is%(contain-all)
  [list:
    "declaration of `some` at ", "shadows a previous declaration of `some` defined at builtin://option"
  ]
  cmsgs("some = 5") is%(contain-none) [list: "and imported from" ]

  cmsgs("import some from option\nsome = 5") is%(contain-all)
  [list:
    "declaration of `some` at ", "shadows a previous declaration of `some` defined at builtin://option", "and imported from"
  ]
end

check "duplicate data fields":
  run-str("data Node: node(a, a, a) end") is%(output) compile-error(CS.is-duplicate-id)
  run-str("data Node: node(a, b, a) end") is%(output) compile-error(CS.is-duplicate-id)
  run-str("data Node: node(a :: Number, a :: String) end") is%(output) compile-error(CS.is-duplicate-id)
  run-str("data Node: node(z, a, a, c) end") is%(output) compile-error(CS.is-duplicate-id)
end

check "underscore data fields":
  run-str("data Node: node(_) end") is%(output) compile-error(CS.is-underscore-as)
  run-str("data Node: node(_, _) end") is%(output) compile-error(CS.is-underscore-as)
  run-str("data Node: node(a, _) end") is%(output) compile-error(CS.is-underscore-as)
  run-str("data Node: node(_, a) end") is%(output) compile-error(CS.is-underscore-as)
end

check "underscore object fields":
  run-str("{_: 5}") is%(output) compile-error(CS.is-underscore-as)
  run-str("data D: n() with: method _(self): 5 end end") is%(output) compile-error(CS.is-underscore-as)
  run-str("data D: n() sharing: method _(self): 5 end end") is%(output) compile-error(CS.is-underscore-as)
  run-str("data D: _() sharing: method m(self): 5 end end") is%(output) compile-error(CS.is-underscore-as)
  run-str("data _: d() sharing: method m(self): 5 end end") is%(output) compile-error(CS.is-underscore-as)
end

check "tuple duplicate names":
  run-str("fun f({k;v;}, {a;k;c;}): a + c end") is%(output) compile-error(CS.is-duplicate-id)
  run-str("fun f({a;a;}, {x;y;z;}): z end") is%(output) compile-error(CS.is-duplicate-id)
  run-str("fun f(w, {k; w;}): k end") is%(output) compile-error(CS.is-duplicate-id)
end

check "unbound ids in provides":
  run-str("provide: u end") is%(output) compile-error(CS.is-unbound-id)
end

check "unbound type ids":
  run-str("link<NotDefined>(1, empty)") is%(output) compile-error(CS.is-unbound-type-id)
  run-str(```
fun test<A>(a :: A):
 a
end

check:
 test<A>(1) is 1
end    
```) is%(output) compile-error(CS.is-unbound-type-id)
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
