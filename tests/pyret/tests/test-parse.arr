import file("../test-parse-helper.arr") as P

does-parse = P.does-parse
wss = P.wss
en-ops = P.en-ops

check "should lex triple-quoted strings":
  does-parse("```asd`asd```") is true
  does-parse("```asd``asd```") is true
  does-parse("```asd``\\`asd```") is true
  does-parse("```asd``\\````") is true
  does-parse("```asd```asd```") is false
end

check "should parse lets and letrecs":
  does-parse("let: 10 end") is false
  does-parse("letrec: 10 end") is false
  does-parse("let x = 10, y = 12: x + y end") is true
  does-parse("let x = 10, y = 12, z = 13: BAMBOOZLE end") is true
  does-parse("letrec x = 10, y = 12: x + y end") is true
  does-parse("letrec z = 62, x = 10, y = 12: x + y end") is true
end

check "should parse type-lets":
  does-parse("type-let t1 = Number, t2 = String: 5 end") is true
  does-parse("type-let t1 = Number: 10 end") is true
  does-parse("type-let: 10 end") is false
  does-parse("type-let newtype List as ListT: {} end") is true
  does-parse("type-let newtype List as ListT, thing = foo: {} end") is true
end

check "should parse standalone types":
  does-parse("type foo = { x :: Number }") is true
  does-parse("type foo = Number -> String") is false
end

check "should parse standalone newtypes":
  does-parse("newtype Foo as FooT") is true
  does-parse("newtype (Number -> String)") is false
end

check "should parse provide-types":
  does-parse("provide-types { List :: List }") is true
  does-parse("provide-types { List :: List, x :: (Number -> String) }") is true
end

check "shouldn't parse expressions in provide-types":
  does-parse("provide-types { List :: 5 + 5 }") is false
  does-parse("provide-types { List :: List, x :: lam(x): x end }") is false
end

check "shouldn't allow English ops as identifiers, no matter the whitespace":
  for map(op from en-ops) block:
    does-parse(op + "=" + "false") is false
    for map(ws from wss) block:
      does-parse("(" + op + ws + op + ws + op + ")") is false
      does-parse(op + ws + "="      + "false") is false
      does-parse(op +      "=" + ws + "false") is false
      does-parse(op + ws + "=" + ws + "false") is false
    end
  end
end

check "shouldn't allow hyphens at the beginning or end of identifiers":
  does-parse("-") is false
  does-parse("(- -)") is false
  does-parse("--") is false
  does-parse("(-- --)") is false
  does-parse("a- b") is false
  does-parse("a -b") is false
  does-parse("a- = b") is false
  does-parse("-a = b") is false
  does-parse("a-a") is true
  does-parse("a-a-a") is true
  does-parse("a--aa") is true
  does-parse("aa--a") is true
end

check "should allow English ops with all manner of surrounding whitespace and parens":
  for map(op from en-ops) block:
    does-parse("(false)" + op            ) is false
    does-parse(            op + "(false)") is false
    does-parse("(false)" + op + "(false)") is true
    for map(ws from wss) block:
      does-parse("(false)" + ws + op                 ) is false
      does-parse(                 op + ws + "(false)") is false
      does-parse("(false)" + ws + op      + "(false)") is true
      does-parse("(false)" +      op + ws + "(false)") is true
      does-parse("(false)" + ws + op + ws + "(false)") is true
    end
  end
end

check "should notice parse errors":
  does-parse("bad end") is false
  does-parse("provide-types { List :: List } end") is false
end

check "should parse all comma-separated things":
  does-parse('import foo from quuz') is true
  does-parse('import foo, bar, baz from quuz') is true
  does-parse('import foo("bar") as floo') is true
  does-parse('import foo("bar", "baz", "quuz") as floo') is true
  does-parse('let x = 5: true end') is true
  does-parse('let x = 5, y = 4,z=2: true end') is true
  does-parse('letrec x = 5: true end') is true
  does-parse('letrec x = 5, y = 4,z=2: true end') is true
  does-parse('type-let foo=Any: true end') is true
  does-parse('type-let Foo=Any, Bar = Foo<Number>: true end') is true
  does-parse('fun foo<A>(): 5 end') is true
  does-parse('fun foo<A, B, C>(): 5 end') is true
  does-parse('fun foo<A>(a): 5 end') is true
  does-parse('fun foo<A, B, C>(a, b, c): 5 end') is true
  does-parse('data Foo: var1() end') is true
  does-parse('data Foo: var1(a) end') is true
  does-parse('data Foo: var1(a, b, c) end') is true
  does-parse('foo()') is true
  does-parse('foo(1)') is true
  does-parse('foo(1, 2, 3)') is true
  does-parse('foo<>') is false
  does-parse('foo<A>') is true
  does-parse('foo<A, B, C>') is true
  does-parse('{}') is true
  does-parse('{foo: 5}') is true
  does-parse('{foo: 5, bar: 6, baz: 7}') is true
  does-parse('{foo: 5, bar: 6, baz: 7,}') is true
  does-parse('a.{}') is false
  does-parse('a.{foo: 5}') is true
  does-parse('a.{foo: 5, bar: 6, baz: 7}') is true
  does-parse('a.{foo: 5, bar: 6, baz: 7,}') is true
  does-parse('[foo: ]') is true
  does-parse('[foo: 5]') is true
  does-parse('[foo: 5, 6, 7]') is true
  does-parse('cases(Foo) bar: | foo() => true end') is true
  does-parse('cases(Foo) bar: | foo(a) => true end') is true
  does-parse('cases(Foo) bar: | foo(a, ref b, c) => true end') is true
  does-parse('for map(): 5 end') is true
  does-parse('for map(a from b): 5 end') is true
  does-parse('for map(a from b, c from d, e from f): 5 end') is true
  does-parse('x :: {} = 5') is true
  does-parse('x :: {foo:: A} = 5') is true
  does-parse('x :: {foo:: A, bar :: B, baz:: C} = 5') is true
  does-parse('x :: -> A') is true
  does-parse('x :: A -> A') is true
  does-parse('x :: A, A -> A') is true
  does-parse('x :: A<( -> A)>') is true
  does-parse('x :: A<( -> A), B>') is true
end

check "should parse angle brackets without whitespace only as type instantiations":
  does-parse("map<A>") is true
  does-parse("(map<A>)") is true
  does-parse("(map<A, B>)") is true
  does-parse("map<A, B>(id)") is true
  does-parse("(map < A, B > (id))") is false
  does-parse("(map\n<\nA, B\n>\n(id))") is false
  does-parse("map<A,\nB>(id)") is true
end

check "should parse angle brackets without whitespace in annotations only as type function application":
  does-parse("a :: List < A > = a") is false
  does-parse("a :: List < A, B > = a") is false
  does-parse("a :: List<A> = a") is true
  does-parse("a :: List<A, B> = a") is true
end

check "should parse angle brackets with whitespace as gt/lt":
  does-parse("1\n<\n2 or false\n B > (id)") is true
  does-parse("1<\n2 or false, B > (id)") is false
end

check "should not care about whitespace and angle brackets in declarations":
  does-parse("fun print<A>(): end") is true
  does-parse("fun print< A>(): end") is true
  does-parse("fun print <A>(): end") is true
  does-parse("fun print < A>(): end") is true
  does-parse("fun print<A >(): end") is true
  does-parse("fun print< A >(): end") is true
  does-parse("fun print <A >(): end") is true
end

check "should not treat (...) after operators as application":
  does-parse("(true) or (false)") is true
  does-parse("(true) < (false)") is true
  does-parse("(true) > (false)") is true
end

check "should not mind end at EOF":
  does-parse("lam<T>(x :: T) -> T: x end") is true
end

check "should not mind end at EOL, and then another statement":
  a = "  fun x<T>(x :: T) -> T: x end"
  does-parse("block:\n" + a + "\n" + a + " end") is true
  does-parse("block:\n" + a + " \n" + a + " end") is true
end

check "should require whitespace after :: and =>":
  does-parse("cases (T) x: | Foo() =>(true) end") is false
  does-parse("cases (T) x: | Foo() => (true) end") is true
  does-parse("cases (T) x: | Foo() =>\n(true) end") is true
  does-parse("block: dog ::Cat = really-huh end") is false
  does-parse("block: dog :: Cat = really-huh end") is true
  does-parse("block: dog :: Cat =\nreally-huh end") is true
end

check "should treat (...) as grouping after ,":
  does-parse("[list: x,(x)]") is true
  does-parse("[list: x , (x)]") is true
  does-parse("[list: x ,\n(x)]") is true
end

check "should treat (...) as grouping after :":
  does-parse("{ asdf:(asdf) }") is true
  does-parse("{ asdf : (asdf) }") is true
  does-parse("{ asdf :\n(asdf) }") is true
  does-parse("fun f(x):\nx\nwhere:(f(5)) is 5\nend") is true
  does-parse("check:(5) is 5 end") is true
  does-parse("examples:(5) is 5 end") is true
  does-parse("ask:\n  | false then: 1\n  | otherwise:(true)\nend") is true
  does-parse("ask:\n  | true then:(1)\nend") is true
  does-parse("if true: 1 else:(1) end") is true
  does-parse("block:(5) end") is true
  does-parse("ask:\n  |(true) then: 1\nend") is true
end

check "should treat (...) as grouping after =":
  does-parse("block: x=(x) end") is true
  does-parse("block: x = (x) end") is true
  does-parse("block: x =\n(x) end") is true
end

check "should treat (...) as grouping after :=":
  does-parse("block: x:=(x) end") is true
  does-parse("block: x := (x) end") is true
  does-parse("block: x :=\n(x) end") is true
end

check "should treat (...) as grouping after ;":
  does-parse("block: lam(x): x end(x)end") is true
  does-parse("block: lam(x): x end (x)end") is true
  does-parse("block: lam(x): x end\n(x)end") is true
end

check "should treat (...) as grouping or as args within {...}":
  does-parse("{(): true}") is true
  does-parse("{(a): true}") is true
  does-parse("{(a, b): true}") is true
  does-parse("{ (): true}") is false
  does-parse("{ (a): true}") is false
  does-parse("{ (a, b): true}") is false
  does-parse("{(1 + 2); (3 + 4)}") is true
  does-parse("{ (1 + 2); (3 + 4) }") is true
end

check "should parse get-bang":
  does-parse("o!x") is true
  does-parse("y.x!x") is true
end

check "should parse update":
  does-parse("o!{x:5}") is true
  does-parse("y!{x:5, y:10}") is true
end

check "should parse ref fields in data definitions":
  does-parse("data D: d(ref x) end") is true
  does-parse("data D: d(ref x :: Number % (is-odd)) end") is true
  does-parse("data D: d(ref x, ref y :: Number) end") is true
  does-parse("data D: | d(ref x :: Boolean, ref y) end") is true
end

check "should parse ref fields in object literals":
  does-parse("{ref x :: Number: 22}") is true
  does-parse("{ref x: 22}") is true
  does-parse("{ref x: 22, y: \"a\"}") is true
  does-parse("{ref x: 22, ref y: \"a\"}") is true
  does-parse("{ref x: 22, ref y :: String: \"a\"}") is true
  does-parse("{ref x :: { z :: Number}: 22, ref y :: String: \"a\"}") is true
  does-parse("{x :: Number: 5}") is false
  does-parse("{ ref ref y :: String: 5 }") is false
  does-parse("{ ref ref: 5 }") is false
end

check "should parse imports":
  does-parse('import modname as G') is true
  does-parse('import file("modname.arr") as G') is true
  does-parse('import file-is-not-special("modname.arr") as G') is true
  does-parse('import gdrive(a) as G') is false
  does-parse('import gdrive("a") as G') is true
  does-parse('import gdrive("a", "b") as G') is true
  does-parse('import gdrive() as G') is false
end

check "should parse includes":
  does-parse('include modname') is true
  does-parse('include file("modname.arr")') is true
  does-parse('include file-is-not-special("modname.arr")') is true
  does-parse('include gdrive(a)') is false
  does-parse('include gdrive("a")') is true
  does-parse('include gdrive("a", "b")') is true
  does-parse('include gdrive()') is false
end

check "should parse new equality operators":
  does-parse('o <=> o2') is true
  does-parse('o <= > o2') is false
  does-parse('o < = > o2') is false
  does-parse('o < => o2') is false
  does-parse('o =~ o2') is true
  does-parse('o == o2') is true
  does-parse('check: o is== o2 end') is true
  does-parse('check: o is == o2 end') is false
  does-parse('check: o is=~ o2 end') is true
  does-parse('check: o is =~ o2 end') is false
  does-parse('check: o is<=> o2 end') is true
  does-parse('check: o is <=> o2 end') is false
  does-parse('check: o is-not== o2 end') is true
  does-parse('check: o is-not == o2 end') is false
  does-parse('check: o is-not=~ o2 end') is true
  does-parse('check: o is-not =~ o2 end') is false
  does-parse('check: o is-not<=> o2 end') is true
  does-parse('check: o is-not <=> o2 end') is false
end

check "should parse examples":
  does-parse('examples: 5 is 5 end') is true
end

check "should parse ref cases bindings":
  does-parse('cases(List) l: | link(ref first, rest) => 5 end') is true
  does-parse('cases(List) l: | link(ref first, ref rest) => 5 end') is true
  does-parse('cases(List) l: | link(first, ref rest) => 5 end') is true
  does-parse('cases(List) l: | link(ref first :: Number, rest) => 5 end') is true
  does-parse('cases(List) l: | link(ref first :: Number, rest :: Number) => 5 end') is true
  does-parse('cases(List) l: | link(first :: Number, ref rest :: Number) => 5 end') is true
  does-parse('cases(List) l: | link(ref first :: Number, ref rest :: Number) => 5 end') is true
  does-parse('cases(List) l: link(ref ref) => 5 end') is false
end

check "should parse type parameters on methods":
  does-parse('method<a>(self): self end') is true
  does-parse('method<a,b>(self): self end') is true
  does-parse('method<a,b,c>(self): self end') is true
  does-parse('{ method m<a>(self): self end }') is true
  does-parse('{ method m<a,b>(self): self end }') is true
  does-parse('{ method m<a,b,c>(self): self end }') is true
  does-parse('data D: | var1 with: method m<a>(self): 5 end sharing: method m2<a>(self): 5 end end') is true
  does-parse('data D: | var1 with: method m<a,b>(self): 5 end sharing: method m2<a,b>(self): 5 end end') is true
  does-parse('data D: | var1 with: method m<a,b,c>(self): 5 end sharing: method m2<a,b,c>(self): 5 end end') is true
end

check "should parse rec statements":
  does-parse('rec a = 10') is true
  does-parse('rec ohn = lz(1, lam(): ohn end)') is true
  does-parse('rec = 5') is false
end

check "should not parse bracket exprs":
  does-parse('o.[x]') is false
end

check "should not parse string keys":
  does-parse('{"x x": true}') is false
  does-parse("{'x x': true}") is false
end

check "should parse block comments":
  does-parse('#| anything |#') is true
  does-parse('#| even with  | pipes |#') is true
  does-parse('#|||#') is true
  does-parse('#||||||#') is true
  does-parse('#| | # | # | # | # |#') is true
  does-parse('#| back to |##| back |#') is true
  does-parse('#||##||#') is true
  does-parse('#|\n|#') is true
  does-parse('#||#') is true
  does-parse(' #||#') is true
  does-parse('\n#||#') is true
  does-parse('\r\n#||#') is true
  does-parse('#| #| |#') is false
  does-parse('#|#||#') is false
  does-parse('#|#|#') is false
  does-parse('x = #| not #| parsed |# here either |# 5') is true
  does-parse('#| |# # extra hash for line comment') is true
  does-parse("#| |# closing hash doesn't count as line comment") is false
  does-parse('#| |#\nfun f():\n  5\nend\n#| |#') is true
  does-parse('#| |#\nfun f():\n  5 + #| |#\n    5\nend\n#| |#') is true
  does-parse('lam(x #| stuff |#, y): x + y end') is true
  does-parse('lam(x #| two |##| comments|#, y): x + y end') is true
  does-parse('#| |# |#') is false
end

check "should not ignore the line after an empty hash comment":
  does-parse('#\n1') is true
  does-parse('#\n{1:2}') is false
end

check "should parse tables":
  does-parse("table 3: row 3 end") is false
  does-parse("table: h1 row: end") is true
  does-parse("table: row: 3 end") is true
  does-parse("table: h1 h2 row: 3 row: 4 end") is false
  does-parse("table: h1 row: 3 3 row: 4 end") is false
  does-parse("table: h1 row: 3 row: 4 4 end") is false
  does-parse("table: h1 h2 row 3, 4 end") is false
  does-parse("table: h1 row: 3 end") is true
  does-parse("table: h1 row: 3 row: 4 end") is true
  does-parse("table: h1, h2 row: 3, 3 row: 4, 4 end") is true
  does-parse("table: h1, h2 row: 3 + 3, 3 * 3 end") is true
end

check "should parse table loaders":
  does-parse("load-table: h1, h2, h3 source: my-src end") is true
  does-parse("load-table: h1 sanitize h1 using s1 source: s sanitize h2 using s2 end") is true
  does-parse("load-table: end") is true
  does-parse("load-table: source: s end") is true
  does-parse("load-table: h1 sanitize h1 using s1 end") is true
  does-parse("load-table: h1 using s1 source: src end") is false
  does-parse("load-table: h1 h2 source: s end") is false
end

check "should parse tuples":
  does-parse("{1; 2; 3}") is true
  does-parse("{4 * 3; 3; 10 - 2}") is true
  does-parse("{12; 2 + 3; 14;}") is true
  does-parse("{{124; 124; 12}") is false
  does-parse("{word; hello; there; pyret") is false
  does-parse("234; hi; bad}") is false
  does-parse("{one}") is true
end

check "should parse tuple-get":
  does-parse("tup.{2}") is true
  does-parse("one.{3 + 4}") is false
  does-parse("two.{4") is false
  does-parse("two.{two}") is false
  does-parse("hello.5}") is false
  does-parse("tup. {2}") is true
  does-parse("tup\n\n\n\n\n    \n\n\n.{2}") is true
  does-parse("{1;2;3}.{2}") is true
  does-parse("(5 + 6).{2}") is true
  does-parse("f().{2}") is true
end

check "should parse tuple binding":
  does-parse("{x;y} = {1;2}") is true
  does-parse("{x;y;z = } {1;2}") is false
  does-parse("{1 + 3; hello} = t") is false
  does-parse("{v;v;b;u} = t") is true
end

check "should parse tuple annotations":
  does-parse("fun f(tup:: {Number; String; Number}): tup.{0} end") is true
  does-parse("fun f(tup:: {Number; String; Number): tup.{0} end") is false
  does-parse("fun f(tup:: {hello; there}): hello end") is true
  does-parse("fun f(tup:: {Number}): tup.{1} end") is true
  does-parse("fun f(tup:: {Number; {String; Number; {hello; there}}; {hi; what}}): tup.{1} end") is true
end

check "should parse tuple binding":
  does-parse("for each({k;v;} from elts): k end") is true
end

check "should parse reactors":
  does-parse("reactor: start-with: 5, on-tick: lam(x): x + 1 end end") is true
  does-parse("reactor: error-later-than-parsing: error end") is true
  does-parse("reactor: end") is false
  does-parse("reactor end") is false
end

check "should parse unit-annotated numbers":
  does-parse("2%<m>") is true
  does-parse("2%<(m)>") is true
  does-parse("2%<m * n>") is true
  does-parse("2%<m / n>") is true
  does-parse("2%<m / n * o>") is true
  does-parse("2%<m ^ 1>") is true
  does-parse("2%<m ^ 1.2>") is true # should be wf-error
  does-parse("2%<(m / n) ^ -5 * (o ^ 10)>") is true

  does-parse("2%<>") is false
  does-parse("2%<()>") is false
  does-parse("2%<m *>") is false
  does-parse("2%<m*n>") is false
  does-parse("2%<m />") is false
  does-parse("2%<m/n>") is false
  does-parse("2%<m^1>") is false
  does-parse("2%<m ^ 6/5>") is false
  does-parse("2%<m ^ n>") is false
  does-parse("2%<m^1>") is false
end

check "should parse unit-anns numbers":
  does-parse("n :: Number%<m> = 0") is true
  does-parse("n :: Number%<m>%(is-even) = 0") is true
  does-parse("n :: Number%<m>%(is-two)%(is-even) = 0") is true
  does-parse("n :: String%<m> = 'foo'") is true

  # TODO(benmusch): Should these parse?
  does-parse("n :: Number%(is-two)%<m> = 0") is true

  # should be caught by wf:
  does-parse("n :: Number%<m>%<s> = 0") is true
  does-parse("n :: Number%<m>%(is-even)%<s> = 0") is true
  does-parse("n :: Number%<m / s * m> = 0") is true

  does-parse("n :: Number%<>%(is-even) = 0") is false
  does-parse("n :: Number%<> = 0") is false
end
