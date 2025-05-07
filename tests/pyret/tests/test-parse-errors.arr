import error as ERR
import file("../../../src/arr/compiler/compile-structs.arr") as CS
import file("../test-parse-helper.arr") as P
import file("../test-compile-helper.arr") as C

get-parse-error = P.get-parse-error
wss = P.wss

next-token = [list:
  "end",
  "foo(args no-comma)",
  "fun foo(params no-comma): nothing end",
  "data: foo end"
].append(P.exp-en-ops)

check "next token":
  for each(program from next-token):
    get-parse-error(program) satisfies ERR.is-parse-error-next-token
  end
  for each(program from P.test-en-ops):
    get-parse-error(program) satisfies ERR.is-parse-error-bad-check-operator
  end
end

eof = [list:
  "block: none",
  "check: 0 is 0",
  "data foo: bar",
  "for map(x from empty): x",
  "fun foo(x, y):",
  "import foo as",
  "include",
  "let foo = 0:",
  "provide",
  "provide-types",
  "type foo",
  "when true: 0",
  "foo =",
  "foo +"
]

check "coloncolon":
  get-parse-error(
    "lam(a :: A):\n" +
    "  {v1; s1} :: {B; D} = f2(a)\n" +
    "end") satisfies ERR.is-parse-error-colon-colon
end

check "eof":
  for each(program from eof):
    get-parse-error(program) satisfies ERR.is-parse-error-eof
  end
end

unterminated-string = [list:
  "'foo",
  "foo'",
  '"bar',
  'bar"',
  "```baz",
  "baz```"
]

check "unterminated string":
  for each(program from unterminated-string):
    get-parse-error(program) satisfies ERR.is-parse-error-unterminated-string
  end
end

bad-operator = [list:
  "0+",
  "*0",
  "0+0",
  "0*0",
  "0 *0",
  "0* 0"
]

check "bad operator":
  for each(program from bad-operator):
    get-parse-error(program) satisfies ERR.is-parse-error-bad-operator
  end
end

bad-number = [list:
  ".0",
  "~.0",
  "foo.0"
]

check "bad-number":
  for each(program from bad-number):
    get-parse-error(program) satisfies ERR.is-parse-error-bad-number
  end
end

check "spacey-apps":
  get-parse-error("foo ()") satisfies ERR.is-parse-error-bad-app
  get-parse-error("foo (x, y)") satisfies ERR.is-parse-error-bad-app
  P.does-parse("foo (x)") is true
  C.run-str("foo (x)") is%(C.output) C.compile-error(lam(e): CS.is-same-line(e) and e.b-is-paren end)
end

spacey-headers = [list:
  "fun foo (): 6 end",
  "fun foo (x): 6 end",
  "fun foo (x :: Number): 6 end",
  "fun foo (x, y): 6 end",
  "lam (): 6 end",
  "lam (x): 6 end",
  "lam (x :: Number): 6 end",
  "lam (x, y): 6 end",
  "{ (): 6 }",
  "{ (x): 6 }",
  "{ (x :: Number): 6 }",
  "{ (x, y): 6 }",
  "method (): 6 end",
  "method (x): 6 end",
  "method (x :: Number): 6 end",
  "method (x, y): 6 end",
  "data Foo: z with: method foo (): 6 end end",
  "data Foo: z with: method foo (x): 6 end end",
  "data Foo: z with: method foo (x :: Number): 6 end end",
  "data Foo: x with: method foo (x, y): 6 end end",
  "{ x : method (): 6 end }",
  "{ x : method (x): 6 end }",
  "{ x : method (x :: Number): 6 end }",
  "{ x : method (x, y): 6 end }",
  "{ method x (): 6 end }",
  "{ method x (x): 6 end }",
  "{ method x (x :: Number): 6 end }",
  "{ method x (x, y): 6 end }",
]

check "spacey-headers":
  for each(program from spacey-headers):
    get-parse-error(program) satisfies ERR.is-parse-error-bad-fun-header
  end
end
