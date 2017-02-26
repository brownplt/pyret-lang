import error as ERR
import file("../test-parse-helper.arr") as P

get-parse-error = P.get-parse-error
wss = P.wss
en-ops = P.en-ops

next-token = [list:
  "end",
  "foo(args no-comma)",
  "fun foo(params no-comma): nothing end",
  "data: foo end"
].append(en-ops)

check "next token":
  for map(program from next-token):
    get-parse-error(program) satisfies ERR.is-parse-error-next-token
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

check "eof":
  for map(program from eof):
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
  for map(program from unterminated-string):
    get-parse-error(program) satisfies ERR.is-parse-error-unterminated-string
  end
end

bad-operator = [list:
  "+",
  "0+",
  "*0",
  "0+0"
]

check "bad operator":
  for map(program from bad-operator):
    get-parse-error(program) satisfies ERR.is-parse-error-bad-operator
  end
end

bad-number = [list:
  ".0",
  "~.0",
  "foo.0"
]

check "bad-number":
  for map(program from bad-number):
    get-parse-error(program) satisfies ERR.is-parse-error-bad-number
  end
end
