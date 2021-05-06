import file("ast.arr") as A
import file("js-ast.arr") as J
import js-file("ts-direct-codegen-impl") as TDC
import string-dict as D
include from J:
  *
end
provide *

fun compile-program(prog :: A.Program, uri, env, post-env, provides, options) block:
  result = TDC.compile-program(prog, uri, env, post-env, provides, options)
  [D.string-dict:
    "requires", j-list(true, [clist:]),
    "provides", j-obj([clist:]),
    "nativeRequires", j-list(true, [clist:]),
    "theModule", J.j-raw-code(result.theModule),
    "theMap", J.j-str(result.theMap)
    ]
end
