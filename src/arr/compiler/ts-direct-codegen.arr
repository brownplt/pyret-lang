import file("ast.arr") as A
import file("js-ast.arr") as J
import string-dict as D
include from J:
  *
end
provide *

fun compile-program(prog :: A.Program, uri, env, post-env, provides, options) block:
  [D.string-dict:
    "requires", j-list(true, [clist:]),
    "provides", j-obj([clist:]),
    "nativeRequires", j-list(true, [clist:]),
    "theModule", J.j-raw-code("console.log('NYI: Stub for to-be-implemented typescript compilation CHANGED')"),
    "theMap", J.j-str("")
    ]
end
