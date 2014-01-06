#lang pyret

provide *
import file as F
import ast as A
import namespaces as N
import "js-of-pyret.arr" as P
import "compile-structs.arr" as C

fun parse-libs(libs):
  for fold(
      pair from {ls : [], env : N.library-env },
      l from libs
    ):
    ast = A.parse-tc(
        F.file-to-string(l),
        l,
        { check : false, env: {}, allow-unbound: true }
      )
    ids = A.toplevel-ids(lib.prog)
    new-env = for fold(the-env from pair.env, id from ids):
      the-env.{[id]: true}
    end
    { ls : [ast] + pair.ls, env : new-env }
  end
end

fun compile-standalone-js(js-file, libs) -> C.CompileResult:
  f = F.input-file(js-file)
  code = f.read-file()
  f.close-file()

  libs-parsed = parse-libs(libs)
  ast = A.parse-tc(code, js-file, {check : true, env: libs-parsed.env })
  ce = C.compile-env(libs-parsed.ls, libs-parsed.env)
  P.pyret-to-js(pyret-to-js, ce)
end

