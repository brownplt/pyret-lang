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
    ids = A.toplevel-ids(l.prog)
    new-env = for fold(the-env from pair.env, id from ids):
      the-env.{[id]: true}
    end
    { ls : [{name: l, ast: ast}] + pair.ls, env : new-env }
  end
end

fun compile-js(code, name, libs, options):
  libs-parsed = parse-libs(libs)
  ast = A.parse-tc(code, name, {check : options.check-mode, env: libs-parsed.env })
  ce = C.compile-env(libs-parsed.ls, libs-parsed.env)
  P.make-compiled-pyret(ast, ce)
end

fun compile-runnable-js(code, name, libs, options):
  compile-js(code, name, libs, options).pyret-to-js-runnable()
end

fun compile-standalone-js-file(js-file, libs, options):
  code = F.file-to-string(js-file)
  compile-standalone-js(code, js-file, libs, option)
end

fun compile-standalone-js(code, name, libs, options) -> C.CompileResult:
  compile-js(code, name, libs, options).pyret-to-js-standalone()
end

