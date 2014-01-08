#lang pyret

provide *
import "compile-structs.arr" as C
import ast as A
import "anf.arr" as N
import "ast-split.arr" as AS
import "anf-simple-compile.arr" as AC

fun make-compiled-pyret(program-ast, env):

  anfed = N.anf-program(program-ast)
  split = AS.ast-split(anfed.body)
  print(split)
  compiled = AC.compile(split)
  code = compiled.tosource().pretty(80).join-str("\n")
  c = C.ok(code)
  
  {
    pyret-to-js-standalone: fun():
      c
    end,
    pyret-to-js-runnable: fun():
      c
    end
  }
end

