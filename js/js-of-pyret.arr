#lang pyret

provide *
import "compile-structs.arr" as C
import ast as A
import "anf.arr" as N
import "ast-split.arr" as AS
import "anf-simple-compile.arr" as AC
import "anf-visitor-compiler.arr" as AV
import "desugar.arr" as D
import "ast-util.arr" as AU

fun pretty(src): src.tosource().pretty(80).join-str("\n") end

fun make-compiled-pyret(program-ast, env):

  desugared = D.desugar(program-ast, env)
  cleaned = desugared.visit(AU.merge-nested-blocks)
                     .visit(AU.flatten-single-blocks)
                     .visit(AU.link-list-visitor(env))
  anfed = N.anf-program(cleaned)
  split = AS.ast-split(anfed.body)
  #split = AS.split-result-e([], anfed.body, set([]))
  compiled = anfed.visit(AV.splitting-compiler)
  #AC.compile(split, anfed.imports)
  
  {
    pyret-to-js-standalone: fun():
      raise("Cannot generate standalone JS")
    end,
    pyret-to-js-pretty: fun():
      C.ok(pretty(compiled))
    end,
    pyret-to-js-runnable: fun():
      code = compiled.to-ugly-source()
      C.ok(code)
    end,
    print-js-runnable: fun(printer):
      compiled.print-ugly-source(printer)
    end
  }
end

