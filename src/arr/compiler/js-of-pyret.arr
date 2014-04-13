#lang pyret

provide *
import "compiler/compile-structs.arr" as C
import ast as A
import "compiler/anf.arr" as N
import "compiler/ast-split.arr" as AS
import "compiler/anf-visitor-compiler.arr" as AV
import "compiler/desugar-check.arr" as CH
import "compiler/desugar.arr" as D
import "compiler/ast-util.arr" as AU

data CompiledCodePrinter:
  | ccp(compiled :: J.JExpr) with:
    pyret-to-js-standalone(self) -> String:
      raise("Cannot generate standalone JS")
    end,
    pyret-to-js-pretty(self) -> String:
      self.compiled.tosource().pretty(80).join-str("\n")
    end,
    pyret-to-js-runnable(self) -> String:
      self.compiled.to-ugly-source()
    end,
    print-js-runnable(self, printer):
      self.compiled.print-ugly-source(printer)
    end
end

fun make-compiled-pyret(program-ast, env) -> CompiledCodePrinter:
  anfed = N.anf-program(program-ast)
  compiled = anfed.visit(AV.splitting-compiler(env))
  ccp(compiled)
end

fun make-unsafe-compiled-pyret(program-ast, env) -> CompiledCodePrinter:
  anfed = N.anf-program(program-ast)
  compiled = anfed.visit(AV.non-splitting-compiler(env))
  ccp(compiled)
end

