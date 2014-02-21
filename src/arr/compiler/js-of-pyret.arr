#lang pyret

provide *
import "./compile-structs.arr" as C
import ast as A
import "./anf.arr" as N
import "./ast-split.arr" as AS
import "./anf-visitor-compiler.arr" as AV
import "./desugar-check.arr" as CH
import "./desugar.arr" as D
import "./ast-util.arr" as AU

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

  checked = program-ast.visit(CH.check-visitor)
  desugared = D.desugar(checked, env)
  cleaned = desugared.visit(AU.merge-nested-blocks)
                     .visit(AU.flatten-single-blocks)
                     .visit(AU.link-list-visitor(env))
  anfed = N.anf-program(cleaned)
  split = AS.ast-split(anfed.body)
  #split = AS.split-result-e([], anfed.body, set([]))
  compiled = anfed.visit(AV.splitting-compiler)
  
  ccp(compiled)
end

