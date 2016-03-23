#lang pyret

provide *
provide-types *
import file("compile-structs.arr") as C
import ast as A
import file("anf.arr") as N
import file("anf-loop-compiler.arr") as AL
import file("desugar-check.arr") as CH
import file("desugar.arr") as D
import file("ast-util.arr") as AU
import file("js-ast.arr") as J

data CompiledCodePrinter:
  | ccp(compiled :: J.JExpr) with:
    pyret-to-js-pretty(self, width) -> String:
      self.compiled.tosource().pretty(width).join-str("\n")
    end,
    pyret-to-js-runnable(self) -> String:
      self.compiled.to-ugly-source()
    end,
    print-js-runnable(self, printer):
      self.compiled.print-ugly-source(printer)
    end
  | ccp-string(compiled :: String) with:
    pyret-to-js-pretty(self, width) -> String:
      raise("Cannot generate pretty JS from code string")
    end,
    pyret-to-js-runnable(self) -> String:
      self.compiled
    end,
    print-js-runnable(self, printer):
      printer.append(self.compiled)
    end
end

fun make-compiled-pyret(program-ast, env, options) -> CompiledCodePrinter:
  anfed = N.anf-program(program-ast)
  compiled = anfed.visit(AL.splitting-compiler(env, options))
  ccp(compiled)
end

fun trace-make-compiled-pyret(trace, phase, program-ast, env, options):
  var ret = trace
  anfed = N.anf-program(program-ast)
  ret := phase("ANFed", anfed, ret)
  phase("Generated JS", ccp(anfed.visit(AL.splitting-compiler(env, options))), ret)
end
