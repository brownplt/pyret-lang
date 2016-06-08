#lang pyret

provide *
provide-types *
import file("compile-structs.arr") as C
import file("concat-lists.arr") as CL
import ast as A
import string-dict as SD
import file as F
import file("anf.arr") as N
import file("anf-loop-compiler.arr") as AL
import file("desugar-check.arr") as CH
import file("desugar.arr") as D
import file("ast-util.arr") as AU
import file("js-ast.arr") as J

# TODO(joe): add methods for printing to module vs static information
data CompiledCodePrinter:
  | ccp-dict(dict :: SD.StringDict) with:
    to-j-expr(self, d):
      J.j-parens(J.j-obj(for CL.map_list(k from d.keys-list()):
          J.j-field(k, d.get-value(k))
        end))
    end,
    pyret-to-js-static(self) -> String:
      self.to-j-expr(self.dict.remove("theModule")).to-ugly-source()
    end,
    print-js-static(self, printer):
      self.to-j-expr(self.dict.remove("theModule")).print-ugly-source(printer)
    end,
    pyret-to-js-pretty(self, width) -> String:
      self.to-j-expr(self.dict).tosource().pretty(width).join-str("\n")
    end,
    pyret-to-js-runnable(self) -> String:
      self.to-j-expr(self.dict).to-ugly-source()
    end,
    print-js-runnable(self, printer):
      self.to-j-expr(self.dict).print-ugly-source(printer)
    end
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
      printer(self.compiled)
    end
  | ccp-file(path :: String) with:
    pyret-to-js-pretty(self, width) -> String:
      raise("Cannot generate pretty JS from code string")
    end,
    pyret-to-js-runnable(self) -> String block:
      F.file-to-string(self.path)
    end,
    print-js-runnable(self, printer):
      printer(self.pyret-to-js-runnable())
    end
end

fun make-compiled-pyret(program-ast, env, provides, options) -> CompiledCodePrinter:
  anfed = N.anf-program(program-ast)
  compiled = anfed.visit(AL.splitting-compiler(env, provides, options))
  ccp-dict(compiled)
end

fun trace-make-compiled-pyret(trace, phase, program-ast, env, provides, options) block:
  var ret = trace
  anfed = N.anf-program(program-ast)
  ret := phase("ANFed", anfed, ret)
  phase("Generated JS", ccp-dict(anfed.visit(AL.splitting-compiler(env, provides, options))), ret)
end
