provide *
provide-types *

import file as F
import string-dict as SD
import pprint as PP

import file("anf.arr") as N
import file("anf-loop-compiler.arr") as AL
import file("ast-util.arr") as AU
import file("compile-structs.arr") as C
import file("concat-lists.arr") as CL
import file("flatness.arr") as FL
import file("js-ast.arr") as J
import file("direct-codegen.arr") as D

cl-empty = CL.concat-empty
cl-cons = CL.concat-cons

fun cl-map-sd(f, sd):
  for SD.fold-keys(acc from cl-empty, key from sd):
    cl-cons(f(key), acc)
  end
end

# TODO(joe): add methods for printing to module vs static information
data CompiledCodePrinter:
  | ccp-dict(dict :: SD.StringDict) with:
    method to-j-expr(self, d):
      J.j-obj(for cl-map-sd(k from d):
          J.j-field(k, d.get-value(k))
        end)
    end,
    method pyret-to-js-static(self) -> String:
      self.to-j-expr(self.dict.remove("theModule")).to-ugly-source()
    end,
    method print-js-static(self, printer):
      self.to-j-expr(self.dict.remove("theModule")).print-ugly-source(printer)
    end,
    method pyret-to-js-pretty(self) -> PP.PPrintDoc:
      self.to-j-expr(self.dict).tosource()
    end,
    method pyret-to-js-runnable(self) -> String:
      self.dict.get-value("theModule").to-ugly-source()
    end,
    method print-js-runnable(self, printer):
      self.dict.get-value("theModule").print-ugly-source(printer)
    end,
    method print-js-module(self, printer):
      self.dict.get-value("theModule").print-ugly-source(printer)
    end
  | ccp(compiled :: J.JExpr) with:
    method pyret-to-js-pretty(self) -> PP.PPrintDoc:
      self.compiled.tosource()
    end,
    method pyret-to-js-runnable(self) -> String:
      self.compiled.to-ugly-source()
    end,
    method print-js-runnable(self, printer):
      self.compiled.print-ugly-source(printer)
    end
  | ccp-string(compiled :: String) with:
    method pyret-to-js-pretty(self) -> PP.PPrintDoc:
      PP.str(self.compiled)
    end,
    method pyret-to-js-runnable(self) -> String:
      self.compiled
    end,
    method print-js-runnable(self, printer):
      printer(self.compiled)
    end
  | ccp-two-files(static-path :: String, code-path :: String) with:
    method pyret-to-js-pretty(self, width) -> String:
      raise("Cannot generate pretty JS from code string")
    end,
    method print-js-static(self, printer):
      printer(F.file-to-string(self.static-path))
    end,
    method print-js-runnable(self, printer):
      printer(F.file-to-string(self.code-path))
    end,
    method pyret-to-js-runnable(self) -> String:
      F.file-to-string(self.code-path)
    end,

  | ccp-file(path :: String) with:
    method pyret-to-js-pretty(self, width) -> String:
      raise("Cannot generate pretty JS from code string")
    end,
    method pyret-to-js-runnable(self) -> String block:
      F.file-to-string(self.path)
    end,
    method print-js-runnable(self, printer):
      printer(self.pyret-to-js-runnable())
    end
end

fun trace-make-compiled-pyret(add-phase, program-ast, env, post-env, provides, options)
  -> { C.Provides; C.CompileResult<CompiledCodePrinter> } block:
  make-compiled-pyret(program-ast, env, post-env, provides, options)
end

fun println(s) block:
  print(s + "\n")
end

fun make-compiled-pyret(program-ast, env, post-env, provides, options) -> { C.Provides; C.CompileResult<CompiledCodePrinter>} block:
#  each(println, program-ast.tosource().pretty(80))
  {provides; 
    C.ok(ccp-dict(D.compile-program(program-ast, env, post-env, provides, options)))}
end

