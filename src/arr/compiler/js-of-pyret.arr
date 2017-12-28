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
      J.j-parens(J.j-obj(for cl-map-sd(k from d):
          J.j-field(k, d.get-value(k))
        end))
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
      self.to-j-expr(self.dict).to-ugly-source()
    end,
    method print-js-runnable(self, printer):
      self.to-j-expr(self.dict).print-ugly-source(printer)
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

fun trace-make-compiled-pyret(add-phase, program-ast, env, bindings, provides, options)
  -> { C.Provides; C.CompileResult<CompiledCodePrinter> } block:
  anfed = add-phase("ANFed", N.anf-program(program-ast))
  flatness-env = add-phase("Build flatness env", FL.make-prog-flatness-env(anfed, bindings, env))
  flat-provides = add-phase("Get flat-provides", FL.get-flat-provides(provides, flatness-env, anfed))
  compiled = anfed.visit(AL.splitting-compiler(env, add-phase, flatness-env, flat-provides, options))
  {flat-provides; add-phase("Generated JS", C.ok(ccp-dict(compiled)))}
end

fun println(s) block:
  print(s + "\n")
end

fun make-compiled-pyret(program-ast, env, bindings, provides, options) -> { C.Provides; CompiledCodePrinter} block:
#  each(println, program-ast.tosource().pretty(80))
  anfed = N.anf-program(program-ast)
  #each(println, anfed.tosource().pretty(80))
  flatness-env = FL.make-prog-flatness-env(anfed, bindings, env)
  flat-provides = FL.get-flat-provides(provides, flatness-env, anfed)
  compiled = anfed.visit(AL.splitting-compiler(env, flatness-env, flat-provides, options))
  {flat-provides; ccp-dict(compiled)}
end

