provide *
provide-types *

import string-dict as SD
import pprint as PP

import file("ast-util.arr") as AU
import file("compile-structs.arr") as C
import file("file.arr") as F
import js-file("ts-direct-codegen") as TD

type CCPDict = {
  provides :: String,
  requires :: String,
  nativeRequires :: String,
  theModule :: String,
  theMap :: String
}

INDENT = 2

fun obj(fields):
  PP.parens(PP.surround-separate(INDENT, 1, PP.str("{}"),
    PP.lbrace, PP.commabreak, PP.rbrace, fields))
end

fun field(name, val):
  PP.nest(INDENT, PP.dquote(PP.str(name)) + PP.str(": ") + val)
end

# TODO(joe): add methods for printing to module vs static information
data CompiledCodePrinter:
  | ccp-dict(dict :: CCPDict) with:
    method pyret-to-js-static(self) -> String:
      "{\n"
        + "\"provides\": " + self.dict.provides + ",\n"
        + "\"requires\": " + self.dict.requires + ",\n"
        + "\"nativeRequires\": " + self.dict.nativeRequires + ",\n"
        + "\"theMap\": " + self.dict.theMap + "\n"
      + "}"
    end,
    method print-js-static(self, printer) block:
      printer(self.pyret-to-js-static())
    end,
    method pyret-to-js-pretty(self) -> PP.PPrintDoc:
      obj([list: 
        field("provides", PP.str(self.dict.provides)),
        field("requires", PP.str(self.dict.requires)),
        field("nativeRequires", PP.str(self.dict.nativeRequires)),
        field("theModule", PP.str(self.dict.theModule)),
        field("theMap", PP.str(self.dict.theMap))
      ])
    end,
    method pyret-to-js-runnable(self) -> String:
      self.dict.theModule
    end,
    method print-js-runnable(self, printer):
      printer(self.dict.theModule)
    end,
    method print-js-module(self, printer):
      printer(self.dict.theModule)
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
    method pyret-to-js-static(self) -> String:
      F.file-to-string(self.static-path)
    end,
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

fun trace-make-compiled-pyret(add-phase, program-ast, uri, env, post-env, provides, options)
  -> { C.Provides; C.CompileResult<CompiledCodePrinter> } block:
  make-compiled-pyret(program-ast, uri, env, post-env, provides, options)
end

fun println(s) block:
  print(s + "\n")
end

fun make-compiled-pyret(program-ast, uri, env, post-env, provides, options) -> { C.Provides; C.CompileResult<CompiledCodePrinter>} block:
#  each(println, program-ast.tosource().pretty(80))
  compiled = TD.compile-program(program-ast, uri, env, post-env, provides, options)
  {provides; C.ok(ccp-dict(compiled))}
end

