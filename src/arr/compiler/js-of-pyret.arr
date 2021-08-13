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
  | ccp-dict(dict :: CCPDict)
  | ccp-string(compiled :: String)
  | ccp-two-files(static-path :: String, code-path :: String)
  | ccp-file(path :: String)
end

fun pyret-to-js-static(ccp :: CompiledCodePrinter) -> String:
  cases(CompiledCodePrinter) ccp:
    | ccp-dict(dict) =>
      "{\n"
        + "\"provides\": " + dict.provides + ",\n"
        + "\"requires\": " + dict.requires + ",\n"
        + "\"nativeRequires\": " + dict.nativeRequires + ",\n"
        + "\"theMap\": " + dict.theMap + "\n"
      + "}"
    | ccp-string(compiled) => raise("pyret-to-js-static not implemented for ccp-string")
    | ccp-two-files(static-path, code-path) => F.file-to-string(static-path)
    | ccp-file(path) => raise("pyret-to-js-static not implemented for ccp-file")
  end
end

fun pyret-to-js-runnable(ccp :: CompiledCodePrinter) -> String:
  cases(CompiledCodePrinter) ccp:
    | ccp-dict(dict) => dict.theModule
    | ccp-string(compiled) => compiled
    | ccp-two-files(static-path, code-path) => F.file-to-string(code-path)
    | ccp-file(path) => F.file-to-string(path)
  end
end
 
fun pyret-to-js-pretty(ccp :: CompiledCodePrinter) -> PP.PPrintDoc:
  cases(CompiledCodePrinter) ccp:
    | ccp-dict(dict) =>
      obj([list: 
        field("provides", PP.str(dict.provides)),
        field("requires", PP.str(dict.requires)),
        field("nativeRequires", PP.str(dict.nativeRequires)),
        field("theModule", PP.str(dict.theModule)),
        field("theMap", PP.str(dict.theMap))
      ])
    | ccp-string(compiled) => PP.str(compiled)
    | ccp-two-files(_, _) => raise("Cannot generate pretty JS from code string")
    | ccp-file(_) => raise("Cannot generate pretty JS from code string")
  end
end

fun make-compiled-pyret(program-ast, uri, env, post-env, provides, options) -> { C.Provides; C.CompileResult<CompiledCodePrinter>} block:
  compiled = TD.compile-program(program-ast, uri, env, post-env, provides, options)
  {provides; C.ok(ccp-dict(compiled))}
end

