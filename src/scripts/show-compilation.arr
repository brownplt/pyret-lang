#lang pyret

import cmdline as C
import either as E
import parse-pyret as P
import string-dict as SD
import file("../../src/arr/compiler/desugar.arr") as D
import file("../../src/arr/compiler/desugar-check.arr") as DC
import ast as A
import file("../../src/arr/compiler/compile.arr") as CM
import file("../../src/arr/compiler/compile-structs.arr") as CS
import file("../../src/arr/compiler/cli-module-loader.arr") as CLI
import file("../../src/arr/compiler/compile-lib.arr") as CL
import file("../../src/arr/compiler/resolve-scope.arr") as R
import file("../../src/arr/compiler/ast-util.arr") as U
import file("../../src/arr/compiler/ast-anf.arr") as AN
import file("../../src/arr/compiler/anf.arr") as N
import file("../../src/arr/compiler/js-of-pyret.arr") as JS
import file("../../src/arr/compiler/desugar-check.arr") as CH
import file as F

cl-options = [SD.string-dict:
  "width",
    C.next-val-default(C.Number, 80, some("w"), C.once, "Pretty-printed width"),
  "standard-builtins",
    C.flag(C.once, "Use standard buildins instead of minimal builtins"),
  "check-mode",
    C.flag(C.once, "Compile code with check-mode enabled"),
  "type-check",
    C.flag(C.once, "Type check code")
]

parsed-options = C.parse-cmdline(cl-options)

compile-str = lam(filename, options):
  base-module = CS.dependency("file-no-cache", [list: filename])
  base = CLI.module-finder({current-load-path:"./", cache-base-dir: "./compiled"}, base-module)
  wlist = CL.compile-worklist(CLI.module-finder, base.locator, base.context)
  result = CL.compile-program(wlist, options.{collect-all: true})
  errors = result.loadables.filter(CL.is-error-compilation)
  cases(List) errors:
    | empty =>
      E.right(result.loadables)
    | link(_, _) =>
      E.left(result.loadables)
  end
end

println = lam(s): print(s + "\n") end


cases (C.ParsedArguments) parsed-options block:
  | success(opts, rest) =>
    print-width = opts.get-value("width")
    libs =
      if opts.has-key("standard-builtins"):
        CS.standard-imports
      else:
        CS.minimal-imports
      end
    check-mode = opts.has-key("check-mode")
    type-check = opts.has-key("type-check")
    print("Success")
    cases (List) rest block:
      | empty => print("Require a file name")
      | link(file, _) =>
        print("File is " + file)
        options = CS.default-compile-options.{
          check-mode: check-mode,
          type-check: type-check,
        }
        compiled = compile-str(file, options)
        print("")

        comp = cases(E.Either) compiled:
          | left(v) => print("Compilation failed")
          | right(v) => v.last().result-printer.tolist()
        end

        for each(phase from comp) block:
          print("\n")
          print(">>>>>>>>>>>>>>>>>>\n")
          print(phase.name + ":\n")
          if A.Program(phase.result) block: each(println, phase.result.tosource().pretty(print-width))
          else if AN.AProg(phase.result): each(println, phase.result.tosource().pretty(print-width))
          else if JS.CompiledCodePrinter(phase.result): println(phase.result.pyret-to-js-pretty(print-width))
          else if CS.NameResolution(phase.result): each(println, phase.result.ast.tosource().pretty(print-width))
          else if CS.CompileResult(phase.result):
            cases(CS.CompileResult) phase.result block:
              | ok(c) =>
                if A.Program(c) block: each(println, c.tosource().pretty(print-width))
                else if JS.CompiledCodePrinter(c): println(c.pyret-to-js-pretty(print-width))
                else:
                  print("Unknown CompileResult result type")
                  print(torepr(c))
                end
              | err(problems) => each(println, problems.map(tostring))
            end
          else:
            println("Unknown phase result type")
            println(torepr(phase.result))
          end
        end
    end
  | arg-error(m, _) =>
    each(println,  ("Error: " + m) ^ link(_, C.usage-info(cl-options)))
end
print("Finished")
