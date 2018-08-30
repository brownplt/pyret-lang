#lang pyret

import cmdline as C
import either as E
import parse-pyret as P
import string-dict as SD
import pprint as PP
import pathlib as PL
import file("../../src/arr/compiler/desugar.arr") as D
import file("../../src/arr/compiler/desugar-check.arr") as DC
import ast as A
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
import file("../../src/arr/compiler/js-ast.arr") as J

# this value is the limit of number of steps that could be inlined in case body
DEFAULT-INLINE-CASE-LIMIT = 5

cl-options = [SD.string-dict:
  "width",
    C.next-val-default(C.Number, 80, some("w"), C.once, "Pretty-printed width"),
  "standard-builtins",
    C.flag(C.once, "Use standard buildins instead of minimal builtins"),
  "check-mode",
    C.flag(C.once, "Compile code with check-mode enabled"),
  "type-check",
    C.flag(C.once, "Type check code"),
  "inline-case-body-limit",
    C.next-val-default(C.Number, DEFAULT-INLINE-CASE-LIMIT, none, C.once, "Set number of steps that could be inlined in case body")
]

parsed-options = C.parse-cmdline(cl-options)

compile-str = lam(filename, options):
  base-module = CS.dependency("file-no-cache", [list: filename])
  base = CLI.module-finder({current-load-path:"./", cache-base-dir: "./compiled"}, base-module)
  wlist = CL.compile-worklist(CLI.module-finder, base.locator, base.context)
  traces = SD.make-mutable-string-dict()
  result = CL.compile-program(wlist, options.{
      collect-all: true,
      method before-compile(_, _): nothing end,
      method on-compile(_, locator, loadable, trace) block:
        traces.set-now(locator.name(), trace)
        loadable
      end
    })
  errors = result.loadables.filter(CL.is-error-compilation)
  cases(List<CS.CompileResult>) errors:
    | empty =>
      E.right({result.loadables; traces})
    | link(_, _) =>
      E.left({result.loadables; traces})
  end
end

println = lam(s) block:
  print(s)
  print("\n")
end

fun pretty-result(result):
  if A.is-Program(result): result.tosource()
  else if AN.is-AProg(result): result.tosource()
  else if JS.is-CompiledCodePrinter(result): result.pyret-to-js-pretty()
  else if CS.is-NameResolution(result): result.ast.tosource()
  else if CS.is-ScopeResolution(result): result.ast.tosource()
  else if J.is-JExpr(result) or J.is-JStmt(result) or J.is-JBlock(result): result.tosource()
  else if CS.is-CompileResult(result):
    cases(CS.CompileResult) result:
      | ok(c) => pretty-result(c)
      | err(problems) => PP.flow-map(PP.hardline, lam(p): PP.str(tostring(p)) end, problems)
    end
  else if is-nothing(result): PP.mt-doc
  else if SD.is-mutable-string-dict(result):
    PP.surround-separate(2, 1, PP.lbrace + PP.rbrace,
      PP.lbrace, PP.commabreak, PP.rbrace,
      result.keys-list-now().sort().map(
        lam(k):
          PP.nest(2, PP.str(k) + PP.str(" => ") + PP.nest(2, pretty-result(result.get-value-now(k))))
        end))
  else if SD.is-string-dict(result):
    PP.surround-separate(2, 1, PP.lbrace + PP.rbrace,
      PP.lbrace, PP.commabreak, PP.rbrace,
      result.keys-list().sort().map(
        lam(k):
          PP.nest(2, PP.str(k) + PP.str(" => ") + PP.nest(2, pretty-result(result.get-value(k))))
        end))
  else: PP.str(torepr(result))
  end
end



cases (C.ParsedArguments) parsed-options block:
  | success(opts, rest) =>
    print-width = opts.get-value("width")
    libs =
      if opts.has-key("standard-builtins"):
        CS.standard-imports
      else:
        CS.minimal-imports # not empty -- need globals
      end
    check-mode = opts.has-key("check-mode")
    type-check = opts.has-key("type-check")
    inline-case-body-limit = opts.get-value("inline-case-body-limit")
    println("Success")
    cases (List) rest block:
      | empty => println("Require a file name")
      | link(file, _) =>
        println("File is " + file)
        options = CS.default-compile-options.{
          check-mode: check-mode,
          type-check: type-check,
          proper-tail-calls: true,
          inline-case-body-limit: inline-case-body-limit
        }
        compiled = compile-str(file, options)
        println("")

        comp = cases(E.Either) compiled block:
          | left(v) =>
            println("Compilation failed")
            {_; traces} = v
            traces.get-value-now(PL.basename(file, ""))
          | right(v) =>
            {_; traces} = v
            println(traces.keys-now())
            traces.get-value-now(PL.basename(file, ""))
        end

        for each(phase from comp) block:
          println("\n")
          println(">>>>>>>>>>>>>>>>>>")
          println(phase.name + ":   " + tostring(phase.time) + "ms")
          each(println, pretty-result(phase.result).pretty(print-width))
        end
    end
  | arg-error(m, _) =>
    each(println,  ("Error: " + m) ^ link(_, C.usage-info(cl-options)))
end
println("Finished")
