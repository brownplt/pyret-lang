#lang pyret

import cmdline as C
import file as F
import string-dict as D
import file("compile-lib.arr") as CL
import file("compile-structs.arr") as CS
import file("cli-module-loader.arr") as CLI
import file("locators/builtin.arr") as B
import format as Format
import either as E
import render-error-display as RED
format = Format.format
left = E.left
right = E.right


fun main(args):
  options = [D.string-dict:
    "build-standalone",
      C.next-val(C.String, C.once, "Main Pyret (.arr) file to build as a standalone"),
    "build-runnable",
      C.next-val(C.String, C.once, "Main Pyret (.arr) file to build as a standalone"),
    "require-config",
      C.next-val(C.String, C.once, "JSON file to use for requirejs configuration of build-runnable"),
    "outfile",
      C.next-val(C.String, C.once, "Output file for build-runnable"),
    "build",
      C.next-val(C.String, C.once, "Pyret (.arr) file to build"),
    "run",
      C.next-val(C.String, C.once, "Pyret (.arr) file to compile and run"),
    "standalone-file",
      C.next-val-default(C.String, "src/js/base/handalone.js", none, C.once, "Path to override standalone JavaScript file for main"),
    "builtin-js-dir",
      C.next-val(C.String, C.many, "Directory to find the source of builtin js modules"),
    "builtin-arr-dir",
      C.next-val(C.String, C.many, "Directory to find the source of builtin arr modules"),
    "allow-builtin-overrides",
      C.flag(C.once, "Allow overlapping builtins defined between builtin-js-dir and builtin-arr-dir"),
    "compiled-dir",
      C.next-val-default(C.String, "compiled", none, C.once, "Directory to save compiled files to"),
    "library",
      C.flag(C.once, "Don't auto-import basics like list, option, etc."),
    "module-load-dir",
      C.next-val-default(C.String, ".", none, C.once, "Base directory to search for modules"),
    "check-all",
      C.flag(C.once, "Run checks all modules (not just the main module)"),
    "no-check-mode",
      C.flag(C.once, "Skip checks"),
    "allow-shadow",
      C.flag(C.once, "Run without checking for shadowed variables"),
    "improper-tail-calls",
      C.flag(C.once, "Run without proper tail calls"),
    "type-check",
      C.flag(C.once, "Type-check the program during compilation")
  ]

  params-parsed = C.parse-args(options, args)

  fun err-less(e1, e2):
    if (e1.loc.before(e2.loc)): true
    else if (e1.loc.after(e2.loc)): false
    else: tostring(e1) < tostring(e2)
    end
  end

  cases(C.ParsedArguments) params-parsed block:
    | success(r, rest) =>
      check-mode = not(r.has-key("no-check-mode") or r.has-key("library"))
      allow-shadowed = r.has-key("allow-shadow")
      libs =
        if r.has-key("library"): CS.minimal-imports
        else: CS.standard-imports end
      module-dir = r.get-value("module-load-dir")
      check-all = r.has-key("check-all")
      type-check = r.has-key("type-check")
      tail-calls = not(r.has-key("improper-tail-calls"))
      compiled-dir = r.get-value("compiled-dir")
      standalone-file = r.get-value("standalone-file")
      when r.has-key("builtin-js-dir"):
        B.set-builtin-js-dirs(r.get-value("builtin-js-dir"))
      end
      when r.has-key("builtin-arr-dir"):
        B.set-builtin-arr-dirs(r.get-value("builtin-arr-dir"))
      end
      when r.has-key("allow-builtin-overrides"):
        B.set-allow-builtin-overrides(r.get-value("allow-builtin-overrides"))
      end
      if not(is-empty(rest)):
        raise("No longer supported")
      else:
        if r.has-key("build-runnable") block:
          outfile = if r.has-key("outfile"):
            r.get-value("outfile")
          else:
            r.get-value("build-runnable") + ".jarr"
          end
          CLI.build-runnable-standalone(
              r.get-value("build-runnable"),
              r.get-value("require-config"),
              outfile,
              CS.default-compile-options.{
                standalone-file: standalone-file,
                check-mode : check-mode,
                type-check : type-check,
                allow-shadowed : allow-shadowed,
                collect-all: false,
                ignore-unbound: false,
                proper-tail-calls: tail-calls,
                compile-module: true,
                compiled-cache: compiled-dir
              })
        else if r.has-key("build-standalone"):
          CLI.build-require-standalone(r.get-value("build-standalone"),
              CS.default-compile-options.{
                check-mode : check-mode,
                type-check : type-check,
                allow-shadowed : allow-shadowed,
                collect-all: false,
                ignore-unbound: false,
                proper-tail-calls: tail-calls,
                compile-module: true,
                compiled-cache: compiled-dir
              })
        else if r.has-key("build"):
          result = CLI.compile(r.get-value("build"),
            CS.default-compile-options.{
              check-mode : check-mode,
              type-check : type-check,
              allow-shadowed : allow-shadowed,
              collect-all: false,
              ignore-unbound: false,
              proper-tail-calls: tail-calls,
              compile-module: false # weird, but accurate for now
            })
          failures = filter(CS.is-err, result.loadables)
          when is-link(failures):
            for each(f from failures) block:
              for lists.each(e from f.errors) block:
                print-error(tostring(e))
                print-error("\n")
              end
              raise("There were compilation errors")
            end
          end
        else if r.has-key("run"):
          CLI.run(r.get-value("run"), CS.default-compile-options.{
              standalone-file: standalone-file,
              compile-module: true
            })
        else:
          print(C.usage-info(options).join-str("\n"))
          raise("Unknown command line options")
        end
      end
    | arg-error(message, partial) =>
      print(message + "\n")
      print(C.usage-info(options).join-str("\n"))
  end
end

_ = main(C.args)
