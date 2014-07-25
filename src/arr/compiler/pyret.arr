#lang pyret

import cmdline as C
import file as F
import exec as X
import string-dict as D
import "compiler/compile.arr" as CM
import "compiler/compile-structs.arr" as CS
import format as Format
import either as E
import "compiler/initialize-trove.arr" as IT

format = Format.format
Either = E.Either
left = E.left
right = E.right


fun parse-dialects(arg-index, name, val):
  if (val == "Pyret") or (val == "Bootstrap"): left(val)
  else: right(format("~a expected a dialect, got ~a", [list: name, torepr(val)]))
  end
end

fun main(args):
  options = {
    compile-standalone-js:
      C.next-val(C.String, C.once, "Pyret (.arr) file to compile"),
    compile-module-js:
      C.next-val(C.String, C.once, "Pyret (.arr) file to compile"),
    library:
      C.flag(C.once, "Don't auto-import basics like list, option, etc."),
    libs:
      C.next-val(C.String, C.many, "Paths to files to include as builtin libraries"),
    module-load-dir:
      C.next-val-default(C.String, ".", none, C.once, "Base directory to search for modules"),
    check-all:
      C.flag(C.once, "Run checks all modules (not just the main module)"),
    no-check-mode:
      C.flag(C.once, "Skip checks"),
    allow-shadow:
      C.flag(C.once, "Run without checking for shadowed variables"),
    type-check:
      C.flag(C.once, "Type-check the program during compilation"),
    dialect:
        C.next-val-default(C.Custom("Pyret|Bootstrap", parse-dialects),
          "Pyret", some("d"), C.once, "Dialect of Pyret to use when compiling")
  }
  
  params-parsed = C.parse-args(options, args)

  fun err-less(e1, e2):
    if (e1.loc.before(e2.loc)): true
    else if (e1.loc.after(e2.loc)): false
    else: tostring(e1) < tostring(e2)
    end
  end
  
  cases(C.ParsedArguments) params-parsed:
    | success(r, rest) => 
      check-mode = not(r.has-key("no-check-mode") or r.has-key("library"))
      allow-shadowed = r.has-key("allow-shadow")
      libs =
        if r.has-key("library"): CS.minimal-builtins
        else if r.get("dialect") == "Bootstrap": CS.bootstrap-builtins
        else: CS.standard-builtins end
      module-dir = r.get("module-load-dir")
      check-all = r.has-key("check-all")
      type-check = r.has-key("type-check")
      if not(is-empty(rest)):
        program-name = rest.first
        result = CM.compile-js(
          CM.start,
          r.get("dialect"),
          F.file-to-string(program-name),
          program-name,
          libs,
          {
            check-mode : check-mode,
            allow-shadowed : allow-shadowed,
            collect-all: false,
            type-check: type-check,
            ignore-unbound: false
          }
          ).result
        cases(CS.CompileResult) result:
          | ok(comp-object) =>
            exec-result = X.exec(comp-object.pyret-to-js-runnable(), program-name, module-dir, check-all, r.get("dialect"), rest)
            if (exec-result.success):
              when check-mode:
                results-str = exec-result.render-check-results()
                print(results-str)
                when not(string-contains(results-str, "Looks shipshape")) and
                  not(string-contains(results-str, "The program didn't define any tests")):
                  raise("There were test errors")
                end
              end
            else:
              print(exec-result.render-error-message())
              raise("There were execution errors")
            end
          | err(errors) =>
            print-error("Compilation errors:")
            for lists.each(e from errors):
              print-error(tostring(e))
            end
            raise("There were compilation errors")
        end
      else:
        result = if r.has-key("compile-standalone-js"):
          CM.compile-standalone-js-file(
            r.get("compile-standalone-js"),
            libs,
            {
              check-mode : check-mode,
              type-check : type-check,
              allow-shadowed : allow-shadowed
            }
            )
        else if r.has-key("compile-module-js"):
          CM.compile-js(
            CM.start,
            r.get("dialect"),
            F.file-to-string(r.get("compile-module-js")),
            r.get("compile-module-js"),
            libs,
            {
              check-mode : check-mode,
              type-check : type-check,
              allow-shadowed : allow-shadowed,
              collect-all: false,
              ignore-unbound: false
            }
            ).result
        else:
          print(C.usage-info(options).join-str("\n"))
          raise("Unknown command line options")
        end
        cases(CS.CompileResult) result:
          | ok(comp-object) => comp-object.print-js-runnable(display)
          | err(errors) =>
            print-error("Compilation errors:")
            for lists.each(e from errors):
              print-error(tostring(e))
            end
            raise("There were compilation errors")
        end
      end
    | arg-error(message, partial) =>
      print(message)
      print(C.usage-info(options).join-str("\n"))
  end
end

_ = main(C.args)
