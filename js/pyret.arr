#lang pyret

import cmdline as C
import file as F
import "compile.arr" as CM
import "compile-structs.arr" as CS
import string-dict as D


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
      no-check-mode:
        C.flag(C.once, "Skip checks")
    }

  params-parsed = C.parse-args(options, args)

  cases(C.ParsedArguments) params-parsed:
    | success(r, rest) => 
      if not is-empty(rest):
        print("Extra arguments provided")
        print(C.usage-info(options).join-str("\n"))
      else:
        bs = if r.has-key("builtins"):
            r.get("libs")
          else:
            []
          end
        check-mode = if r.has-key("no-check-mode"):
            false
          else:
            true
          end
        libs = if r.has-key("library"): CS.no-builtins else: CS.standard-builtins end
        result = if r.has-key("compile-standalone-js"):
            CM.compile-standalone-js-file(
              r.get("compile-standalone-js"),
              libs,
              {
                check-mode : check-mode
              }
            )
          else if r.has-key("compile-module-js"):
            CM.compile-js(
              F.file-to-string(r.get("compile-module-js")),
              r.get("compile-module-js"),
              libs,
              {
                check-mode : check-mode
              }
            )
          else:
            print(C.usage-info(options).join-str("\n"))
            raise("Unknown command line options")
          end

        result.print-js-runnable(display)
        #cases(CS.CompileResult) result:
        #  | ok(code) => print(code)
        #  | err(message) => raise(result)
        #end
      end
    | arg-error(message, partial) =>
      print(C.usage-info(options).join-str("\n"))
  end
end

_ = main(C.args)
