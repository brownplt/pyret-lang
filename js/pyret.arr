#lang pyret

import cmdline as C
import file as F
import "compile.arr" as CM
import "compile-structs.arr" as CS


fun main(args):
  options = {
      compile-standalone-js:
        C.next-val(C.String, C.once, "Pyret (.arr) file to compile"),
      libs:
        C.next-val(C.String, C.many, "Paths to files to include as builtin libraries")
    }

  params-parsed = C.parse-args(options, args)

  cases(C.ParsedArguments) params-parsed:
    | success(r, rest) => 
      if not is-empty(rest):
        print("Extra arguments provided")
        print(C.usage-info(options).join-str("\n"))
      else:
        bs = if builtins.has-field(r, "builtins"):
            r.libs
          else:
            CS.standard-libs
          end
        result = CM.compile-standalone-js(
            r.compile-standalone-js,
            CS.standard-builtins
          )
        cases(CS.CompileResult) result:
          | ok(code) => print(code)
          | err(message) => raise(result)
        end
      end
    | arg-error(message, partial) =>
      print(C.usage-info(options))
  end
end

_ = main(C.args)
