import ast as A
import ast-visitors as AV
import parse-pyret as SP
import file as F
import string-dict as D
import cmdline as C
import system as SYS

dummy = A.dummy-loc
success-code = 0
failure-code = 1

fun main(args :: List<String>) -> Number block:
  options = [D.string-dict:
    "in-file",
      C.next-val(C.String, C.required-once, "The input file"),
    "out-file",
      C.next-val(C.String, C.required-once, "The output file"),
  ]
  cases(C.ParsedArguments) C.parse-args(options, args) block:
    | success(r, rest) =>
      in-file = r.get-value('in-file')
      out-file = r.get-value('out-file')
      p = SP.surface-parse(F.input-file(in-file).read-file(), in-file)
      as-string = p.tosource().pretty(80).join-str("\n")
      F.output-file(out-file, false).display(as-string)
      success-code
    | arg-error(message, partial) =>
      print-error(message + "\n")
      print-error(C.usage-info(options).join-str("\n") + '\n')
      failure-code
  end
end

exit-code = main(C.args)
SYS.exit-quiet(exit-code)
