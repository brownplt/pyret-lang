#lang pyret

import cmdline as C
import pprint as PP
import parse-pyret as P
import string-dict as D
import file as F
fun just-parse(file):
  P.surface-parse(F.file-to-string(file), file)
end

options = [D.string-dict:
  "width", C.next-val-default(C.Number, 80, some("w"), C.once, "Pretty-printed width")
]

parsed = C.parse-cmdline(options)

fun println(str): print(str + "\n") end

cases (C.ParsedArguments) parsed:
  | success(opts, rest) =>
    cases (List) rest:
      | empty => print("Require a file name")
      | link(file, _) =>
        each(println, just-parse(file).tosource().pretty(opts.get-value("width")))
    end
  | arg-error(m, _) =>
    each(println,  link("Error: " + m, C.usage-info(options)))
end
