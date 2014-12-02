#lang pyret

import cmdline as C
import pprint as PP
import parse-pyret as P
import string-dict as D
import file as F
fun just-parse(file):
  P.parse-dialect("Pyret", F.file-to-string(file), file)
end

options = [D.string-dict:
  "width", C.next-val-default(C.Number, 80, some("w"), C.once, "Pretty-printed width")
]

parsed = C.parse-cmdline(options)

cases (C.ParsedArguments) parsed:
  | success(opts, rest) =>
    cases (List) rest:
      | empty => print("Require a file name")
      | link(file, _) =>
        each(print, just-parse(file).tosource().pretty(opts.get-value("width")))
    end
  | arg-error(m, _) =>
    each(print,  ("Error: " + m) ^ link(C.usage-info(options)))
end
