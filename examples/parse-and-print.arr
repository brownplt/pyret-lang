#lang pyret

import cmdline as C
import pprint as PP
import ast as A
import file as F
fun just-parse(file):
  A.parse(F.file-to-string(file), file, {["check"]: false}).pre-desugar
end

options = {
  width: C.next-val-default(C.Number, 80, some("w"), C.once, "Pretty-printed width")
}

parsed = C.parse-cmdline(options)

cases (C.ParsedArguments) parsed:
  | success(opts, rest) =>
    cases (List) rest:
      | empty => print("Require a file name")
      | link(file, _) =>
        each(print, just-parse(file).tosource().pretty(opts.width))
    end
  | arg-error(m, _) =>
    each(print,  ("Error: " + m) ^ link(C.usage-info(options)))
end
