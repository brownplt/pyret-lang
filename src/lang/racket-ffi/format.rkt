#lang pyret

provide { format: format, fix-width: format-internal.fix-width } end

import "format-lib.rkt" as format-internal

fun format(template :: String, args :: list.List):
  format-internal.format(template, args.map(tostring))
where:
  format("~a", ["hi"]) is "hi"
  format("~a ~a", [1, 2]) is "1 2"
end

