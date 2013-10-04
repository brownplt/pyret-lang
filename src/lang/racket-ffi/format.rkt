#lang pyret

import "format-lib.rkt" as format-internal
provide format end

fun format(template :: String, args :: list.List):
  format-internal(template, args.map(tostring))
where:
  format("~a", ["hi"]) is "hi"
  format("~a ~a", [1, 2]) is "1 2"
end

