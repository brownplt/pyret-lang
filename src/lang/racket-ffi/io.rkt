#lang pyret

import Racket as R
import whalesong-lib as w
provide {
  flush-output: flush-output,
  flush-error: flush-error,
  read-line: read-line,
  read-sexpr: read-sexpr-wrap,
  sexpr-from-string: w.read-sexpr
} end

base = R("racket/base")
ip = base("current-input-port")
op = base("current-output-port")
ep = base("current-error-port")

fun flush-output():
  base("flush-output", op)
  nothing
end

fun flush-error():
  base("flush-output", ep)
  nothing
end

s-any = base("string->symbol", "any")

fun read-line():
  base("read-line", ip, s-any)
end

fun read-sexpr-wrap():
  p = base("open-output-string")
  d = base("read", ip)
  base("write", d, p)
  w.read-sexpr(base("get-output-string", p))
end

