#lang pyret

import Racket as R
import whalesong-lib as w
provide {
  with-output-to-string: with-output-to-string,
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

fun with-output-to-string(f):
  stdout = base("open-output-string")
  stderr = base("open-output-string")
  base("current-output-port", stdout)
#  base("current-error-port", stderr)
  try:
    f()
  except(e):
    print(e)
    base("current-output-port", ip)
#    base("current-error-port", ep)
    raise(e)
  end
  base("current-output-port", ip)
#  base("current-error-port", ep)
  {
    stdout: base("get-output-string", stdout),
    stderr: base("get-output-string", stderr)
  }
end

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

