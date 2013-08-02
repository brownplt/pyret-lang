#lang pyret

import Racket as R
provide {
  flush-output: flush-output,
  flush-error: flush-error,
  read-line: read-line,
} end

ip = R("racket/base")("current-input-port")
op = R("racket/base")("current-output-port")
ep = R("racket/base")("current-error-port")

fun flush-output():
  R("racket/base")("flush-output", op)
  nothing
end

fun flush-error():
  R("racket/base")("flush-output", ep)
  nothing
end

s-any = R("racket/base")("string->symbol", "any")

fun read-line():
  R("racket/base")("read-line", ip, s-any)
end
