provide *

import either as E
import error as ERR
import parse-pyret as PP

type Either = E.Either
type ParseError = ERR.ParseError

var i = 0
fun maybe-parse(program :: String) block:
  name = "parse-program-" + tostring(i)
  i := i + 1
  PP.maybe-surface-parse(program, name)
end

fun does-parse(program :: String):
  maybe-ast = maybe-parse(program)
  cases(Either) maybe-ast:
    | left(_) =>
      false
    | right(_) =>
      true
  end
end

fun get-parse-result(program :: String):
  maybe-ast = maybe-parse(program)
  cases(Either) maybe-ast:
    | left(_) =>
      raise("Expected success when parsing: " + program)
    | right(ast) =>
      ast
  end
end

fun get-parse-error(program :: String):
  maybe-ast = maybe-parse(program)
  cases(Either) maybe-ast:
    | left(err) =>
      err.exn
    | right(_) =>
      raise("Expected error when parsing: " + program)
  end
end

wss = [list: " ", " \n", "\n ", " \n", " \n "]
en-ops = [list: "or", "and", "is", "satisfies", "raises"]
