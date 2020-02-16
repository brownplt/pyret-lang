# message.arr
#
# Provides data types and functions for passing messages between the webworker and the page.

provide *
provide-types *

import json as J
import option as O
import string-dict as SD

### messages: These data types encapsulate information sent between the page and the
###   webworker.

# A parsed message from the page.
data Request:
  | lint-program(
      program :: String,
      program-source :: String)
  | compile-program(
      program :: String,
      base-dir :: String,
      builtin-js-dir :: String,
      checks :: String,
      type-check :: Boolean,
      recompile-builtins :: Boolean)
end

data ClearFirst:
  | clear-number(n :: Number)
  | clear-false
sharing:
  method to-json(self :: ClearFirst) -> J.JSON:
    cases(ClearFirst) self:
      | clear-number(n) =>
        J.j-num(n)
      | clear-false =>
        J.jbool(false)
    end
  end
end

# A response from the webworker which can be serialized and sent as a message to the page.
data Response:
  | echo-log(contents :: String, clear-first :: ClearFirst)
  | err(contents :: String)
sharing:
  method to-json(self :: Response) -> J.JSON:
    cases(Response) self:
      | echo-log(contents, clear-first) =>
        J.j-obj([SD.string-dict:
            "type", J.j-str("echo-log"),
            "contents", J.j-str(contents),
            "clear-first", clear-first.to-json()])
      | err(contents) =>
        J.j-obj([SD.string-dict:
            "type", J.j-str("echo-err"),
            "contents", J.j-str(contents)])
    end
  end,
  method send-using(self :: Response, sender :: (String -> nothing)) -> nothing:
    sender(self.to-json().serialize())
  end
end

fun bind-option<AA, BB>(a :: O.Option<AA>, f :: (AA -> O.Option<BB>)) -> O.Option<BB>:
  cases(O.Option) a:
    | none =>
      none
    | some(a-value) =>
      f(a-value)
  end
end

### parsing: These functions parse strings to messages.

# Creates a lint-program Request out of a dict, returning none when the dict could not be
# parsed as a lint-program Request.
fun parse-lint-dict(dict :: SD.StringDict<Any>) -> O.Option<Request % (is-lint-program)>:
  bind-option(
    dict.get("program"),
    lam(program):
      bind-option(
        dict.get("program-source"),
        lam(program-source):
          lint-program(program, program-source)
        end)
    end)
end

# Creates a compile-program Request out of a dict, returning none when the dict could not be
# parsed as a compile Request.
fun parse-compile-dict(dict :: SD.StringDict<Any>) -> O.Option<Request % (is-compile-program)>:
  bind-option(
    dict.get("program"),
    lam(program):
      bind-option(
        dict.get("base-dir"),
        lam(base-dir):
          bind-option(
            dict.get("builtin-js-dir"),
            lam(builtin-js-dir):
              bind-option(
                dict.get("checks"),
                lam(checks):
                  bind-option(
                    dict.get("type-check"),
                    lam(type-check):
                      bind-option(
                        dict.get("recompile-builtins"),
                        lam(recompile-builtins):
                          some(compile-program(
                              program,
                              base-dir,
                              builtin-js-dir,
                              checks,
                              type-check,
                              recompile-builtins))
                        end)
                    end)
                end)
            end)
        end)
    end)
end

# Creates a Request out of a string dict, returning none when the dict could not be parsed.
fun parse-dict(dict :: SD.StringDict<Any>) -> O.Option<Request>:
  should-be-lint :: Boolean = dict.get("lint").or-else(false)
  
  if should-be-lint:
    parse-lint-dict(dict)
  else:
    parse-compile-dict(dict)
  end
end

# Creates a Request out of a String, throwing an error if the string could not be parsed.
fun parse-request(message :: String) -> Request:
  dict :: SD.StringDict<Any> = J.read-json(message).native()
  result :: O.Option<Request> = parse-dict(dict)
  cases(O.Option) result:
    | some(request) =>
      request
    | none =>
      raise("Couldn't parse message from webworker: " + message)
  end
end