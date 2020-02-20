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
  | create-repl
sharing:
  method get-options(self :: Request) -> SD.StringDict<Any>:
    cases(Request) self:
      | lint-program(program, program-source) =>
        [SD.string-dict:
          "program", program,
          "program-source", program-source]
      | compile-program(
          program, base-dir, builtin-js-dir, checks, type-check, recompile-builtins) =>
        [SD.string-dict:
          "program", program,
          "base-dir", base-dir,
          "builtin-js-dir", builtin-js-dir,
          "checks", checks,
          "type-check", type-check,
          "recompile-builtins", recompile-builtins]
      | create-repl =>
        raise(".get-options not implemented for create-repl")
    end
  end
end

# Represents the union type `Number U False'
data ClearFirst:
  | clear-number(n :: Number)
  | clear-false
sharing:
  method to-json(self :: ClearFirst) -> J.JSON:
    cases(ClearFirst) self:
      | clear-number(n) =>
        J.j-num(n)
      | clear-false =>
        J.j-bool(false)
    end
  end
end

# A response from the webworker which can be serialized and sent as a message to the page.
data Response:
  | echo-log(contents :: String, clear-first :: ClearFirst)
  | err(contents :: String)
  | lint-failure(program-source, err-list)
  | lint-success(program-source)
  | create-repl-success
  | compile-failure(err-list)
  | compile-success
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
      | lint-failure(program-source, err-list) =>
        J.j-obj([SD.string-dict:
            "type", J.j-str("lint-failure"),
            "data", J.j-obj([SD.string-dict:
                "name", J.j-str(program-source),
                "errors", J.j-arr(err-list)])])
      | lint-success(program-source) =>
        J.j-obj([SD.string-dict:
            "type", J.j-str("lint-success"),
            "data", J.j-obj([SD.string-dict:
                "name", J.j-str(program-source)])])
      | create-repl-success =>
        J.j-obj([SD.string-dict:
            "type", J.j-str("create-repl-success")])
      | compile-failure(err-list) =>
        J.j-obj([SD.string-dict:
            "type", J.j-str("compile-failure"),
            "data", J.j-arr(err-list)])
      | compile-success =>
        J.j-obj([SD.string-dict:
            "type", J.j-str("compile-success")])
    end
  end,
  method send-using(self :: Response, sender :: (String -> Nothing)) -> Nothing:
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
          some(lint-program(program, program-source))
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

fun parse-create-repl-dict(dict :: SD.StringDict<Any>) -> O.Option<Request % (is-create-repl)>:
  create-repl
end

# Creates a Request out of a string dict, returning none when the dict could not be parsed.
fun parse-dict(dict :: SD.StringDict<Any>) -> O.Option<Request>:
  cases(Option) dict.get("request"):
    | none => none
    | some(request) =>
      if request == "lint-program":
        parse-lint-dict(dict)
      else if request == "compile-program":
        parse-compile-dict(dict)
      else if request == "create-repl":
        parse-create-repl-dict(dict)
      else:
        none
      end
  end
end

# Creates a Request out of a String, returning none when the string could not be parsed.
fun parse-request(message :: String) -> O.Option<Request>:
  dict :: SD.StringDict<Any> = J.read-json(message).native()
  parse-dict(dict)
end