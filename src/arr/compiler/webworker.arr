import either as E
import json as J
import option as O
import pathlib as P
import string-dict as SD
import render-error-display as RED
import file("./cli-module-loader.arr") as CLI
import file("./compile-structs.arr") as CS
import file("locators/builtin.arr") as B
import js-file("webworker") as W
import file("compile-options.arr") as CO

# this value is the limit of number of steps that could be inlined in case body
DEFAULT-INLINE-CASE-LIMIT = 5

success-code = 0
failure-code = 1
pyret-dir = "."

data Request:
  | lint-program(
      program :: String,
      program-source :: String)
  | compile-program(
      program :: String,
      base-dir :: String,
      builtin-js-dir :: String,
      checks :: Boolean,
      type-check :: Boolean,
      recompile-builtins :: Boolean)
end

fun bind-option<AA, BB>(a :: O.Option<AA>, f :: (AA -> O.Option<BB>)) -> O.Option<BB>:
  cases(O.Option) a:
    | none =>
      none
    | some(a-value) =>
      f(a-value)
  end
end

# Creates a lint-program Request out of a dict, returning none when the dict could not be
# parsed as a lint-program Request.
fun parse-lint-dict(dict :: SD.StringDict<Any>) -> O.Option<Request % (is-lint-program)>:
  dict.get("program").and-then(
    lam(program):
      dict.get("program-source").and-then(
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
                              not(checks == "none"),
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
fun parse-message(message :: String) -> Request block:
  dict :: SD.StringDict<Any> = J.read-json(message).native()
  result :: O.Option<Request> = parse-dict(dict)
  cases(O.Option) result:
    | some(request) =>
      request
    | none =>
      raise("Couldn't parse message from webworker: " + message)
  end
end

fun compile(options, this-pyret-dir):
  outfile = cases(Option) options.get("outfile"):
    | some(v) => v
    | none => options.get-value("program") + ".jarr"
  end
  compile-opts = CO.populate-options(options, this-pyret-dir)
  CLI.build-runnable-standalone(
    options.get-value("program"),
    compile-opts.require-config,
    outfile,
    compile-opts
    )
end

compile-handler = lam(msg, send-message) block:
  # print("Got message in pyret-land: " + msg)
  request = parse-message(msg)
  opts = J.read-json(msg).native() 

  spy: opts, msg end
  
  fun log(s, to-clear):
    d = [SD.string-dict: "type", J.j-str("echo-log"), "contents", J.j-str(s)]
    with-clear = cases(Option) to-clear:
      | none => d.set("clear-first", J.j-bool(false))
      | some(n) => d.set("clear-first", J.j-num(n))
    end
    send-message(J.j-obj(with-clear).serialize())
  end
  fun err(s):
    d = [SD.string-dict: "type", J.j-str("echo-err"), "contents", J.j-str(s)]
    send-message(J.j-obj(d).serialize())
  end
  # enable-spies = not(opts.has-key("no-spies"))
  with-logger = opts.set("log", log)
  with-error = with-logger.set("log-error", err)
  compile-opts = CO.populate-options(with-error, pyret-dir)

  cases(Request) request:
    | lint-program(program, program-source) =>
      cases(E.Either) CLI.lint(program-source, program) block:
        | left(errors) =>
          err-list = for map(e from errors):
            J.j-str(RED.display-to-string(e.render-reason(), tostring, empty))
          end
          d = [SD.string-dict:
            "type", J.j-str("lint-failure"),
            "data", J.j-obj([SD.string-dict:
                "name", J.j-str(program-source),
                "errors", J.j-arr(err-list)])]
          send-message(J.j-obj(d).serialize())
        | right(_) =>
          d = [SD.string-dict:
            "type", J.j-str("lint-success"),
            "data", J.j-obj([SD.string-dict:
                "name", J.j-str(program-source)])]
          send-message(J.j-obj(d).serialize())
          nothing
      end
    | compile-program(
        program,
        base-dir,
        builtin-js-dir,
        checks,
        type-check,
        recompile-builtins) =>
      cases(E.Either) run-task(lam(): compile(with-error, pyret-dir) end):
        | right(exn) =>
          err-str = RED.display-to-string(exn-unwrap(exn).render-reason(), tostring, empty)
          err-list = [list: J.j-str(err-str)]
          d = [SD.string-dict:
            "type", J.j-str("compile-failure"),
            "data", J.j-arr(err-list)]
          send-message(J.j-obj(d).serialize())
        | left(val) =>
          cases(E.Either) val block:
            | left(errors) =>
              err-list = for map(e from errors):
                J.j-str(RED.display-to-string(e.render-reason(), tostring, empty))
              end
              d = [SD.string-dict:
                "type", J.j-str("compile-failure"),
                "data", J.j-arr(err-list)]
              send-message(J.j-obj(d).serialize())
            | right(value) =>
              d = [SD.string-dict:
                "type", J.j-str("compile-success")]
              send-message(J.j-obj(d).serialize())
              nothing
          end
      end
  end
end

W.setupHandlers(compile-handler)