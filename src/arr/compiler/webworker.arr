import either as E
import json as J
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

fun compile(options, this-pyret-dir):
  spy: options end
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
  opts = J.read-json(msg).native() 
  
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
  enable-spies = not(opts.has-key("no-spies"))
  with-logger = opts.set("log", log)
  with-error = with-logger.set("log-error", err)

  result = run-task(lam():
    compile(with-error, pyret-dir)
  end)

  # result = compile(with-builtin-js-dirs) 
  # run-task(lam():
  #  compile(with-require-config)
  # end)
  cases(E.Either) result block:
    | right(exn) =>
      err-str = RED.display-to-string(exn-unwrap(exn).render-reason(), tostring, empty)
      err-list = [list: J.j-str(err-str)]
      d = [SD.string-dict: "type", J.j-str("compile-failure"), "data", J.j-arr(err-list)]
      send-message(J.j-obj(d).serialize())
    | left(val) =>
      cases(E.Either) val block:
      | left(errors) =>
      spy "errors": errors end
        err-list = for map(e from errors):
          J.j-str(RED.display-to-string(e.render-reason(), tostring, empty))
        end
        d = [SD.string-dict: "type", J.j-str("compile-failure"), "data", J.j-arr(err-list)]
        send-message(J.j-obj(d).serialize())
      | right(value) =>
        d = [SD.string-dict: "type", J.j-str("compile-success")]
        send-message(J.j-obj(d).serialize())
        nothing
      end
  end
end

W.setupHandlers(compile-handler)
