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
import file("./message.arr") as M

pyret-dir = "."

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
  request = M.parse-request(msg)
  opts = request.get-options()

  spy: opts, msg end
  
  fun log(s, to-clear):
    clear-first = cases(Option) to-clear:
      | none =>
        M.clear-false
      | some(n) =>
        M.clear-number(n)
    end
    M.echo-log(s, clear-first).send-using(send-message)
  end
  fun err(s):
    M.err(s).send-using(send-message)
  end
  # enable-spies = not(opts.has-key("no-spies"))
  with-logger = opts.set("log", log)
  with-error = with-logger.set("log-error", err)
  # compile-opts = CO.populate-options(with-error, pyret-dir)

  cases(M.Request) request:
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