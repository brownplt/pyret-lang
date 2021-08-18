import either as E
import json as J
import option as O
import js-file("ts-pathlib") as P
import string-dict as SD
import file("./render-error-display.arr") as RED
import file("./ast-util.arr") as AU
import file("./cli-module-loader.arr") as CLI
import file("./compile-structs.arr") as CS
import file("./compile-lib.arr") as CL
import js-file("./ts-compile-options") as CO
import file("./file.arr") as F
import js-file("./ts-js-of-pyret") as JSP
import file("./locators/builtin.arr") as B
import file("locators/file.arr") as FL
import file("./message.arr") as M
import js-file("webworker") as W

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
  cases(O.Option) M.parse-request(msg):
    | none =>
      M.failure("Failed to parse " + msg).send-using(send-message)
    | some(request) =>
      cases(M.Request) request block:
        | session-delete(session) => 
          cases(Option) CLI.delete-session(session):
            | some(message) => M.failure(message).send-using(send-message)
            | none => M.success.send-using(send-message)
          end
        | session-filter(session, pattern) => 
          cases(Option) CLI.filter-session(session, pattern):
            | some(message) => M.failure(message).send-using(send-message)
            | none => M.success.send-using(send-message)
          end
        | lint-program(program, program-source) =>
          opts = request.get-options()
          spy: opts end
          cases(E.Either) CLI.lint(program-source, program, opts) block:
            | left(errors) =>
              err-list = for map(e from errors):
                J.j-str(RED.display-to-json(e.render-reason(), tostring, empty))
              end
              M.lint-failure(program-source, err-list).send-using(send-message)
            | right(_) =>
              M.lint-success(program-source).send-using(send-message)
              nothing
          end
        | compile-program(
            program,
            base-dir,
            builtin-js-dir,
            checks,
            type-check,
            recompile-builtins,
            pipeline,
            session) =>
          opts = request.get-options()
          spy: opts end
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
          
          cases(E.Either) run-task(lam(): compile(with-error, pyret-dir) end):
            | right(exn) =>
              err-str = RED.display-to-json(exn-unwrap(exn).render-reason(), tostring, empty)
              err-list = [list: J.j-str(err-str)]
              M.compile-failure(err-list).send-using(send-message)
            | left(val) =>
              cases(E.Either) val block:
                | left(errors) =>
                  err-list = for map(e from errors):
                    J.j-str(RED.display-to-json(e.render-fancy-reason(), tostring, empty))
                  end
                  M.compile-failure(err-list).send-using(send-message)
                | right(value) =>
                  M.compile-success.send-using(send-message)
                  nothing
              end
          end
      end
  end
end

W.setupHandlers(compile-handler)