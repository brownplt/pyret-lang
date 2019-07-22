import either as E
import json as J
import pathlib as P
import string-dict as SD
import render-error-display as RED
import file("./cli-module-loader.arr") as CLI
import file("./compile-structs.arr") as CS
import file("locators/builtin.arr") as B
import js-file("webworker") as W

# this value is the limit of number of steps that could be inlined in case body
DEFAULT-INLINE-CASE-LIMIT = 5

success-code = 0
failure-code = 1

fun compile(options):
  spy: options end
  outfile = cases(Option) options.get("outfile"):
    | some(v) => v
    | none => options.get-value("program") + ".jarr"
  end
  compile-opts = CS.make-default-compile-options(options.get-value("this-pyret-dir"))
  CLI.build-runnable-standalone(
    options.get-value("program"),
    options.get-value("require-config"),
    outfile,
    compile-opts.{
      base-dir: options.get-value("base-dir"),
      this-pyret-dir : options.get-value("this-pyret-dir"),
      check-mode : not(options.get("no-check-mode").or-else(false)),
      type-check : options.get("type-check").or-else(false),
      allow-shadowed : options.get("allow-shadowed").or-else(false),
      collect-all: options.get("collect-all").or-else(false),
      ignore-unbound: options.get("ignore-unbound").or-else(false),
      proper-tail-calls: options.get("improper-tail-calls").or-else(true),
      compiled-cache: options.get("compiled-dir").or-else("./compiled"),
      compiled-read-only: options.get("compiled-read-only").or-else(empty),
      standalone-file: options.get("standalone-file").or-else(compile-opts.standalone-file),
      checks: options.get-value("checks"),
      display-progress: options.get("display-progress").or-else(true),
      log: options.get("log").or-else(compile-opts.log),
      log-error: options.get("log-error").or-else(compile-opts.log-error),
      deps-file: options.get("deps-file").or-else(compile-opts.deps-file),
      user-annotations: options.get("user-annotations").or-else(compile-opts.user-annotations),
      builtin-js-dirs: compile-opts.builtin-js-dirs.append(options.get-value("builtin-js-dirs")),
    })
end
pyret-dir = ""
compile-handler = lam(msg, send-message) block:
  # print("Got message in pyret-land: " + msg)
  opts = J.read-json(msg).native()
  # print(torepr(opts))
  # print("\n")
  builtin-js-dirs = if opts.has-key("builtin-js-dir"):
    if is-List(opts.get-value("builtin-js-dir")):
        opts.get-value("builtin-js-dir")
      else:
        [list: opts.get-value("builtin-js-dir")]
      end
  else:
    empty
  end
  when opts.has-key("builtin-arr-dir"):
    if is-List(opts.get-value("builtin-arr-dir")):
      B.set-builtin-arr-dirs(opts.get-value("builtin-arr-dir"))
    else:
      B.set-builtin-arr-dirs([list: opts.get-value("builtin-arr-dir")])
    end
  end
  when opts.has-key("allow-builtin-overrides"):
    B.set-allow-builtin-overrides(opts.get-value("allow-builtin-overrides"))
  end
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
  with-pyret-dir = with-error.set("this-pyret-dir", pyret-dir)
  with-compiled-read-only-dirs =
    if opts.has-key("perilous") and opts.get-value("perilous"):
      with-pyret-dir.set("compiled-read-only",
        link(P.resolve(P.join(pyret-dir, "../../src/runtime")), empty)
      ).set("user-annotations", false)
    else:
      with-pyret-dir.set("compiled-read-only",
        link(P.resolve(P.join(pyret-dir, "../../src/runtime")), empty)
      )
    end
  with-require-config = with-compiled-read-only-dirs.set("require-config",
    opts.get("require-config").or-else(P.resolve(P.join(pyret-dir, "config.json"))))

  with-builtin-js-dirs = with-require-config.set("builtin-js-dirs", builtin-js-dirs)
  with-spies-flag = with-builtin-js-dirs.set("enable-spies", with-spies-flag)

  result = run-task(lam():
    compile(with-builtin-js-dirs)
  end)

  # result = compile(with-builtin-js-dirs) 
  # run-task(lam():
  #  compile(with-require-config)
  # end)
  cases(E.Either) result block:
    | right(exn) =>
      err-str = RED.display-to-string(exn-unwrap(exn).render-reason(), tostring, empty)
      err(err-str + "\n")
      d = [SD.string-dict: "type", J.j-str("compile-failure")]
      send-message(J.j-obj(d).serialize())
    | left(val) =>
      d = [SD.string-dict: "type", J.j-str("compile-success")]
      send-message(J.j-obj(d).serialize())
      nothing
  end
end

W.setupHandlers(compile-handler)
