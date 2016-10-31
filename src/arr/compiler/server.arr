provide *

import json as J
import string-dict as SD
import js-file("server") as S
import file("./cli-module-loader.arr") as CLI
import file("./compile-structs.arr") as CS

fun compile(options):
  outfile = cases(Option) options.get("outfile"):
    | some(v) => v
    | none => options.get-value("program") + ".jarr"
  end
  CLI.build-runnable-standalone(
    options.get-value("program"),
    options.get-value("require-config"),
    outfile,
    CS.default-compile-options.{
      check-mode : options.get("check-mode").or-else(true),
      type-check : options.get("type-check").or-else(false),
      allow-shadowed : options.get("allow-shadowed").or-else(false),
      collect-all: options.get("collect-all").or-else(false),
      ignore-unbound: options.get("ignore-unbound").or-else(false),
      proper-tail-calls: options.get("improper-tail-calls").or-else(true),
      compile-module: true,
      compiled-cache: options.get("compiled-dir").or-else("./compiled"),
      display-progress: options.get("display-progress").or-else(true),
      log: options.get("log").or-else(CS.default-compile-options.log),
      log-error: options.get("log-error").or-else(CS.default-compile-options.log-error)
    })
end

fun serve(port):
  S.make-server(port, lam(msg, send-message) block:
    print("Got message in pyret-land: " + msg)
    opts = J.read-json(msg).native()
    print(torepr(opts))
    print("\n")
    with-logger = opts.set("log",
      lam(s, to-clear):
        d = [SD.string-dict: "type", J.j-str("echo-log"), "contents", J.j-str(s)]
        with-clear = cases(Option) to-clear:
          | none => d.set("clear-first", J.j-bool(false))
          | some(n) => d.set("clear-first", J.j-num(n))
        end
        send-message(J.j-obj(with-clear).serialize())
      end)
    with-error = with-logger.set("log-error",
      lam(s):
        d = [SD.string-dict: "type", J.j-str("echo-err"), "contents", J.j-str(s)]
        send-message(J.j-obj(d).serialize())
      end)
    compile(with-error)
  end)
end
