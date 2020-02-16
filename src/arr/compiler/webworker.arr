import either as E
import json as J
import option as O
import pathlib as P
import string-dict as SD
import render-error-display as RED
import file("./ast-util.arr") as AU
import file("./cli-module-loader.arr") as CLI
import file("./compile-structs.arr") as CS
import file("./compile-lib.arr") as CL
import file("./compile-options.arr") as CO
import file("./file.arr") as F
import file("./js-of-pyret.arr") as JSP
import file("./locators/builtin.arr") as B
import file("./message.arr") as M
import file("./repl.arr") as R
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

var repl :: Option<R.ChunkyRepl> = none

compile-handler = lam(msg, send-message) block:
  spy: msg end
  cases(O.Option) M.parse-request(msg):
    | none =>
      nothing
    | some(request) =>
      cases(M.Request) request block:
        | lint-program(program, program-source) =>
          opts = request.get-options()
          spy: opts end
          cases(E.Either) CLI.lint(program-source, program) block:
            | left(errors) =>
              err-list = for map(e from errors):
                J.j-str(RED.display-to-string(e.render-reason(), tostring, empty))
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
            recompile-builtins) =>
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
              err-str = RED.display-to-string(exn-unwrap(exn).render-reason(), tostring, empty)
              err-list = [list: J.j-str(err-str)]
              M.compile-failure(err-list).send-using(send-message)
            | left(val) =>
              cases(E.Either) val block:
                | left(errors) =>
                  err-list = for map(e from errors):
                    J.j-str(RED.display-to-string(e.render-reason(), tostring, empty))
                  end
                  M.compile-failure(err-list).send-using(send-message)
                | right(value) =>
                  M.compile-success.send-using(send-message)
                  nothing
              end
          end
        | create-repl =>
          builtin-js-dir = "/compiled/builtin"
          
          fun get-builtin-loadable(raw, uri) -> CL.Loadable:
            provs = CS.provides-from-raw-provides(uri, {
                uri: uri,
                values: raw-array-to-list(raw.get-raw-value-provides()),
                aliases: raw-array-to-list(raw.get-raw-alias-provides()),
                datatypes: raw-array-to-list(raw.get-raw-datatype-provides())
              })
            CL.module-as-string(
              AU.canonicalize-provides(provs, CS.no-builtins),
              CS.no-builtins,
              CS.ok(JSP.ccp-string(raw.get-raw-compiled())))
          end
          
          fun get-builtin-modules() -> SD.MutableStringDict<CS.Loadable> block:
            modules = [SD.mutable-string-dict: ]
            for each(b from F.list-files(builtin-js-dir)):
              modules.set-now(b.uri, get-builtin-loadable(b.raw, b.uri))
            end
            modules
          end
          
          fun make-find-module() -> (String, CS.Dependency -> CL.Located<String>):
            locator-cache = [SD.mutable-string-dict: ]
            fun find-module(dependency):
              uri :: String = cases(CS.Dependency) dependency:
                | builtin(modname) =>
                  "builtin://" + modname
                | dependency(protocol, arguments) =>
                  raise("non-builtin dependencies not yet implemented")
                  #arr = array-from-list(arguments)
                  #if protocol == "my-gdrive":
                  #  "my-gdrive://" + arr.get-now(0)
                  #else if protocol == "shared-gdrive":
                  #  "shared-gdrive://" + arr.get-now(0) + ":" + arr.get-now(1)
                  #else if protocol == "gdrive-js":
                  #  "gdrive-js://" + arr.get-now(1)
                  #else:
                  #  print("Unknown import: " + dependency + "\n")
                  #  protocol + "://" + arguments.join-str(":")
                  #end
              end
              if locator-cache.has-key(uri) block:
                CL.located(locator-cache.get-now(uri), nothing)
              else:
                l = cases(CS.Dependency) dependency:
                  | builtin(name) =>
                    B.make-builtin-js-locator(builtin-js-dir, name)
                  | dependency(protocol, args) =>
                    raise("non-builtin dependencies not yet implemented")
                end
                locator-cache.set-now(uri, l)
                CL.located(l, nothing)
              end
            end
            find-module
          end
          
          modules = get-builtin-modules()
          compile-context = "anchor-context-currently-unused"
          make-finder = make-find-module()
          repl := R.make-chunky-repl(modules, compile-context, make-finder)
          M.create-repl-success.send-using(send-message)
      end
  end
end

W.setupHandlers(compile-handler)