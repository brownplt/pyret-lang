import file as F
import pathlib as P
import render-error-display as RED
import string-dict as D
import system as SYS
import file("cmdline.arr") as C
import file("cli-module-loader.arr") as CLI
import file("compile-lib.arr") as CL
import file("compile-structs.arr") as CS
import file("locators/builtin.arr") as B
import file("server.arr") as S

# this value is the limit of number of steps that could be inlined in case body
DEFAULT-INLINE-CASE-LIMIT = 5

success-code = 0
failure-code = 1

fun compile(options):
  f = raise("FOO")
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
      user-annotations: options.get("user-annotations").or-else(compile-opts.user-annotations)
    })
end
