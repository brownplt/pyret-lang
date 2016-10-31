import json as J
import js-file("server") as S
import file("../arr/compiler/cli-module-loader.arr") as CLI
import file("../arr/compiler/compile-structs.arr") as CS

fun compile(options):
  outfile = cases(Option) options.get("outfile"):
    | some(v) => v.s
    | none => options.get-value("program").s + ".jarr"
  end
  CLI.build-runnable-standalone(
    options.get-value("program").s,
    options.get-value("require-config").s,
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
      display-progress: options.get("display-progress").or-else(true)
    })
end

S.make-server(1700, lam(msg) block:
  print("Got message in pyret-land: " + msg)
  opts = J.read-json(msg)
  print(torepr(opts))
  print("\n")
  compile(opts.dict)
end)

