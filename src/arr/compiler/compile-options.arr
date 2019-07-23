provide *

import pathlib as P
import file("./compile-structs.arr") as CS
import file("locators/builtin.arr") as B

fun populate-options(dictionary, this-pyret-dir) block:
  compile-opts = CS.make-default-compile-options(this-pyret-dir)

  checks = 
    if dictionary.has-key("no-check-mode") or dictionary.has-key("library"): 
      "none"
    else if dictionary.has-key("checks"): 
      dictionary.get-value("checks")
    else: 
      "all" 
    end

  builtin-js-dirs = if dictionary.has-key("builtin-js-dir"):
    if is-List(dictionary.get-value("builtin-js-dir")):
        dictionary.get-value("builtin-js-dir")
      else:
        [list: dictionary.get-value("builtin-js-dir")]
      end
  else:
    empty
  end

  add-profiling = dictionary.has-key("profile")
  allow-shadowed = dictionary.has-key("allow-shadow")
  base-dir = dictionary.get("base-dir").or-else(compile-opts.base-dir)
  build-runnable = dictionary.get("build-runnable").or-else("none")
  # TODO(alex): module dir somehwere
  # module-dir = dictionary.get-value("module-load-dir")
  check-mode = not(dictionary.get("no-check-mode").or-else(false))
  collect-all = dictionary.get("collect-all").or-else(false)
  collect-times = dictionary.has-key("collect-times") and dictionary.get-value("collect-times")
  compiled-dir = dictionary.get("compiled-dir").or-else("compiled")
  deps-file = dictionary.get("deps-file").or-else(compile-opts.deps-file)
  display-progress = not(dictionary.has-key("no-display-progress"))
  enable-spies = not(dictionary.has-key("no-spies"))
  html-file = 
    if dictionary.has-key("html-file"):
      some(dictionary.get-value("html-file"))
    else:
      none
    end
  inline-case-body-limit = dictionary.get("inline-case-body-limit")
    .or-else(compile-opts.inline-case-body-limit)
  log-error = dictionary.get("log-error").or-else(compile-opts.log-error)
  log = dictionary.get("log").or-else(compile-opts.log)
  module-eval = not(dictionary.has-key("no-module-eval"))
  require-config = dictionary.get("require-config")
    .or-else(P.resolve(P.join(this-pyret-dir, "config.json")))
  runtime-annotations = not(dictionary.has-key("no-runtime-annotations"))
  runtime-builtin-relative-path = dictionary.get("runtime-builtin-relative-path")
    .or-else(compile-opts.runtime-builtin-relative-path)
  standalone-file = dictionary.get("standalone-file").or-else(compile-opts.standalone-file)
  tail-calls = not(dictionary.has-key("improper-tail-calls"))
  type-check = dictionary.has-key("type-check")
  user-annotations = not(dictionary.has-key("no-user-annotations"))
    compiled-read-only = dictionary.get("compiled-read-only-dir").or-else(empty)

  # TODO(alex): builtin arr files no longer supported; precompile them
  when dictionary.has-key("builtin-arr-dir"):
    B.set-builtin-arr-dirs(dictionary.get-value("builtin-arr-dir"))
  end

  # TODO(alex): Get rid of hidden global variable and pass through compile-structs
  when dictionary.has-key("allow-builtin-overrides"):
    B.set-allow-builtin-overrides(dictionary.get-value("allow-builtin-overrides"))
  end
  
  compile-opts.{
    add-profiling: add-profiling,
    allow-shadowed : allow-shadowed,
    base-dir: base-dir,
    build-runnable: build-runnable,
    builtin-js-dirs: compile-opts.builtin-js-dirs.append(builtin-js-dirs),
    checks : checks,
    check-mode : check-mode,
    collect-all: collect-all,
    collect-times: collect-times,
    compiled-dir: compiled-dir,
    compiled-cache: compiled-dir,
    compiled-read-only: compiled-read-only,
    deps-file: deps-file,
    display-progress: display-progress,
    enable-spies: enable-spies,
    html-file: html-file,
    ignore-unbound: false,
    inline-case-body-limit: inline-case-body-limit,
    log-error: log-error,
    log: log,
    module-eval: module-eval,
    proper-tail-calls: tail-calls,
    require-config: require-config,
    runtime-annotations: runtime-annotations,
    runtime-builtin-relative-path: runtime-builtin-relative-path,
    standalone-file: standalone-file,
    type-check : type-check,
    user-annotations: user-annotations,
  }
end
