provide *
import runtime-lib as R
import builtin-modules as B
import make-standalone as MS
import load-lib as L
import either as E
import json as JSON
import ast as A
import pathlib as P
import sha as crypto
import string-dict as SD
import render-error-display as RED
import file as F
import filelib as FS
import error as ERR
import system as SYS
import file("js-ast.arr") as J
import file("concat-lists.arr") as C
import file("compile-lib.arr") as CL
import file("compile-structs.arr") as CS
import file("locators/file.arr") as FL
import file("locators/builtin.arr") as BL
import file("locators/jsfile.arr") as JSF
import file("js-of-pyret.arr") as JSP
import file("locators/shared-gdrive.arr") as SGD

j-fun = J.j-fun
j-var = J.j-var
j-id = J.j-id
j-method = J.j-method
j-block = J.j-block
j-true = J.j-true
j-false = J.j-false
j-num = J.j-num
j-str = J.j-str
j-return = J.j-return
j-assign = J.j-assign
j-if = J.j-if
j-if1 = J.j-if1
j-new = J.j-new
j-app = J.j-app
j-list = J.j-list
j-obj = J.j-obj
j-dot = J.j-dot
j-bracket = J.j-bracket
j-field = J.j-field
j-dot-assign = J.j-dot-assign
j-bracket-assign = J.j-bracket-assign
j-try-catch = J.j-try-catch
j-throw = J.j-throw
j-expr = J.j-expr
j-binop = J.j-binop
j-and = J.j-and
j-lt = J.j-lt
j-eq = J.j-eq
j-neq = J.j-neq
j-geq = J.j-geq
j-unop = J.j-unop
j-decr = J.j-decr
j-incr = J.j-incr
j-not = J.j-not
j-instanceof = J.j-instanceof
j-ternary = J.j-ternary
j-null = J.j-null
j-parens = J.j-parens
j-switch = J.j-switch
j-case = J.j-case
j-default = J.j-default
j-label = J.j-label
j-break = J.j-break
j-while = J.j-while
j-for = J.j-for

clist = C.clist

type Loadable = CL.Loadable


type Either = E.Either

fun uri-to-path(uri, name):
  name + "-" + crypto.sha256(uri)
end

# NOTE(joe): This is just a little one-off type to represent a simple
# situation: Builtin pure-JS files are stored in single files with a hash
# followed by .js, while builtin Pyret files are stored in two files – one with
# just static info and one with all the generated code. The CLI system needs to
# know which kind it is to look up the right cached files

data CachedType:
  | split
  | single-file
end

# NOTE(joe): This has its arguments listed instead of taking a Locator because
# when we have cached, built standalones in releases, we need to do this check
# without constructing a locator that knows about the source. In that case,
# it's fine to pass a modified time of 0 to indicate that we're always happy
# with the compiled version of the file.

fun cached-available(basedir, uri, name, modified-time) -> Option<CachedType>:
  saved-path = P.join(basedir, uri-to-path(uri, name))

  if (F.file-exists(saved-path + "-static.js") and
      (F.file-times(saved-path + "-static.js").mtime > modified-time)):
    some(split)
  else if (F.file-exists(saved-path + ".js") and
      (F.file-times(saved-path + ".js").mtime > modified-time)):
    some(single-file)
  else:
    none
  end
end

fun get-cached(basedir, uri, name, cache-type):
  saved-path = P.join(basedir, uri-to-path(uri, name))
  {static-path; module-path} = cases(CachedType) cache-type:
                # NOTE(joe): leaving off .js because builtin-raw-locator below
                # expects no extension
    | split => {saved-path + "-static"; saved-path + "-module"}
    | single-file => {saved-path; saved-path}
  end
  raw = B.builtin-raw-locator(static-path)
  {
    method needs-compile(_, _): false end,
    method get-modified-time(self):
      0
      # F.file-times(static-path + ".js").mtime
    end,
    method get-options(self, options):
      options.{ checks: "none" }
    end,
    method get-module(_):
      raise("Should never fetch source for builtin module " + static-path)
    end,
    method get-extra-imports(self):
      CS.standard-imports
    end,
    method get-dependencies(_):
      deps = raw.get-raw-dependencies()
      raw-array-to-list(deps).map(CS.make-dep)
    end,
    method get-native-modules(_):
      natives = raw.get-raw-native-modules()
      raw-array-to-list(natives).map(CS.requirejs)
    end,
    method get-globals(_):
      CS.standard-globals
    end,

    method uri(_): uri end,
    method name(_): name end,

    method set-compiled(_, _, _): nothing end,
    method get-compiled(self):
      provs = CS.provides-from-raw-provides(self.uri(), {
          uri: self.uri(),
          values: raw-array-to-list(raw.get-raw-value-provides()),
          aliases: raw-array-to-list(raw.get-raw-alias-provides()),
          datatypes: raw-array-to-list(raw.get-raw-datatype-provides())
        })
      some(CL.module-as-string(provs, CS.minimal-builtins,
          CS.ok(JSP.ccp-file(F.real-path(module-path + ".js")))))
    end,

    method _equals(self, other, req-eq):
      req-eq(self.uri(), other.uri())
    end
  }
end

fun get-cached-if-available(basedir, loc) block:
  saved-path = P.join(basedir, uri-to-path(loc.uri(), loc.name()))
  cached-type = cached-available(basedir, loc.uri(), loc.name(), loc.get-modified-time())
  cases(Option) cached-type:
    | none => loc
    | some(ct) => get-cached(basedir, loc.uri(), loc.name(), ct)
  end
end

fun get-file-locator(basedir, real-path):
  loc = FL.file-locator(real-path, CS.standard-globals)
  get-cached-if-available(basedir, loc)
end

fun get-builtin-locator(basedir, read-only-basedirs, modname):
  all-dirs = link(basedir, read-only-basedirs)

  first-available = for find(rob from all-dirs):
    is-some(cached-available(rob, "builtin://" + modname, modname, 0))
  end
  cases(Option) first-available:
    | none =>
      cases(Option) BL.maybe-make-builtin-locator(modname) block:
        | some(loc) =>
          get-cached-if-available(basedir, loc)
        | none =>
          raise("Could not find builtin module " + modname + " in any of " + all-dirs.join-str(", "))
      end
    | some(ro-basedir) =>
      ca = cached-available(ro-basedir, "builtin://" + modname, modname, 0).or-else(split)
      get-cached(ro-basedir, "builtin://" + modname, modname, ca)
  end
end

fun get-builtin-test-locator(basedir, modname):
  loc = BL.make-builtin-locator(modname).{
    method uri(_): "builtin-test://" + modname end
  }
  get-cached-if-available(basedir, loc)
end

fun get-loadable(basedir, read-only-basedirs, l) -> Option<Loadable>:
  locuri = l.locator.uri()
#  cached = cached-available(basedir, l.locator.uri(), l.locator.name(), l.locator.get-modified-time())
  first-available = for find(rob from link(basedir, read-only-basedirs)):
    is-some(cached-available(rob, l.locator.uri(), l.locator.name(), l.locator.get-modified-time()))
  end
  cases(Option) first-available block:
    | none => none
    | some(found-basedir) => 
      c = cached-available(found-basedir, l.locator.uri(), l.locator.name(), l.locator.get-modified-time())
      saved-path = P.join(found-basedir, uri-to-path(locuri, l.locator.name()))
      {static-path; module-path} = cases(CachedType) c.or-else(single-file):
        | split =>
          {saved-path + "-static"; saved-path + "-module.js"}
        | single-file =>
          {saved-path; saved-path + ".js"}
      end
      raw-static = B.builtin-raw-locator(static-path)
      provs = CS.provides-from-raw-provides(locuri, {
        uri: locuri,
        values: raw-array-to-list(raw-static.get-raw-value-provides()),
        aliases: raw-array-to-list(raw-static.get-raw-alias-provides()),
        datatypes: raw-array-to-list(raw-static.get-raw-datatype-provides())
      })
      some(CL.module-as-string(provs, CS.minimal-builtins, CS.ok(JSP.ccp-file(module-path))))
  end
end

fun set-loadable(basedir, locator, loadable) -> String block:
  doc: "Returns the module path of the cached file"
  when not(FS.exists(basedir)):
    FS.create-dir(basedir)
  end
  locuri = loadable.provides.from-uri
  cases(CS.CompileResult) loadable.result-printer block:
    | ok(ccp) =>
      save-static-path = P.join(basedir, uri-to-path(locuri, locator.name()) + "-static.js")
      save-module-path = P.join(basedir, uri-to-path(locuri, locator.name()) + "-module.js")
      fs = F.output-file(save-static-path, false)
      fm = F.output-file(save-module-path, false)

      ccp.print-js-runnable(fm.display)

      # NOTE(joe August 2017): This is a little bit dumb. When caching a file,
      # if we have enough information, split it into -static and -module
      # pieces.  If we don't have a dictionary of this information, save two
      # copies of it. We simply don't have enough metadata floating around to
      # make good decisions at fetch time. The copying is fairly innocuous,
      # because it only happens for hand-written JS files, which are smaller.
      # But this is a point to revisit.

      if JSP.is-ccp-dict(ccp):
        ccp.print-js-static(fs.display)
      else:
        ccp.print-js-runnable(fs.display)
      end

      fs.flush()
      fs.close-file()
      fm.flush()
      fm.close-file()

      save-module-path
    | err(_) => ""
  end
end

fun get-cli-module-storage(storage-dir :: String, extra-dirs :: List<String>):
  {
    method load-modules(self, to-compile) block:
      maybe-modules = for map(t from to-compile):
        get-loadable(storage-dir, extra-dirs, t)
      end
      modules = [SD.mutable-string-dict:]
      for each2(m from maybe-modules, t from to-compile):
        cases(Option<Loadable>) m:
          | none => nothing
          | some(shadow m) =>
            modules.set-now(t.locator.uri(), m)
        end
      end
      modules
    end
  }
end

type CLIContext = {
  current-load-path :: String,
  cache-base-dir :: String
}

fun module-finder(ctxt :: CLIContext, dep :: CS.Dependency):
  cases(CS.Dependency) dep:
    | dependency(protocol, args) =>
      if protocol == "file":
        clp = ctxt.current-load-path
        this-path = dep.arguments.get(0)
        real-path = P.join(clp, this-path)
        new-context = ctxt.{current-load-path: P.dirname(real-path)}
        if F.file-exists(real-path):
          CL.located(get-file-locator(ctxt.cache-base-dir, real-path), new-context)
        else:
          raise("Cannot find import " + torepr(dep))
        end
      else if protocol == "builtin-test":
        l = get-builtin-test-locator(ctxt.cache-base-dir, args.first)
        force-check-mode = l.{
          method get-options(self, options):
            options.{ checks: "all", type-check: false }
          end
        }
        CL.located(force-check-mode, ctxt)
      else if protocol == "file-no-cache":
        clp = ctxt.current-load-path
        this-path = dep.arguments.get(0)
        real-path = P.join(clp, this-path)
        new-context = ctxt.{current-load-path: P.dirname(real-path)}
        if F.file-exists(real-path):
          CL.located(FL.file-locator(real-path, CS.standard-globals), new-context)
        else:
          raise("Cannot find import " + torepr(dep))
        end
      else if protocol == "js-file":
        clp = ctxt.current-load-path
        this-path = dep.arguments.get(0)
        real-path = P.join(clp, this-path)
        new-context = ctxt.{current-load-path: P.dirname(real-path)}
        locator = JSF.make-jsfile-locator(real-path)
        CL.located(locator, new-context)
      else if protocol == "shared-gdrive":
        clp = ctxt.current-load-path
        filename = dep.arguments.get(0)
        program-id = dep.arguments.get(1)
        _ = print("\nLoading: shared-gdrive(" + filename + ", " + program-id + ")\n")

        # TODO: change caching context...
        CL.located(SGD.shared-gdrive-file-locator(filename, program-id, CS.standard-globals), ctxt)
      else:
        raise("Unknown import type: " + protocol)
      end
    | builtin(modname) =>
      CL.located(get-builtin-locator(ctxt.cache-base-dir, ctxt.compiled-read-only-dirs, modname), ctxt)
  end
end

default-start-context = {
  current-load-path: P.resolve("./"),
  cache-base-dir: P.resolve("./compiled"),
  compiled-read-only-dirs: empty
}

default-test-context = {
  current-load-path: P.resolve("./"),
  cache-base-dir: P.resolve("./tests/compiled"),
  compiled-read-only-dirs: empty
}

fun compile(path, options):
  base-module = CS.dependency("file", [list: path])
  base = module-finder({
    current-load-path: P.resolve(options.base-dir),
    cache-base-dir: options.compiled-cache,
    compiled-read-only-dirs: options.compiled-read-only.map(P.resolve)
  }, base-module)
  wl = CL.compile-worklist(module-finder, base.locator, base.context)
  compiled = CL.compile-program(wl, options)
  compiled
end

fun handle-compilation-errors(problems, options) block:
  for lists.each(e from problems) block:
    options.log-error(RED.display-to-string(e.render-reason(), torepr, empty))
    options.log-error("\n")
  end
  raise("There were compilation errors")
end

fun propagate-exit(result) block:
  when L.is-exit(result):
    code = L.get-exit-code(result)
    SYS.exit(code)
  end
  when L.is-exit-quiet(result):
    code = L.get-exit-code(result)
    SYS.exit-quiet(code)
  end
end

fun run(path, options, subsequent-command-line-arguments):
  stats = SD.make-mutable-string-dict()
  maybe-program = build-program(path, options, stats)
  cases(Either) maybe-program block:
    | left(problems) =>
      handle-compilation-errors(problems, options)
    | right(program) =>
      command-line-arguments = link(path, subsequent-command-line-arguments)
      result = L.run-program(R.make-runtime(), L.empty-realm(), program.js-ast.to-ugly-source(), options, command-line-arguments)
      if L.is-success-result(result):
        L.render-check-results(result)
      else:
        _ = propagate-exit(result)
        L.render-error-message(result)
      end
  end
end

fun build-program(path, options, stats) block:
  doc: ```Returns the program as a JavaScript AST of module list and dependency map,
          and its native dependencies as a list of strings```

  print-progress-clearing = lam(s, to-clear):
    when options.display-progress:
      options.log(s, to-clear)
    end
  end
  print-progress = lam(s): print-progress-clearing(s, none) end
  var str = "Gathering dependencies..."
  fun clear-and-print(new-str) block:
    print-progress-clearing(new-str, some(string-length(str)))
    str := new-str
  end
  print-progress(str)
  base-module = CS.dependency("file", [list: path])
  base = module-finder({
    current-load-path: P.resolve(options.base-dir),
    cache-base-dir: options.compiled-cache,
    compiled-read-only-dirs: options.compiled-read-only.map(P.resolve)
  }, base-module)
  clear-and-print("Compiling worklist...")
  wl = CL.compile-worklist(module-finder, base.locator, base.context)
  clear-and-print("Loading existing compiled modules...")
  storage = get-cli-module-storage(options.compiled-cache, options.compiled-read-only)
  starter-modules = storage.load-modules(wl)

  cached-modules = starter-modules.count-now()
  total-modules = wl.length() - cached-modules
  var num-compiled = 0
  when total-modules == 0:
    clear-and-print("All modules already compiled. Cleaning up and generating standalone...\n")
  end
  shadow options = options.{
    method before-compile(_, locator) block:
      num-compiled := num-compiled + 1
      clear-and-print("Compiling " + num-to-string(num-compiled) + "/" + num-to-string(total-modules)
          + ": " + locator.name())
    end,
    method on-compile(_, locator, loadable, trace) block:
      locator.set-compiled(loadable, SD.make-mutable-string-dict()) # TODO(joe): What are these supposed to be?
      clear-and-print(num-to-string(num-compiled) + "/" + num-to-string(total-modules)
          + " modules compiled " + "(" + locator.name() + ")")
      when options.collect-times:
        comp = for map(stage from trace):
          stage.name + ": " + tostring(stage.time) + "ms"
        end
        stats.set-now(locator.name(), comp)
      end
      when num-compiled == total-modules:
        print-progress("\nCleaning up and generating standalone...\n")
      end
      module-path = set-loadable(options.compiled-cache, locator, loadable)
      if (num-compiled == total-modules) and options.collect-all:
        # Don't squash the final JS-AST if we're collecting all of them, so
        # it can be pretty-printed after all
        loadable
      else:
        cases(CL.Loadable) loadable:
          | module-as-string(prov, env, rp) =>
            CL.module-as-string(prov, env, CS.ok(JSP.ccp-file(module-path)))
          | else => loadable
        end
      end
    end
  }
  ans = CL.compile-standalone(wl, starter-modules, options)
  ans
end

fun build-runnable-standalone(path, require-config-path, outfile, options) block:
  stats = SD.make-mutable-string-dict()
  maybe-program = build-program(path, options, stats)
  cases(Either) maybe-program block:
    | left(problems) =>
      handle-compilation-errors(problems, options)
    | right(program) =>
      config = JSON.read-json(F.file-to-string(require-config-path)).dict.unfreeze()
      config.set-now("out", JSON.j-str(P.resolve(P.join(options.base-dir, outfile))))
      when not(config.has-key-now("baseUrl")):
        config.set-now("baseUrl", JSON.j-str(options.compiled-cache))
      end

      when options.collect-times: stats.set-now("standalone", time-now()) end
      make-standalone-res = MS.make-standalone(program.natives, program.js-ast,
        JSON.j-obj(config.freeze()).serialize(), options.standalone-file,
        options.deps-file, options.this-pyret-dir)

      html-res = if is-some(options.html-file):
        MS.make-html-file(outfile, options.html-file.value)
      else:
        true
      end

      ans = make-standalone-res and html-res
      
      when options.collect-times block:
        standalone-end = time-now() - stats.get-value-now("standalone")
        stats.set-now("standalone", [list: "Outputing JS: " + tostring(standalone-end) + "ms"])
        for SD.each-key-now(key from stats):
          print(key + ": \n" + stats.get-value-now(key).join-str(", \n") + "\n")
        end
      end
      ans
  end
end

fun build-require-standalone(path, options):
  stats = SD.make-mutable-string-dict()
  program = build-program(path, options, stats)

  natives = j-list(true, for C.map_list(n from program.natives): n end)

  define-name = j-id(A.s-name(A.dummy-loc, "define"))

  prog = j-block([clist:
      j-app(define-name, [clist: natives, j-fun(J.next-j-fun-id(), [clist:],
        j-block([clist:
          j-return(program.js-ast)
        ]))
      ])
    ])

  print(prog.to-ugly-source())
end
