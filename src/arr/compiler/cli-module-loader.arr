provide *
import namespace-lib as N
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
import file("js-ast.arr") as J
import file("concat-lists.arr") as C
import file("compile-lib.arr") as CL
import file("compile-structs.arr") as CS
import file("locators/file.arr") as FL
import file("locators/legacy-path.arr") as LP
import file("locators/builtin.arr") as BL
import file("js-of-pyret.arr") as JSP

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

fun uri-to-path(uri):
  crypto.sha256(uri)
end

fun get-loadable(basedir, l) -> Option<Loadable>:
  locuri = l.locator.uri()
  saved-path = P.join(basedir, uri-to-path(locuri))
  if not(F.file-exists(saved-path + ".js")) or
     (F.file-times(saved-path + ".js").mtime < l.locator.get-modified-time()):
    none
  else:
    raw = B.builtin-raw-locator(saved-path)
    provs = CS.provides-from-raw-provides(locuri, {
      uri: locuri,
      values: raw-array-to-list(raw.get-raw-value-provides()),
      aliases: raw-array-to-list(raw.get-raw-alias-provides()),
      datatypes: raw-array-to-list(raw.get-raw-datatype-provides())
    })
    some(CL.module-as-string(provs, CS.minimal-builtins, CS.ok(JSP.ccp-string(raw.get-raw-compiled()))))
  end
end

fun set-loadable(basedir, locator, loadable):
  when not(FS.exists(basedir)):
    FS.create-dir(basedir)
  end
  locuri = loadable.provides.from-uri
  cases(CS.CompileResult) loadable.result-printer:
    | ok(ccp) =>
      save-path = P.join(basedir, uri-to-path(locuri) + ".js")
      f = F.output-file(save-path, false)
      f.display(ccp.pyret-to-js-runnable())
      f.close-file()
    | err(_) => nothing
  end
end

fun get-cli-module-storage(storage-dir :: String):
  {
    load-modules(self, to-compile):
      maybe-modules = for map(t from to-compile):
        get-loadable(storage-dir, t)
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
    end,

    save-modules(self, loadables):
      for each(l from loadables): set-loadable(storage-dir, l) end 
      s = for fold(s from "{\n", l from loadables):
        locuri = l.provides.from-uri
        s + "\"" + l.provides.from-uri + "\":\"" + uri-to-path(locuri) + "\"\n"
      end
      f = F.output-file(P.join(storage-dir, "modmap.json"), false)
      f.display(s + "}")
    end
  }
end

type CLIContext = {
  current-load-path :: String
}

fun module-finder(ctxt :: CLIContext, dep :: CS.Dependency):
  cases(CS.Dependency) dep:
    | dependency(protocol, args) =>
      if protocol == "file":
        clp = ctxt.current-load-path
        this-path = dep.arguments.get(0)
        real-path = P.join(clp, this-path)
        new-context = ctxt.{current-load-path: P.dirname(real-path)}
        locator = FL.file-locator(real-path, CS.standard-globals)
        CL.located(locator, new-context)
      else if protocol == "legacy-path":
        CL.located(LP.legacy-path-locator(dep.arguments.get(0)), ctxt)
      else:
        raise("Unknown import type: " + protocol)
      end
    | builtin(modname) =>
      CL.located(BL.make-builtin-locator(modname), ctxt)
  end
end

fun compile(path, options):
  base-module = CS.dependency("file", [list: path])
  base = module-finder({current-load-path: P.resolve("./")}, base-module)
  wl = CL.compile-worklist(module-finder, base.locator, base.context)
  compiled = CL.compile-program(wl, options)
  compiled
end

fun run(path, options):
  prog = build-program(path, options)
  result = L.run-program(R.make-runtime(), prog.js-ast.to-ugly-source())
  if L.is-success-result(result):
    print(L.render-check-results(result))
  else:
    print(L.render-error-message(result))
  end
end

fun build-program(path, options):
  doc: ```Returns the program as a JavaScript AST of module list and dependency map,
          and its native dependencies as a list of strings```


  print("Gathering dependencies...")
  base-module = CS.dependency("file", [list: path])
  base = module-finder({current-load-path: P.resolve("./")}, base-module)
  wl = CL.compile-worklist(module-finder, base.locator, base.context)

  storage = get-cli-module-storage(options.compiled-cache)
  starter-modules = storage.load-modules(wl)

  total-modules = wl.length()
  cached-modules = starter-modules.count-now()
  var num-compiled = cached-modules
  shadow options = options.{
    compile-module: true,
    on-compile: lam(locator, loadable):
      num-compiled := num-compiled + 1
      print("\r" + num-to-string(num-compiled) + "/" + num-to-string(total-modules) + " modules compiled")
      when num-compiled == total-modules:
        print("\nCleaning up and generating standalone...\n")
      end
      set-loadable(options.compiled-cache, locator, loadable)
    end
  }

  CL.compile-standalone(wl, starter-modules, options)
end

fun build-runnable-standalone(path, require-config-path, outfile, options):
  program = build-program(path, options)
  config = JSON.read-json(F.input-file(require-config-path).read-file()).dict.unfreeze()
  config.set-now("out", JSON.j-str(outfile))
  when not(config.has-key-now("baseUrl")):
    config.set-now("baseUrl", JSON.j-str(options.compiled-cache))
  end

  MS.make-standalone(program.natives, program.js-ast.to-ugly-source(), JSON.j-obj(config.freeze()).serialize())
end

fun build-require-standalone(path, options):
  program = build-program(path, options)

  natives = j-list(true, for C.map_list(n from program.natives): n end)

  define-name = j-id(A.s-name(A.dummy-loc, "define"))

  prog = j-block([clist:
      j-app(define-name, [clist: natives, j-fun([clist:],
        j-block([clist:
          j-return(program.js-ast)
        ]))
      ])
    ])

  print(prog.to-ugly-source())
end

