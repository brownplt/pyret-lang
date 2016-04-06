provide *
import namespace-lib as N
import runtime-lib as R
import builtin-modules as B
import load-lib as L
import either as E
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

fun set-loadable(basedir, loadable):
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
  base-module = CS.dependency("file", [list: path])
  base = module-finder({current-load-path: P.resolve("./")}, base-module)
  wl = CL.compile-worklist(module-finder, base.locator, base.context)
  r = R.make-runtime()
  result = CL.compile-and-run-worklist(wl, r, options)
  cases(Either) result:
    | right(answer) =>
      if L.is-success-result(answer):
        print(L.render-check-results(answer))
      else:
        print(L.render-error-message(answer))
        raise("There were execution errors")
      end
      result
    | left(errors) =>
      print-error("Compilation errors:")
      for lists.each(e from errors):
        print-error(RED.display-to-string(e.render-reason(), torepr, empty))
      end
      raise("There were compilation errors")
  end
end

fun build-standalone(path, options):
  shadow options = options.{ compile-module: true }

  base-module = CS.dependency("file", [list: path])
  base = module-finder({current-load-path: P.resolve("./")}, base-module)
  wl = CL.compile-worklist(module-finder, base.locator, base.context)

  storage = get-cli-module-storage(options.compiled-cache)
  starter-modules = storage.load-modules(wl)
  compiled = CL.compile-program-with(wl, starter-modules, options)
  storage.save-modules(compiled.loadables)

  define-name = j-id(A.s-name(A.dummy-loc, "define"))

  static-modules = j-obj(for C.map_list(w from wl):
    loadable = compiled.modules.get-value-now(w.locator.uri())
    cases(CL.Loadable) loadable:
      | module-as-string(_, _, rp) =>
        cases(CS.CompileResult) rp:
          | ok(code) =>
            j-field(w.locator.uri(), J.j-raw-code(code.pyret-to-js-runnable()))
          | err(problems) =>
            for lists.each(e from problems):
              print-error(RED.display-to-string(e.render-reason(), torepr, empty))
            end
            raise("There were compilation errors")
        end
      | pre-loaded(provides, _, _) =>
        raise("Cannot serialize pre-loaded: " + torepr(provides.from-uri))
    end
  end)

  depmap = j-obj(for C.map_list(w from wl):
    deps = w.dependency-map
    j-field(w.locator.uri(),
      j-obj(for C.map_list(k from deps.keys-now().to-list()):
        j-field(k, j-str(deps.get-value-now(k).uri()))
      end))
  end)

  natives = j-list(true, for fold(natives from [clist:], w from wl):
    C.map_list(lam(r): j-str(r.path) end, w.locator.get-native-modules()) + natives
  end)

  to-load = j-list(false, for C.map_list(w from wl):
    j-str(w.locator.uri())
  end)

  prog = j-block([clist:
      j-app(define-name, [clist: natives, j-fun([clist:],
        j-block([clist:
          j-return(j-obj([clist:
            j-field("staticModules", static-modules),
            j-field("depMap", depmap),
            j-field("toLoad", to-load)
          ]))
        ]))
      ])
    ])

  print(prog.tosource().pretty(80).join-str("\n"))
end

