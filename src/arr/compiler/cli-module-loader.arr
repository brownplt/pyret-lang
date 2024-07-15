provide *
import srcloc as SL
import render-error-display as RED
import runtime-lib as R
import builtin-modules as B
import make-standalone as MS
import load-lib as L
import either as E
import json as JSON
import file("ast.arr") as A
import js-file("ts-pathlib") as P
import sha as crypto
import string-dict as SD
import error as ERR
import system as SYS
import js-file("./parse-pyret") as PP
import file("ast-util.arr") as AU
import js-file("ts-well-formed-impl") as W
import file("cmdline.arr") as CMD
import file("compile-lib.arr") as CL
import file("compile-structs.arr") as CS
import file("file.arr") as F
import file("locators/file.arr") as FL
import file("locators/builtin.arr") as BL
import file("locators/jsfile.arr") as JSF
import js-file("ts-js-of-pyret") as JSP
import js-file("dependency-tree") as DT
import js-file("ts-filelib") as FS


data Session:
  | session(
      module-cache :: SD.MutableStringDict<CS.Provides>,
      ref globals :: CS.Globals
    )
end

fun make-session():
  session([SD.mutable-string-dict:], CS.standard-globals)
end

sessions = [SD.mutable-string-dict:]

var module-cache = [SD.mutable-string-dict:]

fun delete-session(shadow session :: String) -> Option<String>:
  if sessions.has-key-now(session) block:
    sessions.remove-now(session)
    none
  else:
    some("Session did not exist: " + session + ", sessions are " + sessions.keys-list-now().join-str(","))
  end
end

fun filter-session(shadow session :: String, pattern :: String) -> Option<String> block:
  cases(Option) sessions.get-now(session):
    | none => nothing
    | some(s) =>
      for each(module-name from s.module-cache.keys-list-now()):
        when not(string-contains(module-name, pattern)) block:
          s.module-cache.remove-now(module-name)
          s!{globals: remove-globals-from-uri(module-name, s!globals)}
        end
      end
  end
  none
end

fun remove-globals-from-uri(uri :: String, g :: CS.Globals) -> CS.Globals:
  fun remove-if-this-module(dict, key):
    if dict.get-value(key).uri-of-definition == uri:
      dict.remove(key)
    else:
      dict
    end
  end

  CS.globals(
    fold(remove-if-this-module, g.modules, g.modules.keys-list()),
    fold(remove-if-this-module, g.values, g.values.keys-list()),
    fold(remove-if-this-module, g.types, g.types.keys-list())
  )
end

fun remove-globals-from-module(locator :: CL.Locator, g :: CS.Globals) -> CS.Globals:
  uri = locator.uri()
  remove-globals-from-uri(uri, g)
end

fun prepare-session(base :: CL.Located, options) block:
  when not(sessions.has-key-now(options.session)):
    sessions.set-now(options.session, make-session())
  end
  current-session = sessions.get-value-now(options.session)
  current-session.module-cache.remove-now(base.locator.uri())
  current-session!{ globals: remove-globals-from-module(base.locator, current-session!globals) }
end

fun make-provide-for-repl(p :: A.Program):
  cases(A.Program) p:
    | s-program(l, _, _, _, _, imports, body) =>
      A.s-program(l,
          none,
          A.s-provide-none(l),
          A.s-provide-types-none(l),
          [list: A.s-provide-block(l, empty, [list:
            A.s-provide-name(l, A.s-star(l, empty)),
            A.s-provide-type(l, A.s-star(l, empty)),
            A.s-provide-module(l, A.s-star(l, empty))
            # Adding s-provide-data for imports would be redundant because the
            # name/type exports will refer to the data anyway
            ])],
          imports,
          body)
  end
end

fun get-base-locator(options, base):
  if options.session <> "empty":
    base.locator.{
      method get-globals(self) block:
        g = sessions.get-value-now(options.session)!globals
        g
      end,
      method get-module(self):
        ast = cases(CL.PyretCode) base.locator.get-module():
          | pyret-ast(ast) => ast
          | pyret-string(ast) => PP.surface-parse(ast, base.locator.uri)
        end
        CL.pyret-ast(make-provide-for-repl(ast))
      end
    }
  else:
    base.locator
  end
end


fun get-starter-modules(options):
  if options.recompile-builtins and (options.session == "empty") block:
    [SD.mutable-string-dict:]
  else if not(options.recompile-builtins) and (options.session == "empty"):
    for each(sm from module-cache.keys-list-now()):
      when string-index-of(sm, "builtin://") <> 0:
        module-cache.remove-now(sm)
      end
    end
    module-cache
  else:
    sessions.get-value-now(options.session).module-cache
  end
end

fun add-globals-from-env(post-env :: CS.ComputedEnvironment, g :: CS.Globals) -> CS.Globals:
  module-env = post-env.module-env
  val-env = post-env.env
  type-env = post-env.type-env

  module-globals = for fold(mg from g.modules, k from module-env.keys-list()):
    mg.set(k, module-env.get-value(k).origin)
  end
  val-globals = for fold(vg from g.values, k from val-env.keys-list()):
    vg.set(k, val-env.get-value(k).origin)
  end
  type-globals = for fold(tg from g.types, k from type-env.keys-list()):
    tg.set(k, type-env.get-value(k).origin)
  end

  CS.globals(module-globals, val-globals, type-globals)
end

fun save-session(options, base-locator, locator, loadable):
  when (options.session <> "empty")
      and (base-locator.uri() == locator.uri())
      and CS.is-computed-env(loadable.post-compile-env):
    current-session = sessions.get-value-now(options.session)
    current-globals = current-session!globals
    new-globals = add-globals-from-env(loadable.post-compile-env, current-globals)
    current-session!{ globals: new-globals }
  end
end

type Loadable = CS.Loadable


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

  if (F.file-exists(saved-path + ".arr.js") and
      (F.mtimes(saved-path + ".arr.js").mtime > modified-time)):
    some(split)
  else if (F.file-exists(saved-path + ".js") and
      (F.mtimes(saved-path + ".js").mtime > modified-time)):
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
    method get-uncached(_): none end,
    method needs-compile(_, _): false end,
    method get-modified-time(self):
      0
      # F.mtimes(static-path + ".js").mtime
    end,
    method get-options(self, options):
      options.{ checks: "none" }
    end,
    method get-module(_):
      raise("Should never fetch source for builtin module " + static-path)
    end,
    method get-extra-imports(self):
      CS.minimal-imports
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
    method get-compiled(self, options):
      provs = CS.provides-from-raw-provides(self.uri(), {
          uri: self.uri(),
          values: raw-array-to-list(raw.get-raw-value-provides()),
          aliases: raw-array-to-list(raw.get-raw-alias-provides()),
          datatypes: raw-array-to-list(raw.get-raw-datatype-provides()),
          modules: raw-array-to-list(raw.get-raw-module-provides())
        })
      CL.already-done(CL.module-as-string(provs, CS.no-builtins, CS.computed-none,
          CS.ok(JSP.ccp-file(F.real-path(module-path + ".js")))))
    end,

    method _equals(self, other, req-eq):
      req-eq(self.uri(), other.uri())
    end
  }
end

fun get-cached-if-available(basedir, loc) block:
  get-cached-if-available-known-mtimes(basedir, loc, [SD.string-dict:])
end
fun get-cached-if-available-known-mtimes(basedir, loc, max-dep-times) block:
  saved-path = P.join(basedir, uri-to-path(loc.uri(), loc.name()))
  dependency-based-mtime =
    if max-dep-times.has-key(loc.uri()): max-dep-times.get-value(loc.uri())
    else: loc.get-modified-time()
    end
  cached-type = cached-available(basedir, loc.uri(), loc.name(), dependency-based-mtime)
  cases(Option) cached-type:
    | none =>
      cases(Option) loc.get-uncached():
        | some(shadow loc) => loc
        | none => loc
      end

    | some(ct)=> get-cached(basedir, loc.uri(), loc.name(), ct).{
        method get-uncached(self): some(loc) end
      } 
  end
end

fun get-file-locator(basedir, real-path):
  loc = FL.file-locator(real-path, CS.standard-globals)
  get-cached-if-available(basedir, loc)
end

fun get-builtin-locator(basedir, read-only-basedirs, modname, options):
  all-dirs = link(basedir, read-only-basedirs)

  first-available = for find(rob from all-dirs):
    is-some(cached-available(rob, "builtin://" + modname, modname, 0))
  end
  cases(Option) first-available:
    | none =>
      cases(Option) BL.maybe-make-builtin-locator(modname, options) block:
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

fun get-builtin-test-locator(basedir, modname, options):
  loc = BL.make-builtin-locator(modname, options).{
    method uri(_): "builtin-test://" + modname end
  }
  get-cached-if-available(basedir, loc)
end

fun get-loadable(basedir, read-only-basedirs, l, max-dep-times) -> Option<Loadable>:
  locuri = l.locator.uri()
#  cached = cached-available(basedir, l.locator.uri(), l.locator.name(), l.locator.get-modified-time())
  first-available = for find(rob from link(basedir, read-only-basedirs)):
    is-some(cached-available(rob, l.locator.uri(), l.locator.name(), max-dep-times.get-value(locuri)))
  end
  cases(Option) first-available block:
    | none => none
    | some(found-basedir) =>
      c = cached-available(found-basedir, l.locator.uri(), l.locator.name(), max-dep-times.get-value(locuri))
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
        modules: raw-array-to-list(raw-static.get-raw-module-provides()),
        values: raw-array-to-list(raw-static.get-raw-value-provides()),
        aliases: raw-array-to-list(raw-static.get-raw-alias-provides()),
        datatypes: raw-array-to-list(raw-static.get-raw-datatype-provides())
      })
      some(CS.module-as-string(provs, CS.no-builtins, CS.computed-none, CS.ok(JSP.ccp-file(module-path))))
  end
end

fun maybe-mkdir(path):
  when not(FS.exists(path)): FS.create-dir(path) end
end

fun mkdirp(path) block:
  components = string-split-all(path, P.path-sep)
  for fold(p from "/", c from components) block:
    new-p = P.join(p, c)
    maybe-mkdir(new-p)
    new-p
  end
end

fun setup-compiled-dirs( options ) block:
  base-dir = P.resolve( options.base-dir )
  compiled-dir = P.resolve( options.compiled-cache )
  project-dir  = P.join( compiled-dir, "project" )
  builtin-dir  = P.join( compiled-dir, "builtin" )

	mkdirp( compiled-dir )
	mkdirp( project-dir )
	mkdirp( builtin-dir )

  # TODO(joe, anchor, May '19): sort this out for builtins needed by generated code
  #when not(FS.exists(P.join(compiled-dir, "node_modules"))):
  #  FS.symlink(P.join(options.this-pyret-dir, "../../node_modules"), P.join(compiled-dir, "node_modules"), "dir")
  #end

  {base-dir; project-dir; builtin-dir}
end

fun time-or-0(p):
  if F.file-exists(p): F.file-times(p).mtime
  else: 0
  end
end


type CLIContext = {
  current-load-path :: String,
  cache-base-dir :: String
}


default-start-context = {
  current-load-path: P.resolve("./"),
  cache-base-dir: P.resolve("./compiled"),
  compiled-read-only-dirs: empty,
  options: CS.default-compile-options
}

default-test-context = {
  current-load-path: P.resolve("./"),
  cache-base-dir: P.resolve("./tests/compiled"),
  compiled-read-only-dirs: empty,
  options: CS.default-compile-options
}

fun set-loadable(options, locator, loadable, max-dep-times) block:
  doc: "Returns the module path of the cached file"
  { project-base; project-dir; builtin-dir } = setup-compiled-dirs( options )

  locuri = loadable.provides.from-uri

  cases(CS.CompileResult) loadable.result-printer block:
    | ok(ccp) =>

      uri = locator.uri()

      {save-path; static-ext; code-ext; dep-path} = ask block:
        | string-index-of(uri, "builtin://") == 0 then:
          {P.join(builtin-dir, locator.name()); ".arr.json"; ".arr.js"; builtin-dir}
        | (string-index-of(uri, "jsfile://") == 0) or (string-index-of(uri, "file://") == 0) then:
          cutoff = if (string-index-of(uri, "jsfile://") == 0): 9 else: 7 end

          full-path = string-substring(uri, cutoff, string-length(uri))

          when string-index-of(full-path, project-base) <> 0:
            raise("An included file was at " + full-path + " which is outside the current project: " + project-base)
          end

          relative-to-project = string-substring(full-path, string-length(project-base), string-length(full-path))

          dep-path = P.dirname(P.join(project-dir, relative-to-project))

          {P.join(project-dir, relative-to-project); ".json"; ".js"; dep-path}
      end

      save-static-path = save-path + static-ext
      save-code-path = save-path + code-ext

      effective-time = max-dep-times.get-value(locator.uri())

      file-was-updated = (time-or-0(save-static-path) <= effective-time)  or (time-or-0(save-code-path) <= effective-time)

      when file-was-updated block:
        when not( FS.exists( dep-path ) ):
          mkdirp( dep-path )
        end

        skip-saving-builtin =
          FS.exists(save-static-path) and
          FS.exists(save-code-path) and
          not(options.recompile-builtins) and
          (string-substring(uri, 0, 10) == "builtin://")

        when not(skip-saving-builtin) block:
          fs = F.output-file(save-static-path, false)
          fr = F.output-file(save-code-path, false)

          fs.display(JSP.pyret-to-js-static(ccp))
          fr.display(JSP.pyret-to-js-runnable(ccp))
        end

      end

      {save-static-path; save-code-path}

    | err(_) =>
      {""; ""}
  end
end


fun get-real-path(current-load-path :: String, dep :: CS.Dependency):
  this-path = dep.arguments.get(0)
  if P.is-absolute(this-path):
    P.relative(current-load-path, this-path)
  else:
    P.join(current-load-path, this-path)
  end
end

fun module-finder(ctxt :: CLIContext, dep :: CS.Dependency):
  cases(CS.Dependency) dep:
    | dependency(protocol, args) =>
      if protocol == "file":
        clp = ctxt.current-load-path
        real-path = get-real-path(clp, dep)
        new-context = ctxt.{current-load-path: P.dirname(real-path)}
        if F.file-exists(real-path):
          CL.located(get-file-locator(ctxt.cache-base-dir, real-path), new-context)
        else:
          raise("Cannot find import " + torepr(dep) + ", looking at " + real-path)
        end
      else if protocol == "builtin-test":
        l = get-builtin-test-locator(ctxt.cache-base-dir, args.first, ctxt.options)
        force-check-mode = l.{
          method get-options(self, options):
            options.{ checks: "all", type-check: false }
          end
        }
        CL.located(force-check-mode, ctxt)
      else if protocol == "file-no-cache":
        clp = ctxt.current-load-path
        real-path = get-real-path(clp, dep)
        new-context = ctxt.{current-load-path: P.dirname(real-path)}
        if F.file-exists(real-path):
          CL.located(FL.file-locator(real-path, CS.standard-globals), new-context)
        else:
          raise("Cannot find import " + torepr(dep) + ", looking at " + real-path)
        end
      else if protocol == "js-file":
        clp = ctxt.current-load-path
        real-path = get-real-path(clp, dep)
        new-context = ctxt.{current-load-path: P.dirname(real-path)}
        locator = JSF.make-jsfile-locator(real-path)
        CL.located(locator, new-context)
      else:
        raise("Unknown import type: " + protocol)
      end
    | builtin(modname) =>
      CL.located(get-builtin-locator(ctxt.cache-base-dir, ctxt.compiled-read-only-dirs, modname, ctxt.options), ctxt)
  end
end


fun handle-compilation-errors(problems, options) block:
  for lists.each(e from problems) block:
    options.log-error(RED.display-to-string(e.render-reason(), torepr, empty))
    options.log-error("\n")
  end
  # TODO (Tiffany): remove handle-compilation-errors and run
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

fun copy-js-dependency( dep-path, uri, dirs, options ) block:
  { base-dir; project-dir; builtin-dir } = dirs
  {save-path; cutoff} = ask block:
    | string-index-of( uri, "builtin://" ) == 0 then:
      {
         builtin-dir;
         string-substring( dep-path, string-length(P.resolve(options.builtin-js-dirs.first)), string-length(dep-path))
      }
    | (string-index-of( uri, "jsfile://" ) == 0) or ( string-index-of( uri, "file://" ) == 0 ) then:
       { project-dir;
         string-substring( dep-path, string-length( base-dir ), string-length( dep-path ) )
       }
  end

  save-code-path = P.join( save-path, cutoff )
  mkdirp( P.resolve( P.dirname( save-code-path ) ) )

  when not(F.file-exists(save-code-path)) or (F.mtimes(save-code-path).mtime < F.mtimes(dep-path).mtime) block:
    fc = F.output-file( save-code-path, false )

    file-content = F.file-to-string( dep-path )
    fc.display( file-content )

    fc.flush()
    fc.close-file()
  end

  save-code-path
end

# NOTE(alex):
#   Under compile mode "builtin-stage-1", the global module is actually NOT imported
#     at runtime (see direct-codegen.arr)
#   However, from the compiler's PoV, the global module is still a dependency.
#   If global depends on a "builtin-stage-1" module BM, whle compiling BM, copy-js-dependencies
#     will attempt to file the compiled BM in the build/runtime directory.
#   To allow such a dependency, add a conditional based on the current compile mode somewhere
#     in copy-js-dependencies() such that it does NOT trace the global module dependencies.
#
fun copy-js-dependencies( wl, options ) block:
  arr-js-modules = for filter( tc from wl ):
    CL.is-arr-js-file( tc.locator.get-compiled( options ) )
  end

  code-paths = for map(tc from arr-js-modules): tc.locator.get-compiled( options ).code-file end
  uris = for map(tc from arr-js-modules): tc.locator.uri() end

  copy-js-dependencies-code-path( code-paths, uris, options )
end

fun copy-js-dependencies-code-path( code-paths, uris, options ) block:
  dirs = setup-compiled-dirs( options )

  paths = SD.make-mutable-string-dict()

  for each2( code-path from code-paths, uri from uris ):
    deps = DT.get-dependencies( P.resolve(code-path) )
    deps-list = raw-array-to-list( deps )

    for each( dep-path from deps-list ):
      when P.resolve( code-path ) <> dep-path:
        paths.set-now( dep-path, uri )
      end
    end
  end

  for each( dep-path from paths.keys-list-now() ):
    copy-js-dependency( dep-path, paths.get-value-now( dep-path ), dirs, options )
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
  var str = "Gathering dependencies..."
  fun print-progress(s) block:
    print-progress-clearing(s + "\n", none) 
    str := ""
  end
  fun clear-and-print(new-str) block:
    print-progress-clearing(new-str, some(string-length(str)))
    str := new-str
  end
  print-progress(str)
  base-module = CS.dependency("file", [list: path])
  base = module-finder({
    current-load-path: P.resolve(options.base-dir),
    cache-base-dir: options.compiled-cache,
    compiled-read-only-dirs: options.compiled-read-only.map(P.resolve),
    options: options
  }, base-module)
  clear-and-print("Compiling worklist...")

  prepare-session(base, options)

  starter-modules = get-starter-modules(options)

  base-locator = get-base-locator(options, base)
  

  wl = CL.compile-worklist-known-modules(module-finder, base-locator, base.context, starter-modules)
  compiler-edited-time = if FS.exists(CMD.file-name): F.file-times(CMD.file-name).mtime else: 0 end
  max-dep-times = CL.dep-times-from-worklist(wl, compiler-edited-time)
  shadow wl = for map(located from wl):
    located.{ locator: get-cached-if-available-known-mtimes(options.compiled-cache, located.locator, max-dep-times) }
  end
  length-before-wl = starter-modules.count-now()
  copy-js-dependencies( wl, options )

  clear-and-print("Loading existing compiled modules...")

  CL.modules-from-worklist-known-modules(wl, starter-modules, max-dep-times, get-loadable(options.compiled-cache, options.compiled-read-only.map(P.resolve), _, _))

  cached-modules = starter-modules.count-now() - length-before-wl
  total-modules = wl.length() - cached-modules
  var num-compiled = 0
  when total-modules == 0:
    clear-and-print("All modules already compiled. Cleaning up and generating standalone...\n")
  end
  shadow options = options.{
    method should-profile(_, locator):
      options.add-profiling and (locator.uri() == base-locator.uri())
    end,
    method before-compile(_, locator) block:
      num-compiled := num-compiled + 1
      clear-and-print("Compiling " + num-to-string(num-compiled) + "/" + num-to-string(total-modules)
          + ": " + locator.name())
    end,
    method on-compile(self, locator, loadable, trace) block:
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

      save-session(options, base-locator, locator, loadable)
      {static-path; code-path} = set-loadable(self, locator, loadable, max-dep-times)
      if (num-compiled == total-modules) and options.collect-all:
        # Don't squash the final JS-AST if we're collecting all of them, so
        # it can be pretty-printed after all
        loadable
      else:
        cases(CL.Loadable) loadable:
          | module-as-string(prov, env, post-env, rp) =>
            CL.module-as-string(prov, env, post-env, CS.ok(JSP.ccp-two-files(static-path, code-path)))
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
  build-program(path, options, stats)
end

fun lint(program :: String, uri :: String, options):
  cases(E.Either) PP.maybe-surface-parse(uri, program):
    | left(exn) => E.left([list: exn.exn])
    | right(ast) =>
      ast-ended = AU.wrap-toplevels(AU.append-nothing-if-necessary(ast))
      cases(CS.CompileResult) W.check-well-formed(ast-ended, options):
        | ok(_) => E.right(ast)
        | err(errs) => E.left(errs)
      end
  end
end
