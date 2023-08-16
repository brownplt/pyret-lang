provide *
provide-types *

import either as E
import js-file("parse-pyret") as P
import file("ast.arr") as A
import load-lib as L
import runtime-lib as R
import string-dict as SD
import file("compile-structs.arr") as CS
import js-file("ts-gensym") as G
import js-file("ts-js-of-pyret") as JSP
import file("ast-util.arr") as AU
import js-file("ts-well-formed-impl") as W
import js-file("ts-desugar-impl") as D
import js-file("ts-desugar-post-tc") as DP
import file("type-check.arr") as T
import file("resolve-scope.arr") as RS
import js-file("ts-compiler-lib-impl") as TCL

data CompilationPhase:
  | start(time :: Number)
  | phase(name :: String, result :: Any, time :: Number, prev :: CompilationPhase)
sharing:
  method tolist(self):
    fun help(the-phase, acc):
      if is-start(the-phase): acc
      else:
        help(the-phase.prev,
          { name : the-phase.name,
            time : the-phase.time - the-phase.prev.time,
            result : the-phase.result} ^ link(_, acc))
      end
    end
    help(self, empty)
  end
end

left = E.left
right = E.right
type Either = E.Either

mtd = [SD.string-dict:]

type URI = String

data PyretCode:
  | pyret-string(s :: String)
  | pyret-ast(ast :: A.Program)
end

type ModuleResult = Any

type Loadable = CS.Loadable
module-as-string = CS.module-as-string

type Provides = CS.Provides

data CompileTODO:
  | already-done( result :: CS.Loadable )
  | arr-js-file( provides, header-file :: String, code-file :: String )
  | arr-file( mod :: PyretCode, libs :: CS.ExtraImports, options :: CS.CompileOptions )
end

type Locator = {

  # In milliseconds-since-epoch format
  get-modified-time :: (-> Number),

  get-options :: (CS.CompileOptions -> CS.CompileOptions),

  # Could either have needs-provide be implicitly stateful, and cache
  # the most recent map, or use explicit interface below
  needs-compile :: (SD.StringDict<Provides> -> Boolean),

  # Pre-compile (skippable if get-compile returns something)
  get-module :: ( -> PyretCode),

  # Pre-compile (had better be known with no other help)
  get-dependencies :: ( -> List<CS.Dependency>),

  # Pre-compile (to find list of requirejs dependencies to include)
  get-native-modules :: ( -> List<CS.NativeModule>),

  # Pre-compile
  get-extra-imports :: ( -> CS.ExtraImports),

  # Post- or pre- compile
  # get-import-names :: ( -> List<String>),

  # Pre-compile, specification of available globals
  get-globals :: ( -> CS.Globals),

  uri :: (-> URI),
  name :: (-> String),

  set-compiled :: (Loadable, SD.StringDict<Provides> -> Nothing),

  # Pre-compile if needs-compile is false
  get-compiled :: (Any -> CompileTODO ),

  # _equals should compare uris for locators
  _equals :: Method
}



fun string-locator(uri :: URI, s :: String):
  {
    method needs-compile(self, _): true end,
    method get-modified-time(self): 0 end,
    method get-options(self, options): options end,
    method get-module(self): pyret-string(s) end,
    method get-native-modules(self): [list:] end,
    method get-dependencies(self): get-standard-dependencies(pyret-string(s), uri) end,
    method get-extra-imports(self): CS.minimal-imports end,
    method get-globals(self): CS.standard-globals end,
    method uri(self): uri end,
    method name(self): uri end,
    method set-compiled(self, _, _): nothing end,
    method get-compiled(self, options): arr-file(self.get-module(), self.get-extra-imports(), self.get-options(options)) end,
    method _equals(self, other, rec-eq): rec-eq(other.uri(), self.uri()) end
  }
end

data Located<a>:
  | located(locator :: Locator, context :: a)
end

fun get-ast(p :: PyretCode, uri :: URI):
  cases(PyretCode) p:
    | pyret-string(s) => P.surface-parse(s, uri)
    | pyret-ast(a) => a
  end
end

fun get-import-type(i):
  cases(A.Import) i:
    | s-import(_, f, _) => some(f)
    | s-import-types(_, f, _, _) => some(f)
    | s-include(_, f) => some(f)
    | s-import-fields(_, _, f) => some(f)
    | s-include-from(_, _, _) => none
  end
end

fun get-dependencies(p :: PyretCode, uri :: URI) -> List<CS.Dependency>:
  TCL.get-dependencies(p, uri)
end

fun get-standard-dependencies(p :: PyretCode, uri :: URI) -> List<CS.Dependency>:
  TCL.get-standard-dependencies(p, uri)
end

type ToCompile = { locator :: Locator, dependency-map :: SD.MutableStringDict<Locator> }

dummy-provides = lam(uri): CS.provides(uri, SD.make-string-dict(), SD.make-string-dict(), SD.make-string-dict(), SD.make-string-dict()) end

compile-worklist = TCL.compile-worklist

compile-worklist-known-modules = TCL.compile-worklist-known-modules
  # temp-marked = SD.make-mutable-string-dict()
  # var topo = empty
  # fun visit(shadow locator :: Locator, shadow context :: a, curr-path :: List<Locator>) block:
  #   cases(Option) temp-marked.get-now(locator.uri()) block:
  #     | some(mark) =>
  #       when mark:
  #         raise("Detected module cycle: " + curr-path.reverse().map(_.uri()).join-str(" => "))
  #       end
  #     | none =>
  #       # mark current locator temporarily
  #       temp-marked.set-now(locator.uri(), true)
  #       pmap = SD.make-mutable-string-dict()
  #       deps = locator.get-dependencies()
  #       found-mods = for lists.filter-map(d from deps) block:
  #         cases(CS.Dependency) d block:
  #           | dependency(_, _) =>
  #             found = dfind(context, d)
  #             pmap.set-now(d.key(), found.locator.uri())
  #             some(found)
  #           | builtin(name) =>
  #             cases (Option) current-modules.get-now("builtin://" + name) block:
  #               | none =>
  #                 found = dfind(context, d)
  #                 pmap.set-now(d.key(), found.locator.uri())
  #                 some(found)
  #               | some(builtin-mod) =>
  #                 pmap.set-now(d.key(), builtin-mod.provides.from-uri)
  #                 none
  #             end
  #         end
  #       end
  #       # visit all dependents
  #       for each(f from found-mods):
  #         when not(current-modules.has-key-now(f.locator.uri())):
  #           visit(f.locator, f.context, link(f.locator, curr-path))
  #         end
  #       end
  #       # add current locator to head of topo sort
  #       topo := {locator: locator, dependency-map: pmap} ^ link(_, topo)
  #       # mark current locator permanently
  #       temp-marked.set-now(locator.uri(), false)
  #   end
  #   topo
  # end
  # # our include edges are backwards to how the topological sort algorithm expects dependencies,
  # # so reverse the result
  # ans = visit(locator, context, [list: locator]).reverse()
  # ans

modules-from-worklist-known-modules = TCL.modules-from-worklist-known-modules

# NOTE(joe): base-time is usually the time the *compiler* was last edited.
# Other clients might have another “min” time after which they want to
# make sure all modules are recompiled.
dep-times-from-worklist = TCL.dep-times-from-worklist

type CompiledProgram = {loadables :: List<Loadable>, modules :: SD.MutableStringDict<Loadable>}

fun compile-program-with(worklist :: List<ToCompile>, modules, options) -> CompiledProgram:
  if CS.is-pipeline-ts-anchor(options.pipeline) and options.pipeline.modules.member("compile-lib"):
    TCL.compile-program-with(worklist, modules, options)
  else:
    internal-compile-program-with(worklist, modules, options)
  end
end

fun internal-compile-program-with(worklist :: List<ToCompile>, modules, options) -> CompiledProgram block:
  cache = modules
  loadables = for map(w from worklist):
    uri = w.locator.uri()
    if not(cache.has-key-now(uri)) block:
      provide-map = w.dependency-map.freeze()
      options.before-compile(w.locator)
      {loadable :: Loadable; trace :: List} = compile-module(w.locator, provide-map, cache, options)
      # I feel like here we want to generate two copies of the loadable:
      # - One local for calling on-compile with and serializing
      # - One canonicalized for the local cache
      cache.set-now(uri, loadable)
      local-loadable = cases(Loadable) loadable:
        | module-as-string(provides, env, post-env, result) =>
          module-as-string(AU.localize-provides(provides, env), env, post-env, result)
      end
      # allow on-compile to return a new loadable
      options.on-compile(w.locator, local-loadable, trace)
    else:
      cache.get-value-now(uri)
    end
  end
  { loadables: loadables, modules: cache }
end

rec compile-program = TCL.compile-program

fun unique(lst):
  sets.list-to-list-set(lst).to-list()
end

fun compile-module(locator :: Locator, provide-map :: SD.StringDict<URI>, modules, options) -> {Loadable; List} block:
  doc: ```
    Invariant: provide-map maps dependency keys to URIs
    which ALL must be keys in modules.
  ```
  G.reset()
  A.global-names.reset()
  #print("Compiling module: " + locator.uri() + "\n")
  env = CS.compile-env(locator.get-globals(), modules, provide-map)
  cases(CompileTODO) locator.get-compiled(options) block:
    | already-done(loadable) =>
      #print("Module is already compiled\n")
      cases(Loadable) loadable:
        | module-as-string(pvds, ce-unused, post-env, m) =>
          {module-as-string(AU.canonicalize-provides(pvds, env), ce-unused, post-env, m); empty}
      end
    | arr-js-file(provides, header-file, code-file) =>
      {module-as-string(provides, CS.no-builtins, CS.computed-none, CS.ok(JSP.ccp-two-files(header-file, code-file))); empty}
    | arr-file(mod, libs, shadow options)  =>
      #print("Module is being freshly compiled\n")
      var ast = cases(PyretCode) mod:
        | pyret-string(module-string) =>
          P.surface-parse(module-string, locator.uri())
        | pyret-ast(module-ast) =>
          module-ast
      end
      var ret = start(time-now())
      fun add-phase(name, value) block:
        if options.collect-all:
          ret := phase(name, value, time-now(), ret)
        else if options.collect-times:
          ret := phase(name, nothing, time-now(), ret)
        else:
          nothing
        end
        value
      end
      var ast-ended = AU.append-nothing-if-necessary(ast)
      ast := nothing
      add-phase("Added nothing", ast-ended)
      var wf = W.check-well-formed(ast-ended, options)
      ast-ended := nothing
      add-phase("Checked well-formedness", wf)
      cases(CS.CompileResult) wf block:
        | ok(_) =>
          var wf-ast = AU.wrap-toplevels(wf.code)
          wf := nothing
          var checked = wf-ast
          # NOTE(joe, anchor): no desugaring of check blocks
          # checker(wf-ast, options)
          wf-ast := nothing
          add-phase(if not(options.checks == "none"): "Desugared (with checks)" else: "Desugared (skipping checks)" end, checked)
          var imported = AU.wrap-extra-imports(checked, libs)
          checked := nothing
          add-phase("Added imports", imported)
          var scoped = RS.desugar-scope(imported, env, options)
          imported := nothing
          add-phase("Desugared scope", scoped)
          var named-result = RS.resolve-names(scoped.ast, locator.uri(), env)
          var any-errors = scoped.errors + named-result.errors
          scoped := nothing
          if is-link(any-errors) block:
            { module-as-string(dummy-provides(locator.uri()), env, CS.computed-none, CS.err(unique(any-errors)));
              if options.collect-all or options.collect-times:
                phase("Result", named-result.ast, time-now(), ret).tolist()
              else:
                empty
              end }
          else:
            add-phase("Resolved names", named-result)
            var spied =
              if options.enable-spies: named-result.ast
              else: named-result.ast.visit(A.default-map-visitor.{
                    method s-block(self, l, stmts):
                      A.s-block(l, stmts.foldr(lam(stmt, acc):
                            if A.is-s-spy-block(stmt): acc
                            else: link(stmt.visit(self), acc)
                            end
                          end, empty))
                    end
                  })
              end
            var provides = dummy-provides(locator.uri())
            # Once name resolution has happened, any newly-created s-binds must be added to bindings...
            # var desugared = named-result.ast

            # NOTE(joe, anchor): removed this to see what un-desugared output looks like
            # and changed desugared.ast to desugared below

            var desugared = D.desugar(named-result.ast, options)

            named-result.env.bindings.merge-now(desugared.new-binds)

            # ...in order to be checked for bad assignments here
            any-errors := RS.check-unbound-ids-bad-assignments(desugared.ast, named-result, env)
            add-phase("Fully desugared", desugared)
            var type-checked =
              if is-link(any-errors):
                CS.err(unique(any-errors))
              else if options.type-check:
                type-checked = T.type-check(desugared.ast, env, named-result.env, modules, options)
                if CS.is-ok(type-checked) block:
                  provides := AU.get-typed-provides(named-result, type-checked.code, locator.uri(), env)
                  CS.ok(desugared.ast)
                else:
                  type-checked
                end
              else: CS.ok(desugared.ast)
              end
            desugared := nothing
            add-phase("Type Checked", type-checked)
            shadow options = options.{should-profile: options.should-profile(locator)}
            cases(CS.CompileResult) type-checked block:
              | ok(_) =>
                var tc-ast = type-checked.code
                var dp-ast = DP.desugar-post-tc(tc-ast, env, options)
                tc-ast := nothing
                var cleaned = dp-ast
                dp-ast := nothing
                cleaned := cleaned ^ AU.set-safe-letrec-binds
                            ^ AU.inline-lams
                            ^ AU.set-recursive
                            ^ AU.set-tail-position
                when not(options.user-annotations):
                  cleaned := cleaned ^ AU.strip-annotations
                end
                add-phase("Cleaned AST", cleaned)
                when not(options.type-check) block:
                  provides := AU.get-named-provides(named-result, locator.uri(), env)
                end
                {final-provides; cr} = JSP.make-compiled-pyret(cleaned, locator.uri(), env, named-result.env, provides, options)
                cleaned := nothing
                canonical-provides = AU.canonicalize-provides(final-provides, env)
                #|
                spy "compile-lib:canonicalize-provides":
                  final-provides,
                  canonical-provides
                end
                |#
                mod-result = module-as-string(canonical-provides, env, named-result.env, cr)
                {mod-result; if options.collect-all or options.collect-times: ret.tolist() else: empty end}
              | err(_) =>
                { module-as-string(provides, env, CS.computed-none, type-checked);
                  if options.collect-all or options.collect-times:
                    phase("Result", type-checked, time-now(), ret).tolist()
                  else: empty
                  end }
            end
          end
        | err(_) =>
          { module-as-string(dummy-provides(locator.uri()), env, CS.computed-none, wf) ;
            if options.collect-all or options.collect-times:
              phase("Result", wf, time-now(), ret).tolist()
            else: empty
            end }
      end
  end
end

type PyretAnswer = Any
type PyretMod = Any

fun is-error-compilation(cr):
  CS.is-module-as-string(cr) and CS.is-err(cr.result-printer)
end

fun run-program(ws :: List<ToCompile>, prog :: CompiledProgram, realm :: L.Realm, runtime :: R.Runtime, options):
  compiled-mods = prog.loadables
  errors = compiled-mods.filter(is-error-compilation)
  cases(List) errors block:
    | empty =>
      #print("Make standalone program\n")
      program = make-standalone(ws, prog, options)
      #print("Run program\n")
      ans = right(L.run-program(runtime, realm, program.v.js-ast.to-ugly-source(), options, empty))
      #print("Done\n")
      ans
    | link(_, _) =>
      left(errors.map(_.result-printer))
  end
end

fun compile-and-run-locator(locator, finder, context, realm, runtime, starter-modules, options) block:
  #print("Make worklist\n")
  wl = compile-worklist(finder, locator, context)
  #print("Compile program\n")

  compiled = compile-program-with(wl, starter-modules, options)
  compiled-mods = compiled.loadables
  errors = compiled-mods.filter(is-error-compilation)
  cases(List) errors block:
    | empty =>
      #print("Make standalone program\n")

      program = make-standalone(wl, compiled, options)
      #print("Run program\n")

      # NOTE(joe): program.v OK because no errors above
      ans = right(L.run-program(runtime, realm, program.v.js-ast.to-ugly-source(), options, empty))
      #print("Done\n")
      ans
    | link(_, _) =>
      left(errors.map(_.result-printer))
  end
end

fun compile-standalone(wl, starter-modules, options):
  compiled = compile-program-with(wl, starter-modules, options)
  ans = make-standalone(wl, compiled, options)
  ans
end

# NOTE(joe): I strongly suspect options will be used in the future
fun make-standalone(wl, compiled, options):
  TCL.make-standalone(wl, compiled, options)
end
