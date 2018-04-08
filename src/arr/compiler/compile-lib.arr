provide *
provide-types *

import either as E
import parse-pyret as P
import ast as A
import load-lib as L
import render-error-display as RED
import runtime-lib as R
import sets as S
import sha as SHA
import srcloc as Loc
import string-dict as SD
import ast-visitors as AV
import file("compile-structs.arr") as CS
import file("concat-lists.arr") as C
import file("gensym.arr") as G
import file("js-of-pyret.arr") as JSP
import file("js-ast.arr") as J
import file("ast-util.arr") as AU
import file("well-formed.arr") as W
import file("desugar.arr") as D
import file("desugar-post-tc.arr") as DP
import file("type-check.arr") as T
import file("desugar-check.arr") as CH
import file("resolve-scope.arr") as RS
import file("../desugar/stepify.arr") as ST
import file("../desugar/ds-main.arr") as DNew

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


left = E.left
right = E.right
type Either = E.Either

mtd = [SD.string-dict:]

# for re-export
standard-builtins = CS.standard-builtins

type URI = String

data PyretCode:
  | pyret-string(s :: String)
  | pyret-ast(ast :: A.Program)
end

data Loadable:
  | module-as-string(provides :: CS.Provides, compile-env :: CS.CompileEnvironment, result-printer :: CS.CompileResult<JSP.CompiledCodePrinter>)
end

type ModuleResult = Any

type Provides = CS.Provides

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
  get-compiled :: ( -> Option<CS.CompileResult>),

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
    method get-extra-imports(self): CS.standard-imports end,
    method get-globals(self): CS.standard-globals end,
    method uri(self): uri end,
    method name(self): uri end,
    method set-compiled(self, _, _): nothing end,
    method get-compiled(self): none end,
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
    | s-import(_, f, _) => f
    | s-include(_, f) => f
    | s-import-complete(_, _, _, f, _, _) => f
    | s-import-fields(_, _, f) => f
  end
end

fun get-dependencies(p :: PyretCode, uri :: URI) -> List<CS.Dependency>:
  parsed = get-ast(p, uri)
  for map(s from parsed.imports.map(get-import-type)):
    AU.import-to-dep(s)
  end
end

fun get-standard-dependencies(p :: PyretCode, uri :: URI) -> List<CS.Dependency>:
  mod-deps = get-dependencies(p, uri)
  mod-deps + CS.standard-imports.imports.map(_.dependency)
end

fun const-dict<a>(strs :: List<String>, val :: a) -> SD.StringDict<a>:
  for fold(d from mtd, s from strs):
    d.set(s, val)
  end
end

fun get-provides(p :: PyretCode, uri :: URI) -> Provides:
  parsed = get-ast(p, uri)
  vals-part =
    cases (A.Provide) parsed._provide:
      | s-provide-none(l) => mtd
      | s-provide-all(l) =>
        const-dict(A.toplevel-ids(parsed).map(_.toname()), CS.v-just-there)
      | s-provide(l, e) =>
        cases (A.Expr) e:
          | s-obj(_, mlist) => const-dict(mlist.map(_.name), CS.v-just-there)
          | else => raise("Non-object expression in provide: " + l.format(true))
        end
    end
  types-part =
    cases(A.ProvideTypes) parsed.provided-types:
      | s-provide-types-none(l) => mtd
      | s-provide-types-all(l) =>
        type-ids = A.block-type-ids(parsed.block)
        type-strs = type-ids.map(lam(i): i.name.toname() end)
        const-dict(type-strs, CS.t-just-there)
      | s-provide-types(l, anns) =>
        const-dict(anns.map(_.name), CS.t-just-there)
    end
  CS.provides(vals-part, types-part)
end

type ToCompile = { locator :: Locator, dependency-map :: SD.MutableStringDict<Locator> }

fun dict-map<a, b>(sd :: SD.MutableStringDict, f :: (String, a -> b)):
  for fold(sd2 from mtd, k from sd.keys-now().to-list()):
    sd2.set(k, f(k, sd.get-value-now(k)))
  end
end

dummy-provides = lam(uri): CS.provides(uri, SD.make-string-dict(), SD.make-string-dict(), SD.make-string-dict()) end

fun compile-worklist<a>(dfind, locator, context):
  compile-worklist-known-modules(dfind, locator, context, SD.make-mutable-string-dict())
end

fun compile-worklist-known-modules<a>(dfind :: (a, CS.Dependency -> Located<a>), locator :: Locator, context :: a, current-modules :: SD.MutableStringDict<Provides>) -> List<ToCompile> block:
  temp-marked = SD.make-mutable-string-dict()
  var topo = empty
  fun visit(shadow locator :: Locator, shadow context :: a, curr-path :: List<Locator>) block:
    cases(Option) temp-marked.get-now(locator.uri()) block:
      | some(mark) =>
        when mark:
          raise("Detected module cycle: " + curr-path.reverse().map(_.uri()).join-str(" => "))
        end
      | none =>
        # mark current locator temporarily
        temp-marked.set-now(locator.uri(), true)
        pmap = SD.make-mutable-string-dict()
        deps = locator.get-dependencies()
        found-mods = for map(d from deps) block:
          found = dfind(context, d)
          pmap.set-now(d.key(), found.locator)
          found
        end
        # visit all dependents
        for each(f from found-mods):
          when not(current-modules.has-key-now(f.locator.uri())):
            visit(f.locator, f.context, link(f.locator, curr-path))
          end
        end
        # add current locator to head of topo sort
        topo := {locator: locator, dependency-map: pmap} ^ link(_, topo)
        # mark current locator permanently
        temp-marked.set-now(locator.uri(), false)
    end
    topo
  end
  # our include edges are backwards to how the topological sort algorithm expects dependencies,
  # so reverse the result
  ans = visit(locator, context, [list: locator]).reverse()
  ans
end

type CompiledProgram = {loadables :: List<Loadable>, modules :: SD.MutableStringDict<Loadable>}

fun compile-program-with(worklist :: List<ToCompile>, modules, options) -> CompiledProgram block:
  cache = modules
  loadables = for map(w from worklist):
    uri = w.locator.uri()
    if not(cache.has-key-now(uri)) block:
      provide-map = dict-map(
          w.dependency-map,
          lam(_, v): cache.get-value-now(v.uri()).provides end
      )
      options.before-compile(w.locator)
      {loadable :: Loadable; trace :: List} = compile-module(w.locator, provide-map, cache, options)
      # I feel like here we want to generate two copies of the loadable:
      # - One local for calling on-compile with and serializing
      # - One canonicalized for the local cache
      cache.set-now(uri, loadable)
      local-loadable = cases(Loadable) loadable:
        | module-as-string(provides, env, result) =>
          module-as-string(AU.localize-provides(provides, env), env, result)
      end
      # allow on-compile to return a new loadable
      options.on-compile(w.locator, local-loadable, trace)
    else:
      cache.get-value-now(uri)
    end
  end
  { loadables: loadables, modules: cache }
end

fun compile-program(worklist, options):
  compile-program-with(worklist, SD.make-mutable-string-dict(), options)
end

fun is-builtin-module(uri :: String) -> Boolean:
  string-index-of(uri, "builtin://") == 0
end

fun unique(lst):
  sets.list-to-list-set(lst).to-list()
end

fun compile-module(locator :: Locator, provide-map :: SD.StringDict<CS.Provides>, modules, options) -> {Loadable; List} block:
  G.reset()
  A.global-names.reset()
  #print("Compiling module: " + locator.uri() + "\n")
  env = CS.compile-env(locator.get-globals(), provide-map)
  cases(Option<Loadable>) locator.get-compiled() block:
    | some(loadable) =>
      #print("Module is already compiled\n")
      cases(Loadable) loadable:
        | module-as-string(pvds, ce-unused, m) =>
          {module-as-string(AU.canonicalize-provides(pvds, env), ce-unused, m); empty}
      end
    | none =>
      #print("Module is being freshly compiled\n")
      shadow options = locator.get-options(options)
      libs = locator.get-extra-imports()
      mod = locator.get-module()
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
      ast-ended := AU.wrap-toplevels(ast-ended)
      var wf = W.check-well-formed(ast-ended)
      ast-ended := nothing
      add-phase("Checked well-formedness", wf)
      checker = if options.check-mode and not(is-builtin-module(locator.uri())):
        CH.desugar-check
      else:
        CH.desugar-no-checks
      end
      cases(CS.CompileResult) wf block:
        | ok(_) =>
          var wf-ast = wf.code
          wf := nothing
          var checked = checker(wf-ast)
          wf-ast := nothing
          add-phase(if options.check-mode: "Desugared (with checks)" else: "Desugared (skipping checks)" end, checked)
          var imported = AU.wrap-extra-imports(checked, libs)
          checked := nothing
          add-phase("Added imports", imported)

          var new-desugared = DNew.desugar(imported)
          imported := nothing
          add-phase("New desugar", new-desugared)

          var scoped = RS.desugar-scope(new-desugared, env)
          new-desugared := nothing
          add-phase("Desugared scope", scoped)
          var named-result = RS.resolve-names(scoped.ast, env)
          var any-errors = scoped.errors + named-result.errors
          scoped := nothing
          if is-link(any-errors) block:
            { module-as-string(dummy-provides(locator.uri()), env, CS.err(unique(any-errors)));
              if options.collect-all or options.collect-times:
                phase("Result", named-result.ast, time-now(), ret).tolist()
              else:
                empty
              end }
          else:
            add-phase("Resolved names", named-result)
            #print(new-desugared) # TODO: this is for development
            var provides = AU.get-named-provides(named-result, locator.uri(), env)
            # Once name resolution has happened, any newly-created s-binds must be added to bindings...
            var desugared = D.desugar(named-result.ast)
            named-result.bindings.merge-now(desugared.new-binds)
            # ...in order to be checked for bad assignments here
            any-errors := RS.check-unbound-ids-bad-assignments(desugared.ast, named-result, env)
            add-phase("Fully desugared", desugared.ast)
            var type-checked =
              if options.type-check:
                type-checked = T.type-check(desugared.ast, env, modules)
                if CS.is-ok(type-checked) block:
                  provides := AU.get-typed-provides(type-checked.code, locator.uri(), env)
                  CS.ok(type-checked.code.ast)
                else:
                  type-checked
                end
              else: CS.ok(desugared.ast)
              end
            desugared := nothing
            add-phase("Type Checked", type-checked)
            var stepified =
              if options.trace block:
                # TODO: Clean this up
                cases (CS.CompileResult) type-checked block:
                  | ok(prog) =>
                    print("Pre-stepified program:\n") # TODO
                    print(ST.pretty-ast(prog.block))
                    print("\n")
                  | err(_) => nothing
                end
                stepified = ST.stepify(type-checked)
                add-phase("Stepified", stepified)
                stepified
              else:
                type-checked
              end
            cases(CS.CompileResult) stepified block:
              | ok(_) =>
                var tc-ast = stepified.code
                stepified := nothing
                var dp-ast = DP.desugar-post-tc(tc-ast, env)
                tc-ast := nothing
                var cleaned = dp-ast
                dp-ast := nothing
                cleaned := cleaned.visit(AU.letrec-visitor)
                            .visit(AU.inline-lams)
                            .visit(AU.set-recursive-visitor)
                            .visit(AU.set-tail-visitor)
                add-phase("Cleaned AST", cleaned)
                {final-provides; cr} = if is-empty(any-errors):
                  JSP.trace-make-compiled-pyret(add-phase, cleaned, env, named-result.bindings, named-result.type-bindings, provides, options)
                else:
                  if options.collect-all and options.ignore-unbound:
                    JSP.trace-make-compiled-pyret(add-phase, cleaned, env, options)
                  else:
                    {provides; add-phase("Result", CS.err(unique(any-errors)))}
                  end
                end
                cleaned := nothing
                canonical-provides = AU.canonicalize-provides(final-provides, env)
                mod-result = module-as-string(canonical-provides, env, cr)
                {mod-result; if options.collect-all or options.collect-times: ret.tolist() else: empty end}
              | err(_) =>
                { module-as-string(dummy-provides(locator.uri()), env, stepified);
                  if options.collect-all or options.collect-times:
                    phase("Result", stepified, time-now(), ret).tolist()
                  else: empty
                  end }
            end
          end
        | err(_) =>
          { module-as-string(dummy-provides(locator.uri()), env, wf) ;
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
  is-module-as-string(cr) and CS.is-err(cr.result-printer)
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
  make-standalone(wl, compiled, options)
end

# NOTE(joe): I strongly suspect options will be used in the future
fun make-standalone(wl, compiled, options):
  natives = for fold(natives from empty, w from wl):
    w.locator.get-native-modules().map(_.path) + natives
  end
  
  var all-compile-problems = empty
  static-modules = j-obj(for C.map_list(w from wl):
      loadable = compiled.modules.get-value-now(w.locator.uri())
      cases(Loadable) loadable:
        | module-as-string(_, _, rp) =>
          cases(CS.CompileResult) rp block:
            | ok(code) =>
              j-field(w.locator.uri(), J.j-raw-code(code.pyret-to-js-runnable()))
            | err(problems) =>
              all-compile-problems := problems + all-compile-problems
              j-field(w.locator.uri(), J.j-raw-code("\"error\""))
          end
      end
    end)
  cases(List) all-compile-problems:
    | link(_, _) => left(all-compile-problems)
    | empty =>
      depmap = j-obj(for C.map_list(w from wl):
        deps = w.dependency-map
        j-field(w.locator.uri(),
          j-obj(for C.map_list(k from deps.keys-now().to-list()):
            j-field(k, j-str(deps.get-value-now(k).uri()))
          end))
      end)

      to-load = j-list(false, for C.map_list(w from wl):
        j-str(w.locator.uri())
      end)

      uris = j-obj(for C.map_list(w from wl):
        uri = w.locator.uri()
        hashed = SHA.sha256(uri)
        j-field(hashed, j-str(uri))
      end)

      program-as-js = j-obj([C.clist:
          j-field("staticModules", static-modules),
          j-field("depMap", depmap),
          j-field("toLoad", to-load),
          j-field("uris", uris)
        ])

      right({
        js-ast: program-as-js,
        natives: natives
      })
  end
end
