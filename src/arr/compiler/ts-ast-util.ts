import type * as A from './ts-ast'
import { List, PFunction, Runtime, Option } from './ts-impl-types';
import type * as TJ from './ts-codegen-helpers';
import type * as TCSH from './ts-compile-structs-helpers';
import type * as TCS from './ts-compile-structs';
import type * as TTS from './ts-type-check-structs';

export interface Exports {
  dict: {
    values: {
      dict: { 
        "checkers": PFunction<(l: A.Srcloc) => TJ.Variant<A.Expr, 's-prim-app'>>,
        "append-nothing-if-necessary": PFunction<(prog: A.Program) => A.Program>,
        "wrap-toplevels": PFunction<(prog: A.Program) => A.Program>,
        "inline-lams": PFunction<(prog: A.Program) => A.Program>,
        "set-recursive": PFunction<(prog: A.Program) => A.Program>,
        "set-tail-position": PFunction<(prog: A.Program) => A.Program>,
        "set-safe-letrec-binds": PFunction<(prog: A.Program) => A.Program>,
        "wrap-extra-imports": PFunction<(prog: A.Program, env: TCS.ExtraImports) => A.Program>,
        "import-to-dep": PFunction<(imp: A.ImportType) => TCS.Dependency>,
        "strip-annotations": PFunction<(prog: A.Program) => A.Program>,
        "localize-provides": PFunction<(provides: TCS.Provides, compileEnv: TCS.CompileEnvironment) => TCS.Provides>,
        "canonicalize-provides": PFunction<(provides: TCS.Provides, compileEnv: TCS.CompileEnvironment) => TCS.Provides>,
        "get-typed-provides": PFunction<(namedResult: TCS.NameResolution, program: TTS.Typed, uri: string, compileEnv: TCS.CompileEnvironment) => TCS.Provides>,
        "get-named-provides": PFunction<(namedResult: TCS.NameResolution, uri: string, compileEnv: TCS.CompileEnvironment) => TCS.Provides>,
      }
    }
  }
}

({
  requires: [
    { 'import-type': 'dependency', protocol: 'js-file', args: ['ts-codegen-helpers']},
    { 'import-type': 'dependency', protocol: 'js-file', args: ['ts-compile-structs-helpers']},
    { 'import-type': 'dependency', protocol: 'file', args: ['ast.arr'] },
    { 'import-type': 'dependency', protocol: 'file', args: ['compile-structs.arr'] },
  ],
  nativeRequires: [],
  provides: {
    values: {
      "checkers": "tany",
      "append-nothing-if-necessary": "tany",
      "wrap-toplevels": "tany",
      "inline-lams": "tany",
      "set-recursive": "tany",
      "set-tail-position": "tany",
      "set-safe-letrec-binds": "tany",
      "wrap-extra-imports": "tany",
      "import-to-dep": "tany",
      "strip-annotations": "tany",
    },
  },
  theModule: function(runtime: Runtime, _, __, tj : TJ.Exports, TCSH: TCSH.Exports, Ain: A.Exports, CSin: TCS.Exports) {
    const { 
      ExhaustiveSwitchError,
      InternalCompilerError,
      listToArray,
      mutableStringDictFromMap,
      stringDictFromMap,
      map,
      sameName,
      nameToKey,
    } = tj;
    const A = Ain.dict.values.dict;
    const CS = CSin.dict.values.dict;

    function okLast(stmt: A.Expr): boolean {
      switch(stmt.$name) {
        case 's-let':
        case 's-var':
        case 's-rec':
        case 's-fun':
        case 's-data':
        case 's-contract':
        case 's-check':
        case 's-type':
        case 's-newtype': return false;
        default: return true;
      }
    }

    const mtList: List<any> = runtime.ffi.makeList([]);
    const flatPrimApp = A['prim-app-info-c'].app(false);
    function checkers(l: A.Srcloc): TJ.Variant<A.Expr, 's-prim-app'> {
      return A['s-prim-app'].app(l, "current-checker", mtList, flatPrimApp);
    }

    function appendNothingIfNecessary(prog: A.Program): A.Program {
      const { l: l1, _use, _provide,"provided-types": providedTypes, provides, imports, block: body } = prog.dict;
      if (body.$name === "s-block") {
        const l2 = body.dict.l;
        const stmts = listToArray(body.dict.stmts);
        const nothing = A['s-id'].app(A['dummy-loc'], A['s-name'].app(l2, "nothing"));
        if (stmts.length === 0) {
          return A['s-program'].app(l1, _use, _provide, providedTypes, provides, imports,
            A['s-block'].app(l2, runtime.ffi.makeList([nothing])));
        } else {
          if (okLast(stmts[stmts.length - 1])) {
            return prog;
          } else {
            stmts.push(nothing);
            return A['s-program'].app(l1, _use, _provide, providedTypes, provides, imports,
              A['s-block'].app(l2, runtime.ffi.makeList(stmts)));
          }
        }
      }
      return prog;
    }

    function wrapIfNeeded(exp: A.Expr): A.Expr {
      const l = exp.dict.l;
      if (okLast(exp) && exp.$name !== "s-spy-block") {
        return A['s-prim-app'].app(l, "$traceValue", runtime.ffi.makeList([A['s-srcloc'].app(l, l), exp]), A['prim-app-info-c'].app(true));
      } else {
        return exp;
      }
    }

    function wrapToplevels(prog: A.Program): A.Program {
      const { l, _use, _provide, "provided-types": providedTypes, provides, imports, block: body } = prog.dict;
      let newBody: A.Expr;
      switch(body.$name) {
        case 's-block': {
          const l2 = body.dict.l;
          const stmts = listToArray(body.dict.stmts);
          newBody = A['s-block'].app(l2, runtime.ffi.makeList(stmts.map(wrapIfNeeded)));
          break;
        }
        default: 
          newBody = wrapIfNeeded(body);
          break;
      }
      return A['s-program'].app(l, _use, _provide, providedTypes, provides, imports, newBody);
    }

    function inlineLams(prog: A.Program): A.Program {
      return map<A.Program | A.Expr, A.Program>({
        's-app': (visitor, app: TJ.Variant<A.Expr, 's-app'>) => {
          const loc = app.dict.l;
          const f = app.dict._fun;
          const exps = listToArray(app.dict.args);
          switch(f.$name) {
            case 's-lam': {
              const { l, ann, body } = f.dict;
              const args = listToArray(f.dict.args);
              if (args.length === exps.length) {
                const a = A['global-names'].dict['make-atom'].app("inline_body");
                const letBinds: A.LetBind[] = [];
                for (let i = 0; i < args.length; i++) {
                  letBinds.push(A['s-let-bind'].app(args[i].dict.l, args[i], map(visitor, exps[i])));
                }
                switch(ann.$name) {
                  case 'a-blank':
                  case 'a-any':
                    return A['s-let-expr'].app(l, runtime.ffi.makeList(letBinds), map(visitor, body), false);
                  default: {
                    letBinds.push(A['s-let-bind'].app(body.dict.l, A['s-bind'].app(body.dict.l, false, a, ann), map(visitor, body)));
                    return A['s-let-expr'].app(l, runtime.ffi.makeList(letBinds), A['s-id'].app(l, a), false);
                  }
                }
              } 
            }
          }
          return A['s-app'].app(loc, map(visitor, f), runtime.ffi.makeList(exps.map((e) => map(visitor, e))));
        }
      }, prog);
    }

    /**
     * Replaces all `s-app` expressions with `s-app-enhanced` with correct
     * `is-recursive` parameters but uninformative `is-tail` parameters (all are set to `false`).
     * 
     * @param prog the input program to transform
     * @returns Program, but with all `s-app` replaced
     */
    function setRecursive(prog: A.Program): A.Program {
      type Scope = {
        type: "none"
      } | {
        type: "fun",
        id: A.Name
      } | {
        type: "method",
        selfId: A.Name,
        name: string
      } | {
        type: "partial-fun",
        id: A.Name
      } | {
        type: "partial-method",
        name: string
      }

      const clearScope: Scope = { type: "none" };

      /** Return a Boolean indicating whether a call with `f` is recursive or not */
      function isRecursive(scope: Scope, f: A.Expr): boolean {
        if (scope.type === 'fun' && f.$name === 's-id-letrec') {
          return sameName(f.dict.id, scope.id);
        } else if (scope.type === 'partial-fun') {
          throw new InternalCompilerError("Error while querying: after partial-fun-s should immediately be fun-s");
        }
        return false;
      }

      function activateFun(scope: Scope): Scope {
        switch(scope.type) {
          case 'partial-fun': {
            return { type: 'fun', id: scope.id };
          }
          case 'none': // no letrec, meaning it's just normal lambda: `lam(x): x + 1 end(5)`, for example
          case 'fun':  // lam in function: `fun foo() lam(): 1 end end`
          case 'method': // lam in method: `{ foo(self): lam(): 1 end end }`
          case 'partial-method': // lam in object: `{ a : lam(): id(1) end }`
            return clearScope;
          default: throw new ExhaustiveSwitchError(scope); 
        }
      }
      function collectFunName(scope: Scope, binding: A.LetrecBind): Scope {
        if (binding.dict.value.$name === 's-lam') {
          return { type: 'partial-fun', id: (binding.dict.b as TJ.Variant<A.Bind, 's-bind'>).dict.id };
        }
        return scope;
      }
      function collectMethodSelf(scope: Scope, selfBind: A.Bind): Scope {
        switch(scope.type) {
          case 'partial-method': {
            return { type: 'method', selfId: (selfBind as TJ.Variant<A.Bind, 's-bind'>).dict.id, name: scope.name };
          }
          case 'none': // `method(self): 1 end`
          case 'fun': // fun foo(): method(self): 1 end end
          case 'method': // { a(self1): method(self2): 1 end end }
            return clearScope;
          case 'partial-fun':
            throw new InternalCompilerError("Error while collecting self: after partial-fun-s should immediately be fun-s")
        }
      }
      function collectMethodName(methodName: string): Scope {
        return { type: 'partial-method', name: methodName };
      }

      return map<
        A.Program | A.Expr | A.Member | A.Name | A.Ann | A.Bind | A.LetrecBind | Option<A.Expr>, 
        A.Program,
        Scope
      >({
        's-app': (visitor, app: TJ.Variant<A.Expr, 's-app'>, scope) => {
          const exps = listToArray(app.dict.args)
          return A['s-app-enriched'].app(app.dict.l,
            map(visitor, app.dict._fun, scope),
            runtime.ffi.makeList(exps.map((e) => map(visitor, e, scope))),
            A['app-info-c'].app(isRecursive(scope, app.dict._fun), false));
        },
        's-lam': (visitor, lam: TJ.Variant<A.Expr, 's-lam'>, scope) => {
          const { l, name, ann, doc, body, "_check-loc": checkLoc, _check, blocky } = lam.dict;
          const params = listToArray(lam.dict.params);
          const args = listToArray(lam.dict.args);
          return A['s-lam'].app(
            l,
            name,
            runtime.ffi.makeList(params.map((p) => map(visitor, p, clearScope))),
            runtime.ffi.makeList(args.map((a) => map(visitor, a, clearScope))),
            map(visitor, ann, clearScope),
            doc,
            map(visitor, body, activateFun(scope)),
            checkLoc,
            map(visitor, _check, clearScope),
            blocky
          );
        },
        's-letrec': (visitor, letrec: TJ.Variant<A.Expr, 's-letrec'>, scope) => {
          const { l, body, blocky } = letrec.dict;
          const binds = listToArray(letrec.dict.binds);
          const visitedBinds: A.LetrecBind[] = [];
          for (let b of binds) {
            visitedBinds.push(map(visitor, b, collectFunName(scope, b)));
          }
          return A['s-letrec'].app(l, runtime.ffi.makeList(visitedBinds), map(visitor, body, scope), blocky);
        },
        's-data-field': (visitor, dataField: TJ.Variant<A.Member, 's-data-field'>) => {
          const { l, name, value } = dataField.dict;
          const ret = A['s-data-field'].app(l, name, map(visitor, value, collectMethodName(name)));
          return ret;
        },

      }, prog, clearScope);
    }

    /**
     * Corrects the `is-tail` information in any `s-app-enhanced` expressions
     * Precondition: assumes no `s-app` expressions any more
     * 
     * @param prog the input program to transform
     * @returns Program, but with all `s-app-enhanced` set with improved `is-tail` information
     */
    function setTailPosition(prog: A.Program): A.Program {
      function isStatefulAnn(ann: A.Ann): boolean {
        switch(ann.$name) {
          case 'a-blank':
          case 'a-any':
          case 'a-name':
          case 'a-type-var':
          case 'a-arrow':
          case 'a-arrow-argnames':
          case 'a-method': return false;
          case 'a-record':
            // TODO(Ben): should this be some instead of every?
            return listToArray(ann.dict.fields).map((f) => f.dict.ann).every(isStatefulAnn);
          case 'a-tuple':
            // TODO(Ben): should this be some instead of every?
            return listToArray(ann.dict.fields).every(isStatefulAnn);
          case 'a-app': return isStatefulAnn(ann.dict.ann);
          case 'a-pred':
          case 'a-dot': return true // NOTE(Oak): could refine this later
          case 'a-checked': throw new InternalCompilerError("Not yet implemented");
          default: throw new ExhaustiveSwitchError(ann);
        }
      }
      return map<
        A.Program | A.Expr | A.Ann | A.DefinedModule | A.DefinedType | A.DefinedValue | A.LetBind | A.LetrecBind | A.IfBranch | A.CasesBranch | A.Name | A.Bind | A.Option<A.Expr> | A.Member,
        A.Program,
        boolean
      >({
        's-app-enriched': (visitor, app: TJ.Variant<A.Expr, 's-app-enriched'>, isTail) => {
          return A['s-app-enriched'].app(
            app.dict.l,
            map(visitor, app.dict._fun, false),
            runtime.ffi.makeList(listToArray(app.dict.args).map((a) => map(visitor, a, false))),
            // This one line is the point of this visitor!
            A['app-info-c'].app(app.dict['app-info'].dict['is-recursive'], isTail) 
          )
        },
        's-module': (visitor, mod: TJ.Variant<A.Expr, 's-module'>, isTail) => {
          return A['s-module'].app(
            mod.dict.l,
            map(visitor, mod.dict.answer, false),
            runtime.ffi.makeList(listToArray(mod.dict['defined-modules']).map((dm) => map(visitor, dm, false))),
            runtime.ffi.makeList(listToArray(mod.dict['defined-values']).map((dv) => map(visitor, dv, false))),
            runtime.ffi.makeList(listToArray(mod.dict['defined-types']).map((dt) => map(visitor, dt, false))),
            map(visitor, mod.dict.checks, false),
          );
        },
        's-let-expr': (visitor, letExp: TJ.Variant<A.Expr, 's-let-expr'>, isTail) => {
          return A['s-let-expr'].app(
            letExp.dict.l,
            runtime.ffi.makeList(listToArray(letExp.dict.binds).map((b) => map(visitor, b, false))),
            map(visitor, letExp.dict.body, isTail),
            letExp.dict.blocky,
          );
        },
        's-letrec': (visitor, letrec: TJ.Variant<A.Expr, 's-letrec'>, isTail) => {
          return A['s-letrec'].app(
            letrec.dict.l,
            runtime.ffi.makeList(listToArray(letrec.dict.binds).map((b) => map(visitor, b, false))),
            map(visitor, letrec.dict.body, isTail),
            letrec.dict.blocky,
          );
        },
        's-if-branch': (visitor, ifBranch: A.IfBranch, isTail) => {
          return A['s-if-branch'].app(
            ifBranch.dict.l,
            map(visitor, ifBranch.dict.test, false),
            map(visitor, ifBranch.dict.body, isTail),
          );
        },
        's-cases-else': (visitor, cases: TJ.Variant<A.Expr, 's-cases-else'>, isTail) => {
          return A['s-cases-else'].app(
            cases.dict.l,
            map(visitor, cases.dict.typ, isTail),
            map(visitor, cases.dict.val, false),
            runtime.ffi.makeList(listToArray(cases.dict.branches).map((b) => map(visitor, b, isTail))),
            map(visitor, cases.dict._else, isTail),
            cases.dict.blocky
          );
        },
        's-block': (visitor, block: TJ.Variant<A.Expr, 's-block'>, isTail) => {
          const stmts = listToArray(block.dict.stmts);
          const lastStmt = stmts.pop()!;
          return A['s-block'].app(
            block.dict.l,
            runtime.ffi.makeList([
              ...stmts.map((s) => map<A.Expr, A.Expr, boolean>(visitor, s, false)),
              map(visitor, lastStmt, isTail)
            ]),
          );
        },
        's-check-expr': (visitor, check: TJ.Variant<A.Expr, 's-check-expr'>, isTail) => {
          return A['s-check-expr'].app(
            check.dict.l,
            map(visitor, check.dict.expr, false),
            map(visitor, check.dict.ann, false)
          );
        },
        's-lam': (visitor, lam: TJ.Variant<A.Expr, 's-lam'>, isTail) => {
          return A['s-lam'].app(
            lam.dict.l,
            lam.dict.name,
            runtime.ffi.makeList(listToArray(lam.dict.params).map((p) => map(visitor, p, false))),
            runtime.ffi.makeList(listToArray(lam.dict.args).map((a) => map(visitor, a, false))),
            map(visitor, lam.dict.ann, false),
            lam.dict.doc,
            map(visitor, lam.dict.body, !isStatefulAnn(lam.dict.ann)),
            lam.dict['_check-loc'],
            map(visitor, lam.dict._check, false),
            lam.dict.blocky,
          );
        },
        's-array': (visitor, array: TJ.Variant<A.Expr, 's-array'>, isTail) => {
          return A['s-array'].app(
            array.dict.l,
            runtime.ffi.makeList(listToArray(array.dict.values).map((v) => map(visitor, v, false)))
          );
        },
        's-prim-app': (visitor, app: TJ.Variant<A.Expr, 's-prim-app'>, isTail) => {
          return A['s-prim-app'].app(
            app.dict.l,
            app.dict._fun,
            runtime.ffi.makeList(listToArray(app.dict.args).map((a) => map(visitor, a, false))),
            app.dict['app-info'],
          );
        },
        's-dot': (visitor, dot: TJ.Variant<A.Expr, 's-dot'>, isTail) => {
          return A['s-dot'].app(
            dot.dict.l,
            map(visitor, dot.dict.obj, false),
            dot.dict.field
          );
        },
        's-get-bang': (visitor, getExp: TJ.Variant<A.Expr, 's-get-bang'>, isTail) => {
          return A['s-get-bang'].app(
            getExp.dict.l,
            map(visitor, getExp.dict.obj, false),
            getExp.dict.field
          );
        },
        's-assign': (visitor, assign: TJ.Variant<A.Expr, 's-assign'>, isTail) => {
          return A['s-assign'].app(
            assign.dict.l,
            map(visitor, assign.dict.id, false),
            map(visitor, assign.dict.value, false)
          );
        },
        's-obj': (visitor, obj: TJ.Variant<A.Expr, 's-obj'>, isTail) => {
          return A['s-obj'].app(
            obj.dict.l,
            runtime.ffi.makeList(listToArray(obj.dict.fields).map((f) => map(visitor, f, false)))
          );
        },
        's-update': (visitor, update: TJ.Variant<A.Expr, 's-update'>, isTail) => {
          return A['s-update'].app(
            update.dict.l,
            map(visitor, update.dict.supe, false),
            runtime.ffi.makeList(listToArray(update.dict.fields).map((f) => map(visitor, f, false)))
          );
        },
        's-extend': (visitor, extend: TJ.Variant<A.Expr, 's-extend'>, isTail) => {
          return A['s-extend'].app(
            extend.dict.l,
            map(visitor, extend.dict.supe, false),
            runtime.ffi.makeList(listToArray(extend.dict.fields).map((f) => map(visitor, f, false)))
          );
        },
      }, prog, false);
    }

    function setSafeLetrecBinds(prog: A.Program): A.Program {
      let env = new Map<string, boolean>();
      return map<A.Program | A.Expr | A.LetrecBind, A.Program>({
        's-letrec': (visitor, letrec: TJ.Variant<A.Expr, 's-letrec'>) => {
          const oldEnv = env;
          const binds = listToArray(letrec.dict.binds);
          const bindEnvs: Map<string, boolean>[] = [];
          for (let i = 0; i < binds.length; i++) {
            const newEnv = new Map<string, boolean>(env);
            const value = binds[i].dict.value;
            const rhsIsDelayed = (value.$name === 's-lam');
            for (let j = 0; j < binds.length; j++) {
              const b2 = binds[j].dict.b as TJ.Variant<A.Bind, 's-bind'>;
              const key = nameToKey(b2.dict.id);
              if (i < j) {
                newEnv.set(key, false);
              } else if (i == j) {
                newEnv.set(key, rhsIsDelayed);
              } else {
                newEnv.set(key, true);
              }
            }
            bindEnvs.push(newEnv);
          }
          const newBinds: A.LetrecBind[] = [];
          for (let i = 0; i < binds.length; i++) {
            env = bindEnvs[i];
            newBinds.push(map<A.LetrecBind>(visitor, binds[i]));
          }
          env = bindEnvs[bindEnvs.length - 1];
          env.set(nameToKey((binds[binds.length - 1].dict.b as TJ.Variant<A.Bind, 's-bind'>).dict.id), true);
          const newBody = map<A.Expr>(visitor, letrec.dict.body);
          env = oldEnv;
          return A['s-letrec'].app(letrec.dict.l, runtime.ffi.makeList(newBinds), newBody, letrec.dict.blocky);
        },
        's-id-letrec': (visitor, idLetrec: TJ.Variant<A.Expr, 's-id-letrec'>) => {
          const isSafe = env.get(nameToKey(idLetrec.dict.id))!;
          return A['s-id-letrec'].app(idLetrec.dict.l, idLetrec.dict.id, isSafe);
        }
      }, prog);
    }
    
    function wrapExtraImports(prog: A.Program, env: TCS.ExtraImports): A.Program {
      const { l, _use, _provide, "provided-types": providedTypes, provides, block } = prog.dict;
      const origImports = listToArray(prog.dict.imports)
      if(_use.$name === 'some') {
        const useImports = [A['s-include'].app(A['dummy-loc'], _use.dict.value.dict.mod), ...origImports];
        return A['s-program'].app(l, _use, _provide, providedTypes, provides, runtime.ffi.makeList(useImports), block);
      }

      const imports = listToArray(env.dict.imports);
      /*
         NOTE(Ben): I've moved the existing p.imports *after* these generated imports,
         so that any user-requested imports have to coexist in the global environment,
         rather than globals having to coexist in the user's environment.
         Additionally, this allows for better srcloc reporting: suppose the user's program says
           `import some from option`
         which is already existing in the global scope.  The global import will have
         srcloc=A.dummy-loc, but the deliberate import will have srcloc within the file,
         which will ensure the error message refers to that explicit location.
         (I can't change how `resolve-scope:add-value-name` or `resolve-scope:make-import-atom-for`
         handle this case, because we haven't finished resolving names to know whether the name
         collision is acceptable or not.)
      */
      const dummyLoc = A['dummy-loc']
      const fullImports: A.Import[] = [];
      for (let i of imports) {
        let nameToUse: A.Name;
        if (i.dict['as-name'] === "_") {
          nameToUse = A['global-names'].dict['make-atom'].app("$extra-import");
        } else {
          nameToUse = A['s-name'].app(dummyLoc, i.dict['as-name']);
        }
        let astDep: A.ImportType;
        switch(i.dict.dependency.$name) {
          case 'builtin': {
            astDep = A['s-const-import'].app(l, i.dict.dependency.dict.modname); 
            break;
          }
          case 'dependency': {
            astDep = A['s-special-import'].app(l, i.dict.dependency.dict.protocol, i.dict.dependency.dict.arguments);
            break;
          }
          default: throw new ExhaustiveSwitchError(i.dict.dependency);
          const importLine = A['s-import'].app(l, astDep, nameToUse);
          const values = listToArray(i.dict.values).map((v) => {
            return A['s-include-name'].app(dummyLoc, 
              A['s-module-ref'].app(dummyLoc, runtime.ffi.makeList([A['s-name'].app(dummyLoc, v)]), runtime.ffi.makeNone())
            );
          });
          const types = listToArray(i.dict.types).map((t) => {
            return A['s-include-type'].app(dummyLoc, 
              A['s-module-ref'].app(dummyLoc, runtime.ffi.makeList([A['s-name'].app(dummyLoc, t)]), runtime.ffi.makeNone())
            );
          })
          const includeLine = A['s-include-from'].app(
            l,
            runtime.ffi.makeList([nameToUse]),
            runtime.ffi.makeList([...values, ...types])
          )
          fullImports.push(importLine, includeLine);
        }
      }
      fullImports.push(...origImports);
      return A['s-program'].app(l, _use, _provide, providedTypes, provides, runtime.ffi.makeList(fullImports), block);
    }

    function importToDep(imp: A.ImportType): TCS.Dependency {
      switch(imp.$name) {
        case 's-const-import': return CS.builtin.app(imp.dict.mod);
        case 's-special-import': return CS.dependency.app(imp.dict.kind, imp.dict.args);
        default: throw new ExhaustiveSwitchError(imp);
      }
    }
    function stripAnnotations(prog: A.Program): A.Program {
      return map<A.Program | A.Ann | A.AField, A.Program>({
        'a-blank': (_visitor, _) => A['a-blank'],
        'a-any': (_visitor, _) => A['a-blank'],
        'a-name': (_visitor, _) => A['a-blank'],
        'a-type-var': (_visitor, _) => A['a-blank'],
        'a-arrow': (_visitor, _) => A['a-blank'],
        'a-arrow-argnames': (_visitor, _) => A['a-blank'],
        'a-method': (_visitor, _) => A['a-blank'],
        'a-record': (_visitor, _) => A['a-blank'],
        'a-tuple': (_visitor, _) => A['a-blank'],
        'a-app': (_visitor, _) => A['a-blank'],
        'a-pred': (_visitor, _) => A['a-blank'],
        'a-dot': (_visitor, _) => A['a-blank'],
        'a-field': (_visitor, _) => A['a-blank'],
      }, prog);
    }

    const exports: Exports['dict']['values']['dict'] = {
      "checkers": runtime.makeFunction(checkers),
      "append-nothing-if-necessary": runtime.makeFunction(appendNothingIfNecessary),
      "wrap-toplevels": runtime.makeFunction(wrapToplevels),
      "inline-lams": runtime.makeFunction(inlineLams),
      "set-recursive": runtime.makeFunction(setRecursive),
      "set-tail-position": runtime.makeFunction(setTailPosition),
      "set-safe-letrec-binds": runtime.makeFunction(setSafeLetrecBinds),
      "wrap-extra-imports": runtime.makeFunction(wrapExtraImports),
      "import-to-dep": runtime.makeFunction(importToDep),
      "strip-annotations": runtime.makeFunction(stripAnnotations),
      "localize-provides": null as any, // TODO!
      "canonicalize-provides": null as any, // TODO!
      "get-typed-provides": null as any, // TODO!
      "get-named-provides": null as any, // TODO!
    };
    return runtime.makeModuleReturn(exports, {});
  }
})