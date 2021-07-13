import type * as TS from './ts-type-structs';
import type * as A from './ts-ast';
import type * as CS from './ts-compile-structs';
import type * as TJ from './ts-codegen-helpers';
import type { List, MutableStringDict, PFunction, StringDict, PMethod } from './ts-impl-types';

type SDExports = {
  dict: { values: { dict: {
    'make-mutable-string-dict': PFunction<<T>() => MutableStringDict<T>>
    'is-mutable-string-dict': PFunction<(val: any) => boolean>,
    'make-string-dict': PFunction<<T>() => StringDict<T>>,
    'is-string-dict': PFunction<(val: any) => boolean>,
    'map-keys': PFunction<<T, U>(f: ((key: T) => U), isd: StringDict<T>) => List<U>>,
    'map-keys-now': PFunction<<T, U>(f: ((key: T) => U), msd: MutableStringDict<T>) => List<U>>,
    'fold-keys': PFunction<<T, U>(f: (key: string, acc: U) => U, init: U, isd: StringDict<T>) => U>,
    'fold-keys-now': PFunction<<T, U>(f: (key: string, acc: U) => U, init: U, msd: MutableStringDict<T>) => U>,
    'each-key': PFunction<<T>(f: ((key: T) => void), isd: StringDict<T>) => void>,
    'each-key-now': PFunction<<T>(f: ((key: T) => void), msd: MutableStringDict<T>) => void>,
  }}}
}

type DesugarInfo = {
  dict: {
    ast: A.Program,
    'new-binds': MutableStringDict<CS.ValueBind>,
  }
}

({
  requires: [
    { 'import-type': 'builtin', name: 'string-dict' },
    { 'import-type': 'dependency', protocol: 'js-file', args: ['ts-codegen-helpers']},
    { 'import-type': 'dependency', protocol: 'file', args: ['type-structs.arr']},
    { 'import-type': 'dependency', protocol: 'file', args: ['ast.arr']},
    { 'import-type': 'dependency', protocol: 'file', args: ['compile-structs.arr']},
    { 'import-type': 'dependency', protocol: 'file', args: ['type-defaults.arr']},
 ],
  nativeRequires: ["escodegen", "path"],
  provides: {
    values: {
      "desugar": "tany"
    }
  },
  theModule: function(runtime, _, __, SD: SDExports, tj : TJ.Exports, TS : (TS.Exports), Ain : (A.Exports), CS : (CS.Exports)) {
    const A = Ain.dict.values.dict;
    const reactorOptionalFields = new Map<string, (l: A.Srcloc) => TJ.Variant<A.Ann, "a-name">>();
    reactorOptionalFields.set("last-image", l => A['a-name'].app(l, A['s-type-global'].app("Function")));
    reactorOptionalFields.set("on-tick", l => A['a-name'].app(l, A['s-type-global'].app("Function")));
    reactorOptionalFields.set("to-draw", l => A['a-name'].app(l, A['s-type-global'].app("Function")));
    reactorOptionalFields.set("on-key", l => A['a-name'].app(l, A['s-type-global'].app("Function")));
    reactorOptionalFields.set("on-mouse", l => A['a-name'].app(l, A['s-type-global'].app("Function")));
    reactorOptionalFields.set("stop-when", l => A['a-name'].app(l, A['s-type-global'].app("Function")));
    reactorOptionalFields.set("seconds-per-tick", l => A['a-name'].app(l, A['s-type-global'].app("NumPositive")));
    reactorOptionalFields.set("title", l => A['a-name'].app(l, A['s-type-global'].app("String")));
    reactorOptionalFields.set("close-when-stop", l => A['a-name'].app(l, A['s-type-global'].app("Boolean")));

    /**
    Desugar non-scope and non-check based constructs.
    Preconditions on program:
      - well-formed
      - contains no s-var, s-fun, s-data, s-check, or s-check-test
      - contains no s-provide in headers
      - all where blocks are none
      - contains no s-name (e.g. call resolve-names first)

    Postconditions on program:
      - NOTE(tiffay): Outdated postconditions
      - in addition to preconditions,
        contains no s-for, s-if (will all be s-if-else), s-op, s-method-field,
                    s-cases (will all be s-cases-else), s-not, s-when, s-if-pipe, s-paren
      - contains no s-underscore in expression position (but it may
        appear in binding positions as in s-let-bind, s-letrec-bind)

    Postconditions on program:
      - contains no s-if, s-if-pipe, s-if-else-pipe, s-cases, s-when, s-paren
      - s-for will remain: it is useful to know for the code generator
      - s-op will remain: code generated for these ops are an implementation detail
      - s-method-field will remain: different code generated properties based on position
      - s-not does not exist
      - contains no s-underscore in expression position (but it may
        appear in binding positions as in s-let-bind, s-letrec-bind)
      - s-construct is not desugared
    */
    function desugar(program: A.Program, options, makeNameStart: number): DesugarInfo {
      const {
        listToArray,
        map,
        nameToKey,
        MakeName,
        ExhaustiveSwitchError,
        InternalCompilerError,
      } = tj;

      // this is duplicated code and needs to be deleted when merging
      type DropFirst<T extends unknown[]> = ((...p: T) => void) extends ((p1: infer P1, ...rest: infer R) => void) ? R : never

      function callMethod<Name extends string, O extends {dict: {[n in Name]: PMethod<any, (...args: any[]) => any>}}>(obj : O, name: Name, ...args: DropFirst<Parameters<O["dict"][Name]["full_meth"]>>) : ReturnType<O["dict"][Name]["full_meth"]> {
        return obj.dict[name].full_meth(obj, ...args);
      }
       
      function mapFromStringDict<T>(s : StringDict<T>) : Map<string, T> {
        const m : Map<string, T> = new Map();
        for (let valKey of listToArray(callMethod(s, 'keys-list'))) {
          m.set(valKey, callMethod(s, "get-value", valKey));
        }
        return m;
      }
      function mapFromMutableStringDict<T>(s : MutableStringDict<T>) : Map<string, T> {
        const m : Map<string, T> = new Map();
        for (let valKey of listToArray(callMethod(s, 'keys-list-now'))) {
          m.set(valKey, callMethod(s, "get-value-now", valKey));
        }
        return m;
      }
      function stringDictFromMap<T>(m : Map<string, T>): StringDict<T> {
        return callMethod(mutableStringDictFromMap(m), 'freeze');
      }
      function mutableStringDictFromMap<T>(m : Map<string, T>): MutableStringDict<T> {
        const s = SD.dict.values.dict['make-mutable-string-dict'].app<T>();
        for (const [k, v] of m.entries()) {
          callMethod(s, 'set-now', k, v);
        }
        return s;
      }
      // end duplicated code
       
      const names = MakeName(makeNameStart); // TODO change this back to A['global-names']
      const generatedBinds = new Map<string, CS.ValueBind>();
      const flatPrimApp = A['prim-app-info-c'].app(false);
  
      function boLocal(l: A.Srcloc, originalName: A.Name): CS.BindOrigin {
        switch (l.$name) {
          case 'builtin': {
            return CS.dict.values.dict['bind-origin'].app(l, l, true, l.dict['module-name'], originalName);
          }
          case 'srcloc': {
            return CS.dict.values.dict['bind-origin'].app(l, l, true, l.dict.source, originalName);
          }
          default: {
            throw new ExhaustiveSwitchError(l);
          }
        }
      }
      function g(id: string): TJ.Variant<A.Name, 's-global'> {
        return A['s-global'].app(id);
      }
      function gid(l : A.Srcloc, id: string): TJ.Variant<A.Expr, 's-id'> {
        return A['s-id'].app(l, g(id));
      }
      function noBranchesExn(l: A.Srcloc, typ: string): TJ.Variant<A.Expr, 's-prim-app'> {
        const srcloc = A['s-srcloc'].app(l, l);
        const str = A['s-str'].app(l, typ);
        return A['s-prim-app'].app(l, "throwNoBranchesMatched", runtime.ffi.makeList([srcloc, str]), flatPrimApp);
      }
      function mkIdAnn(l: A.Srcloc, base: string, ann: A.Ann): { id: A.Name, idB: A.Bind, idE: A.Expr } {
        const a = names.makeAtom(base);
        const key = nameToKey(a);
        const bindingLocal = boLocal(l, a);
        const value = CS.dict.values.dict['value-bind'].app(
          bindingLocal,
          CS.dict.values.dict['vb-let'],
          a,
          ann
        );
        generatedBinds.set(
          key,
          value,
        );
        return { 
          id: a, 
          idB: A['s-bind'].app(l, false, a, ann), 
          idE: A['s-id'].app(l, a),
        };
      }
      function mkId(l: A.Srcloc, base: string): { id: A.Name, idB: A.Bind, idE: A.Expr } {
        return mkIdAnn(l, base, A['a-blank']);
      }
      function desugarIf(l: A.Srcloc, branches: List<TJ.Variant<A.IfBranch, 's-if-branch'> | TJ.Variant<A.IfPipeBranch, 's-if-pipe-branch'>>, _else: A.Expr, blocky: boolean, visitor): A.Expr {
        let dsElse = map(visitor, _else);
        return listToArray(branches).reduceRight((acc, branch) => {
          const dsTest = map(visitor, branch.dict.test);
          const dsBody = map(visitor, branch.dict.body);
          return A['s-if-else'].app(
            l,
            runtime.ffi.makeList([A['s-if-branch'].app(
              branch.dict.l,
              dsTest,
              dsBody)]),
            acc,
            blocky);
        }, dsElse);
      }
      function isUnderscore(e: A.Expr): boolean {
        return A['is-s-id'].app(e) && A['is-s-underscore'].app(e.dict.id);
      }
      function dsCurryArgs(l: A.Srcloc, args: Array<A.Expr>): [A.Bind[], A.Expr[]] {
        const binds: Array<A.Bind> = [];
        const exprs: Array<A.Expr> = [];
        args.forEach(arg => {
          if (isUnderscore(arg)) {
            const argId = mkId(l, "arg_");
            binds.push(argId.idB);
            exprs.push(argId.idE);
          } else {
            exprs.push(arg);
          }
        });
        return [binds, exprs];
      }
      function dsCurryNullary<F>(rebuildNode: (l: A.Srcloc, obj: A.Expr, field: F) => A.Expr, l: A.Srcloc, obj: A.Expr, field: F, visitor): A.Expr {
        if(isUnderscore(obj)) {
          const curriedObj = mkId(l, "recv_");
          return A['s-lam'].app(
            l,
            "",
            runtime.ffi.makeList([]),
            runtime.ffi.makeList([curriedObj.idB]),
            A['a-blank'],
            "",
            rebuildNode(l, curriedObj.idE, field),
            runtime.ffi.makeNone(),
            runtime.ffi.makeNone(),
            false
          );
        } else {
          return rebuildNode(l, map(visitor, obj), field);
        }
      }
      function dsCurryBinop(l: A.Srcloc, e1: A.Expr, e2: A.Expr, rebuild: (e1: A.Expr, e2: A.Expr) => A.Expr): A.Expr {
        const [params, args] = dsCurryArgs(l, [e1, e2]);
        if (params.length === 0) {
          return rebuild(e1, e2);
        } else {
          return A['s-lam'].app(
            l,
            "",
            runtime.ffi.makeList([]),
            runtime.ffi.makeList(params),
            A['a-blank'],
            "",
            rebuild(args[0], args[1]),
            runtime.ffi.makeNone(),
            runtime.ffi.makeNone(),
            false);
        }
      }
      function dsCurry(l: A.Srcloc, f: A.Expr, args: Array<A.Expr>, visitor): A.Expr {
        if (f.$name === 's-dot' && isUnderscore(f.dict.obj)) { // _.foo(args)
          const curriedObj = mkId(l, "recv_");
          const [params, argsRight]= dsCurryArgs(l, args);
          params.unshift(curriedObj.idB);
          return A['s-lam'].app(
            l,
            "",
            runtime.ffi.makeList([]),
            runtime.ffi.makeList(params),
            A['a-blank'],
            "",
            A['s-app'].app(
              l,
              A['s-dot'].app(
                l,
                curriedObj.idE,
                f.dict.field
              ),
              runtime.ffi.makeList(argsRight)
            ),
            runtime.ffi.makeNone(),
            runtime.ffi.makeNone(),
            false
          );
        } else {
          const [params, argsRight] = dsCurryArgs(l, args);
          if (isUnderscore(f)) { // _(args)
            const fId = mkId(l, "f_");
            params.unshift(fId.idB);
            return A['s-lam'].app( 
              l,
              "",
              runtime.ffi.makeList([]),
              runtime.ffi.makeList(params),
              A['a-blank'],
              "",
              A['s-app'].app(l, fId.idE, runtime.ffi.makeList(argsRight)),
              runtime.ffi.makeNone(),
              runtime.ffi.makeNone(),
              false
            );
          } else {
            const dsF = map(visitor, f);
            if (params.length === 0) { // f(args)
              return A['s-app'].app(
                l, dsF, runtime.ffi.makeList(args)
              );
            } else { // f(args, _, args)
              return A['s-lam'].app(
                l,
                "",
                runtime.ffi.makeList([]),
                runtime.ffi.makeList(params),
                A['a-blank'],
                "",
                A['s-app'].app(l, dsF, runtime.ffi.makeList(argsRight)),
                runtime.ffi.makeNone(),
                runtime.ffi.makeNone(),
                false
              );
            }
          }
        }
      }
      const dsVisitor = {
        generatedBinds,
        // s-module is uniform
        // s-instantiate is uniform
        // s-block is uniform
        's-user-block': (visitor, expr: TJ.Variant<A.Expr, 's-user-block'>) => {
          return map(visitor, expr.dict.body);
        },
        // s-template is uniform
        's-app': (visitor, expr: TJ.Variant<A.Expr, 's-app'>) => {
          return dsCurry(
            expr.dict.l, 
            expr.dict._fun, 
            listToArray(expr.dict.args).map(arg => map(visitor, arg)),
            visitor
          );
        },
        // s-prim-app is uniform
        // s-lam is uniform
        // s-method is uniform
        // s-type is uniform
        // s-newtype is uniform
        // s-type-let-expr is uniform
        // s-let-expr is uniform
        // s-letrec is uniform
        // s-data-expr is uniform
        's-when': (visitor, expr: TJ.Variant<A.Expr, 's-when'>) => {
          const dsTest = map(visitor, expr.dict.test);
          const nothing = gid(expr.dict.l, "nothing");
          const dsBody = map(visitor, expr.dict.block);
          let bodyWithNothing;
          let stmts;
          if (dsBody.$name === 's-block') {
            stmts = listToArray(dsBody.dict.stmts);
            stmts.push(nothing);
          } else {
            stmts = [dsBody, nothing]
          }
          bodyWithNothing = A['s-block'].app(
            expr.dict.l, 
            runtime.ffi.makeList(stmts),
          );
          return A['s-if-else'].app(
            expr.dict.l,
            runtime.ffi.makeList([
              A['s-if-branch'].app(expr.dict.l, dsTest, bodyWithNothing)
            ]), 
            A['s-block'].app(expr.dict.l, runtime.ffi.makeList([nothing])),
            expr.dict.blocky,
          );
        },
        's-if': (visitor, expr : TJ.Variant<A.Expr, 's-if'>) => {
          const l = expr.dict.l;
          const noElse = A['s-block'].app(l, runtime.ffi.makeList([noBranchesExn(l, "if")]));
          return desugarIf(l, expr.dict.branches, noElse, expr.dict.blocky, visitor);
        },
        's-if-else': (visitor, expr: TJ.Variant<A.Expr, 's-if-else'>) => {
          return desugarIf(expr.dict.l, expr.dict.branches, expr.dict._else, expr.dict.blocky, visitor);
        },
        's-if-pipe': (visitor, expr: TJ.Variant<A.Expr, 's-if-pipe'>) => {
          const l = expr.dict.l;
          const noOtherwise = A['s-block'].app(l, runtime.ffi.makeList([noBranchesExn(l, "ask")]));
          return desugarIf(l, expr.dict.branches, noOtherwise, expr.dict.blocky, visitor);
        },
        's-if-pipe-else': (visitor, expr: TJ.Variant<A.Expr, 's-if-pipe-else'>) => {
          return desugarIf(expr.dict.l, expr.dict.branches, expr.dict._else, expr.dict.blocky, visitor);
        },
        // s-cases is uniform
        // s-cases-else is uniform
        // s-assign is uniform
        's-dot': (visitor, expr: TJ.Variant<A.Expr, 's-dot'>) => {
          return dsCurryNullary(A['s-dot'].app, expr.dict.l, expr.dict.obj, expr.dict.field, visitor);
        },
        's-bracket': (visitor, expr: TJ.Variant<A.Expr, 's-bracket'>) => {
          return dsCurryBinop(expr.dict.l, map(visitor, expr.dict.obj), map(visitor, expr.dict.key), (e1: A.Expr, e2: A.Expr) => A['s-bracket'].app(expr.dict.l, e1, e2));
        },
        's-get-bang': (visitor, expr: TJ.Variant<A.Expr, 's-get-bang'>) => {
          return dsCurryNullary(A['s-get-bang'].app, expr.dict.l, expr.dict.obj, expr.dict.field, visitor);
        },
        's-update': (visitor, expr: TJ.Variant<A.Expr, 's-update'>) => {
          const dsFields = runtime.ffi.makeList(listToArray(expr.dict.fields).map(field => map(visitor, field)));
          return dsCurryNullary(A['s-update'].app, expr.dict.l, expr.dict.supe, dsFields, visitor);
        },
        's-extend': (visitor, expr: TJ.Variant<A.Expr, 's-extend'>) => {
          const dsFields = runtime.ffi.makeList(listToArray(expr.dict.fields).map(field => map(visitor, field)));
          return dsCurryNullary(A['s-extend'].app, expr.dict.l, expr.dict.supe, dsFields, visitor);
        },
        // s-for is uniform
        // s-op is uniform
        // s-id is uniform
        // s-id-modref is uniform
        // s-id-var-modref is uniform
        // s-id-var is uniform
        // s-id-letrec is uniform
        // s-srcloc is uniform
        // s-num is uniform
        // s-frac is uniform
        // s-rfrac is uniform
        // s-str is uniform
        // s-bool is uniform
        // s-obj is uniform
        // s-tuple is uniform
        // s-tuple-get is uniform
        // s-ref is uniform
        // s-construct in uniform
        's-reactor': (visitor, expr: TJ.Variant<A.Expr, 's-reactor'>) => {
          const fieldsByName = new Map<string, A.Expr>();
          listToArray(expr.dict.fields).forEach(field => {
            if ("value" in field.dict) {
              fieldsByName.set(field.dict.name, field.dict.value);
            }
          });
          const optionFields: TJ.Variant<A.Member, "s-data-field">[] = [];
          for (const [key, value] of reactorOptionalFields) {
            if (fieldsByName.has(key)) {
              const thisField = fieldsByName.get(key);
              const thisFieldL = thisField.dict.l;
              optionFields.push(A['s-data-field'].app(
                thisFieldL,
                key,
                A['s-prim-app'].app(thisFieldL, "makeSome",
                  runtime.ffi.makeList([A['s-check-expr'].app(thisFieldL, map(visitor, thisField), value(thisFieldL))]),
                  flatPrimApp
                )
              ));
            } else {
              optionFields.push(A['s-data-field'].app(
                expr.dict.l,
                key,
                A['s-prim-app'].app(expr.dict.l, "makeNone", runtime.ffi.makeList([]), flatPrimApp)
              ));
            }
          }
          return A['s-prim-app'].app(
            expr.dict.l,
            "makeReactor",
            runtime.ffi.makeList([
              map(visitor, fieldsByName.get("init")),
              A['s-obj'].app(expr.dict.l, runtime.ffi.makeList(optionFields))]),
            flatPrimApp);
        },
        // s-table is uniform
        's-paren': (visitor, expr: TJ.Variant<A.Expr, 's-paren'>) => {
          return map(visitor, expr.dict.expr);
        },
        's-let': (visitor, expr: TJ.Variant<A.Expr, 's-let'>) => {
          throw new InternalCompilerError('s-let should have already been desugared');
        },
        's-var': (visitor, expr: TJ.Variant<A.Expr, 's-var'>) => {
          throw new InternalCompilerError('s-var should have already been desugared');
        },
        // s-check is uniform
        // s-check-test is uniform
        // s-load-table is uniform
        // s-table-extend is uniform
        // s-table-update is uniform
        // s-table-select is uniform
        // s-table-extract is uniform
        // s-table-order is uniform
        // s-table-filter is uniform
        // s-spy-block is uniform
        's-tuple-bind': (visitor, expr: TJ.Variant<A.Bind, 's-tuple-bind'>) => {
          throw new InternalCompilerError("s-tuple-bind should not appear in desugar: " + JSON.stringify(expr));
        },
        's-mutable-field': (visitor, expr: TJ.Variant<A.Member, 's-mutable-field'>) => {
          throw new InternalCompilerError("s-mutable-field desugar not yet implemented: " + JSON.stringify(expr));
        },
        'a-checked': (visitor, expr: TJ.Variant<A.Ann, 'a-checked'>) => {
          throw new InternalCompilerError("a-checked should not appear in desugar");
        }
      };
      generatedBinds.clear();
      const desugared = map(dsVisitor, program);
      return runtime.makeObject({
        ast: desugared,
        'new-binds': mutableStringDictFromMap(generatedBinds),
      });
    }
    return runtime.makeModuleReturn({
      'desugar': runtime.makeFunction(desugar)
    }, {});
  }
})