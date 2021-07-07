import type * as TS from './ts-type-structs';
import type * as A from './ts-ast';
import type * as CS from './ts-compile-structs';
import type * as TJ from './ts-codegen-helpers';
import type { List, MutableStringDict, PFunction, StringDict } from './ts-impl-types';

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
  theModule: function(runtime, _, __, SD: SDExports, tj : TJ.Exports, TS : (TS.Exports), A : (A.Exports), CS : (CS.Exports)) {
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
    function desugar(program: A.Program, options): DesugarInfo {
      const {
        listToArray,
        map,
        nameToKey,
        MakeName,
        ExhaustiveSwitchError
      } = tj;

      const names = MakeName(0);
      const generatedBinds = SD.dict.values.dict['make-mutable-string-dict'].app<CS.ValueBind>();
      const flatPrimApp = A.dict.values.dict['prim-app-info-c'].app(false);
  
      function boLocal(l: A.Srcloc, originalName: A.Name) {
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
        return A.dict.values.dict['s-global'].app(id);
      }
      function gid(l : A.Srcloc, id: string): TJ.Variant<A.Expr, 's-id'> {
        return A.dict.values.dict['s-id'].app(l, g(id));
      }
      function noBranchesExn(l: A.Srcloc, typ: string) {
        const srcloc = A.dict.values.dict['s-srcloc'].app(l, l);
        const str = A.dict.values.dict['s-str'].app(l, typ);
        return A.dict.values.dict['s-prim-app'].app(l, "throwNoBranchesMatched", runtime.ffi.makeList([srcloc, str]), flatPrimApp);
      }
      function mkIdAnn(l: A.Srcloc, base: string, ann: A.Ann): { id: A.Name, idB: A.Bind, idE: A.Expr } {
        const a = names.makeAtom(base);
        generatedBinds.dict['set-now'].full_meth(
          generatedBinds, 
          nameToKey(a), 
          CS.dict.values.dict['value-bind'].app(
            boLocal(l, a),
            CS.dict.values.dict['vb-let'],
            a,
            ann
          )
        );
        return { 
          id: a, 
          idB: A.dict.values.dict['s-bind'].app(l, false, a, ann), 
          idE: A.dict.values.dict['s-id'].app(l, a),
        };
      }
      function mkId(l: A.Srcloc, base: string) {
        return mkIdAnn(l, base, A.dict.values.dict['a-blank']);
      }
      function desugarIf(l: A.Srcloc, branches: List<TJ.Variant<A.IfBranch, 's-if-branch'> | TJ.Variant<A.IfPipeBranch, 's-if-pipe-branch'>>, _else: A.Expr, blocky: boolean, visitor) {
        let dsElse = map(visitor, _else);
        return listToArray(branches).reduceRight((acc, branch) => {
          const dsTest = map(visitor, branch.dict.test);
          const dsBody = map(visitor, branch.dict.body);
          return A.dict.values.dict['s-if-else'].app(
            l,
            runtime.ffi.makeList([A.dict.values.dict['s-if-branch'].app(
              branch.dict.l,
              dsTest,
              dsBody)]),
            acc,
            blocky);
        }, dsElse);
      }
      function isUnderscore(e: A.Expr) {
        return A.dict.values.dict['is-s-id'].app(e) && A.dict.values.dict['is-s-underscore'].app(e);
      }
      function dsCurryArgs(l: A.Srcloc, args: List<A.Expr>) {
        const binds: Array<A.Bind> = [];
        const exprs: Array<A.Expr> = [];
        listToArray(args).forEach(arg => {
          if (isUnderscore(arg)) {
            const argId = mkId(l, "arg_");
            binds.push(argId.idB);
            exprs.push(argId.idE);
          } else {
            exprs.push(arg);
          }
        });
        const ret = [];
        ret.push(binds);
        ret.push(exprs);
        return ret;
      }
      function dsCurry(l: A.Srcloc, f: A.Expr, args: List<A.Expr>, visitor) {
        function fallthrough() {
          const paramsAndArgs = dsCurryArgs(l, args);
          const params: Array<A.Bind> = paramsAndArgs[0];
          const argsRight: Array<A.Expr> = paramsAndArgs[1];
          if (isUnderscore(f)) {
            const fId = mkId(l, "f_");
            params.unshift(fId.idB);
            return A.dict.values.dict['s-lam'].app(
              l,
              "",
              runtime.ffi.makeList([]),
              runtime.ffi.makeList(params),
              A.dict.values.dict['a-blank'],
              "",
              A.dict.values.dict['s-app'].app(l, fId.idE, runtime.ffi.makeList(argsRight)),
              runtime.ffi.makeNone(),
              runtime.ffi.makeNone(),
              false
            );
          } else {
            const dsF = map(visitor, f);
            if (params.length === 0) {
              return A.dict.values.dict['s-app'].app(
                l, dsF, args
              );
            } else {
              return A.dict.values.dict['s-lam'].app(
                l,
                "",
                runtime.ffi.makeList([]),
                runtime.ffi.makeList(params),
                A.dict.values.dict['a-blank'],
                "",
                A.dict.values.dict['s-app'].app(l, dsF, runtime.ffi.makeList(argsRight)),
                runtime.ffi.makeNone(),
                runtime.ffi.makeNone(),
                false
              );
            }
          }
        }
        switch (f.$name) {
          case 's-dot': {
            if (isUnderscore(f.dict.obj)) {
              const curriedObj = mkId(l, "recv_");
              const paramsAndArgs = dsCurryArgs(l, args);
              const params: Array<A.Bind> = paramsAndArgs[0];
              params.unshift(curriedObj.idB);
              const argsRight: Array<A.Expr> = paramsAndArgs[1];
              return A.dict.values.dict['s-lam'].app(
                l,
                "",
                runtime.ffi.makeList([]),
                runtime.ffi.makeList(params),
                A.dict.values.dict['a-blank'],
                "",
                A.dict.values.dict['s-app'].app(
                  l,
                  A.dict.values.dict['s-dot'].app(
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
              fallthrough();
            }
          }
          default: {
            fallthrough();
          }
        }
      }
      // options.dict.log.app("Hi from ts-desugar-impl!\n", runtime.ffi.makeNone());
      const dsVisitor = {
        generatedBinds,
        // s-module is uniform
        // s-instantiate is uniform
        // s-block is uniform
        's-user-block': (visitor, expr: TJ.Variant<A.Expr, 's-user-block'>) => {
          return map(visitor, expr.dict.body);
        },
        's-template': (visitor, expr: TJ.Variant<A.Expr, 's-template'>) => {
          return expr;
        },
        /*'s-app': (visitor, expr: TJ.Variant<A.Expr, 's-app'>) => {
          return dsCurry(
            expr.dict.l, 
            expr.dict._fun, 
            runtime.ffi.makeList(listToArray(expr.dict.args).map(arg => map(visitor, arg))),
            visitor
          );
        },*/
        's-when': (visitor, expr: TJ.Variant<A.Expr, 's-when'>) => {
          options.dict.log.app("In s-when!\n", runtime.ffi.makeNone());
          
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
          bodyWithNothing = A.dict.values.dict['s-block'].app(
            expr.dict.l, 
            runtime.ffi.makeList(stmts),
          );
          return A.dict.values.dict['s-if-else'].app(
            expr.dict.l,
            runtime.ffi.makeList([
              A.dict.values.dict['s-if-branch'].app(expr.dict.l, dsTest, bodyWithNothing)
            ]), 
            A.dict.values.dict['s-block'].app(expr.dict.l, runtime.ffi.makeList([nothing])),
            expr.dict.blocky,
          );
        },
        's-if': (visitor, expr : TJ.Variant<A.Expr, 's-if'>) => {
          const l = expr.dict.l;
          const noElse = A.dict.values.dict['s-block'].app(l, runtime.ffi.makeList([noBranchesExn(l, "if")]));
          return desugarIf(l, expr.dict.branches, noElse, expr.dict.blocky, visitor);
        },
        's-if-else': (visitor, expr: TJ.Variant<A.Expr, 's-if-else'>) => {
          return desugarIf(expr.dict.l, expr.dict.branches, expr.dict._else, expr.dict.blocky, visitor);
        },
        's-if-pipe': (visitor, expr: TJ.Variant<A.Expr, 's-if-pipe'>) => {
          const l = expr.dict.l;
          const noOtherwise = A.dict.values.dict['s-block'].app(l, runtime.ffi.makeList([noBranchesExn(l, "ask")]));
          return desugarIf(l, expr.dict.branches, noOtherwise, expr.dict.blocky, visitor);
        },
        's-if-pipe-else': (visitor, expr: TJ.Variant<A.Expr, 's-if-pipe-else'>) => {
          return desugarIf(expr.dict.l, expr.dict.branches, expr.dict._else, expr.dict.blocky, visitor);
        },
        // s-cases is uniform
        // s-cases-else is uniform
        // s-assign is uniform
      };
      const desugared = map(dsVisitor, program);
      return runtime.makeObject({
        ast: desugared,
        'new-binds': generatedBinds,
      });
    }
    return runtime.makeModuleReturn({
      'desugar': runtime.makeFunction(desugar)
    }, {});
  }
})