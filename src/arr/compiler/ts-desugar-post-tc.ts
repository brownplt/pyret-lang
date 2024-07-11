import type * as TJ from './ts-codegen-helpers';
import type * as A from './ts-ast';
import type * as C from './ts-compile-structs'
import type { PFunction } from './ts-impl-types';
import { CompileOptions } from './ts-compiler-lib-impl';

export interface Exports {
  dict: {
    values: {
      dict: {
        'desugar-post-tc': PFunction<(prog: A.Program, compileEnv: C.CompileEnvironment, options: CompileOptions) => A.Program>,
      }
    }
  }
}

({
  requires: [
    { 'import-type': 'dependency', protocol: 'js-file', args: ['ts-codegen-helpers']},
    { 'import-type': 'dependency', protocol: 'file', args: ['ast.arr']},
  ],
  provides: {
    values: {
      'desugar-post-tc': "tany",
    }
  },
  nativeRequires: [],
  theModule: function(runtime, _, __, tj : TJ.Exports, Ain : A.Exports) {
    const A  = Ain.dict.values.dict;
    const { listToArray, map } = tj;

    // Note(Ben): we need the actual name-maker from ast.arr to ensure we don't duplicate serial numbers
    // but we might be able to eliminate this by making the generated names be non-Pyret-legal
    const names = A['global-names'].dict;
    const flatPrimApp = A['prim-app-info-c'].app(false);

    function noCasesExn(l: A.Srcloc, val: A.Expr): TJ.Variant<A.Expr, 's-prim-app'> {
      return A['s-prim-app'].app(l, 
        "throwNoCaseesMatched", 
        runtime.ffi.makeList([
          A['s-srcloc'].app(l, l), 
          val]
        ), 
        flatPrimApp);
    }

    /**
      Desugar non-scope and non-check based constructs.

      Preconditions on program:
        - well-formed
        - has been type-checked
        - contains no s-var, s-fun, s-data, s-check, or s-check-test
        - contains no s-provide in headers
        - all where blocks are none
        - contains no s-name (e.g. call resolve-names first)
        - contains no s-for, s-if, s-op, s-method-field,
                      s-not, s-when, s-if-pipe, s-paren
        - contains no s-underscore in expression position (but it may
          appear in binding positions as in s-let-bind, s-letrec-bind)

      Postconditions on program:
        - in addition to preconditions,
          contains no s-cases, s-cases-else, s-instantiate
    */
    function desugarPostTc(prog: A.Program, _compileEnv: C.CompileEnvironment, _options: CompileOptions): A.Program {
      return map<A.Program | A.Expr | A.Ann | A.CasesBranch, A.Program>({
        's-template': (_visitor, template) => {
          const l = template.dict.l;
          return A['s-prim-app'].app(l, 
            "throwUnfinishedTemplate", runtime.ffi.makeList([A['s-srcloc'].app(l, l)]), flatPrimApp);
        },
        's-cases-else': (visitor, cases: TJ.Variant<A.Expr, 's-cases-else'>) => {
          const { l, typ, val, _else } = cases.dict;
          const branches = listToArray(cases.dict.branches);
          const name = names['make-atom'].app("cases")
          const typCompiled = map<A.Ann>(visitor, typ);

          const valExp = map<A.Expr>(visitor, val);
          const valId = A['s-id'].app(l, name);
          const branchesCompiled = branches.map((b) => map<A.CasesBranch>(visitor, b));
          const elsCompiled = map<A.Expr>(visitor, _else);

          return A['s-let-expr'].app(l,
            runtime.ffi.makeList([
              A['s-let-bind'].app(l, 
                A['s-bind'].app(l, false, name, typCompiled),
                valExp)]),
            A['s-cases-else'].app(l, typCompiled, valId, runtime.ffi.makeList(branchesCompiled),
              elsCompiled, true), false);
        },
        's-cases': (visitor, cases: TJ.Variant<A.Expr, 's-cases'>) => {
          const { l, typ, val } = cases.dict;
          const branches = listToArray(cases.dict.branches);
          const name = names['make-atom'].app("cases")
          const typCompiled = map<A.Ann>(visitor, typ);

          const valExp = map<A.Expr>(visitor, val);
          const valId = A['s-id'].app(l, name);
          const branchesCompiled = branches.map((b) => map<A.CasesBranch>(visitor, b));

          return A['s-let-expr'].app(l,
            runtime.ffi.makeList([
              A['s-let-bind'].app(l, 
                A['s-bind'].app(l, false, name, typCompiled),
                valExp)]),
            A['s-cases-else'].app(l, typCompiled, valId, runtime.ffi.makeList(branchesCompiled),
              A['s-block'].app(l, runtime.ffi.makeList([noCasesExn(l, valId)])), true), false);
        },
        // NOTE(alex): Remove these comments to drop check blocks
        /*
        's-check': (_visitor, check) => {
          return A['s-id'].app(check.dict.l, A['s-global'].app("nothing"));
        }
        */
      }, prog);
    }

    const exports : Exports['dict']['values']['dict'] = {
      'desugar-post-tc': runtime.makeFunction(desugarPostTc),
    };
    return runtime.makeModuleReturn(exports, {});
  }
})