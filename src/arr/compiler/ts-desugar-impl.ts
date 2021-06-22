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
        ExhaustiveSwitchError,
        InternalCompilerError,
        listToArray,
        nameToKey,
        map,
      } = tj;
  
      function g(id: string): TJ.Variant<A.Name, 's-global'> {
        return A.dict.values.dict['s-global'].app(id);
      }
      function gid(l : A.Srcloc, id: string): TJ.Variant<A.Expr, 's-id'> {
        return A.dict.values.dict['s-id'].app(l, g(id));
      }
      // options.dict.log.app("Hi from ts-desugar-impl!\n", runtime.ffi.makeNone());
      const generatedBinds = SD.dict.values.dict['make-mutable-string-dict'].app<CS.ValueBind>();
      const dsVisitor = {
        generatedBinds,
        's-when': (visitor, expr : TJ.Variant<A.Expr, 's-when'>) => {
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
        }
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