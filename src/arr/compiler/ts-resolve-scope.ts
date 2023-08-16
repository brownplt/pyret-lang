import type * as TS from './ts-type-structs';
import type * as A from './ts-ast';
import type * as CS from './ts-compile-structs';
import type * as AU from './ts-ast-util';
import type * as TJ from './ts-codegen-helpers';
import type { List, MutableStringDict, PFunction, StringDict, PMethod, Runtime } from './ts-impl-types';

export type Exports = {
  dict: {
    values: {
      dict: {
        'desugar-scope': PFunction<(program: A.Program, env: CS.CompileEnvironment) => CS.ScopeResolution>
        'resolve-names': PFunction<(program: A.Program, thismodulesUri: string, initialEnv: CS.CompileEnvironment) => CS.NameResolution>
      }
    }
  }
}

({
  requires: [
    { 'import-type': 'dependency', protocol: 'js-file', args: ['ts-codegen-helpers']},
    { 'import-type': 'dependency', protocol: 'file', args: ['type-structs.arr']},
    { 'import-type': 'dependency', protocol: 'file', args: ['ast.arr']},
    { 'import-type': 'dependency', protocol: 'file', args: ['compile-structs.arr']},
    { 'import-type': 'dependency', protocol: 'js-file', args: ['ts-ast-util']},
 ],
  nativeRequires: [],
  provides: {
    values: {
      "desugar-scope": "tany",
      "resolve-names": "tany"
    }
  },
  theModule: function(runtime: Runtime, _, __, tj : TJ.Exports, TS : (TS.Exports), Ain : (A.Exports), CSin : (CS.Exports), AUin : (AU.Exports)) {
    const A = Ain.dict.values.dict;
    const AU = AUin.dict.values.dict;
    const CS = CSin.dict.values.dict;
    const {
      listToArray,
      InternalCompilerError
    } = tj;

    // NOTE(joe/ben Aug 2023): This is a global that is referred to and reset on each call to
    // resolve scope.
    let errors : Array<CS.CompileError>;

    function desugarToplevelTypes(stmts : Array<A.Expr>) : Array<A.Expr> {
      return [];
    }

    type Contract = TJ.Variant<A.Expr, 's-contract'>;

    type BindingGroup =
      | [ 'let-binds', Contract[], A.LetBind[] ]
      | [ 'letrec-binds', Contract[], A.LetrecBind[] ]
      | [ 'type-let-binds', A.TypeLetBind[] ]
    
    type DesugarVisitor = TJ.Visitor<A.Expr, A.Expr> & TJ.Visitor<A.CasesBranch, A.CasesBranch> & TJ.Visitor<A.Member, A.Member>;

    function desugarScopeBlock(stmts: A.Expr[], bindingGroup : BindingGroup) : A.Expr {
      return undefined as any;
    }

    const desugarScopeVisitor : DesugarVisitor = {
      's-block': function(self, e) {
        const { l, stmts } = e.dict;
        const newStmts = listToArray(stmts).map((s : A.Expr) => tj.map(self, s));
        return desugarScopeBlock(newStmts, [ 'let-binds', [], [] ]);
      },
      's-let-expr': function(self, e) {
        return undefined as unknown as A.Expr;
      },
      's-for': function(self, e) {
        return undefined as unknown as A.Expr;
      },
      's-cases-branch': function(self, e) {
        return undefined as unknown as A.CasesBranch;
      },
      's-fun': function(self, e) {
        return undefined as unknown as A.Expr;
      },
      's-lam': function(self, e) {
        return undefined as unknown as A.Expr;
      },
      's-method-field': function(self, e) {
        return undefined as unknown as A.Member;
      }
    };

    /**
       Remove x = e, var x = e, tuple bindings, and fun f(): e end
       and turn them into explicit let and letrec expressions.
       Do this recursively through the whole program.
       Preconditions on prog:
         - well-formed
       Postconditions on prog:
         - contains no s-provide in headers
         - contains no s-let, s-var, s-data, s-tuple-bind
     */
    function desugarScope(program: A.Program, env: CS.CompileEnvironment): CS.ScopeResolution {
      switch(program.$name) {
        case 's-program': {
          const { l, '_use' : _useRaw, '_provide' : _provideRaw, 'provided-types' : provideTypesRaw, provides, imports: importsRaw, block : body} = program.dict;
          const str = function(s : string) { return A['s-str'].app(l, s); }

          let withImports : A.Expr;
          switch(body.$name) {
            case 's-block': {
              const asArray = runtime.ffi.makeList(desugarToplevelTypes(listToArray(body.dict.stmts)))
              withImports = A['s-block'].app(l, asArray);
              break;
            }
            default: {
              const asArray = runtime.ffi.makeList(desugarToplevelTypes([body]))
              withImports = A['s-block'].app(l, asArray);
            }
          }
          const empty = runtime.ffi.makeList([]);

          function transformToplevelLast(l2 : A.Srcloc, last : A.Expr) : A.Expr {
            const checkers = A['s-dot'].app(l2, AU.checkers.app(l2), "results");
            return A['s-module'].app(l2, last, empty, empty, empty, A['s-app'].app(l2, checkers, empty));
          }

          let withProvides : TJ.Variant<A.Expr, 's-block'>;
          switch(withImports.$name) {
            case 's-block': {
              const { l : l2, stmts } = withImports.dict;
              const stmtsArray = listToArray(stmts);
              const stmtsFront = stmtsArray.slice(0, stmtsArray.length - 1);
              const last = stmtsArray[stmtsArray.length - 1];
              switch(last.$name) {
                case 's-type-let-expr': {
                  const { l : l3, binds, body : body2, blocky } = last.dict;
                  const innerLastArray = listToArray((body2 as TJ.Variant<A.Expr, 's-block'>).dict.stmts);
                  const innerFront = innerLastArray.slice(0, innerLastArray.length - 1);
                  const innerLast = innerLastArray[innerLastArray.length - 1];
                  const innerTransformed = [...innerFront, transformToplevelLast(l3, innerLast)];
                  const newTypeLet = A['s-type-let-expr'].app(l3, binds,
                    A['s-block'].app(body2.dict.l, runtime.ffi.makeList(innerTransformed)), true);
                  const newBlock = A['s-block'].app(l2, runtime.ffi.makeList([...stmtsFront, newTypeLet]))
                  withProvides = newBlock;
                  break;
                }
                default: {
                  withProvides = A['s-block'].app(l2, runtime.ffi.makeList([...stmtsFront, transformToplevelLast(l2, last)]))
                  break;
                }
              }
              break;
            }
            default: {
              throw new InternalCompilerError("Impossible");
            }

          }
          errors = [];

          // NOTE(joe/ben Aug 2023): This next line seems unnecessary, but copying from
          // original Pyret code faithfully (it's just recreating the same block)
          const recombined = A['s-block'].app(withProvides.dict.l, withProvides.dict.stmts);
          const visited = tj.map(desugarScopeVisitor, recombined);
          return CS['resolved-scope'].app(
            A['s-program'].app(
              l, _useRaw, _provideRaw, provideTypesRaw, provides, importsRaw, visited
            ),
            runtime.ffi.makeList(errors)
          );
        }
      }
    }
    function resolveNames(program: A.Program, thismoduleURI: string, initialEnv: CS.CompileEnvironment): CS.NameResolution {
      return undefined as any;
    }
    const exports: Exports['dict']['values']['dict'] = {
      'desugar-scope': runtime.makeFunction(desugarScope),
      'resolve-names': runtime.makeFunction(resolveNames)
    };
    return runtime.makeModuleReturn(exports, {});
  }
})