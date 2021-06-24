import type * as TS from './ts-type-structs';
import type * as A from './ts-ast';
import type * as CS from './ts-compile-structs';
import type * as TJ from './ts-codegen-helpers';
import type * as TCS from './ts-type-check-structs';
import type * as TCSH from './ts-compile-structs-helpers';
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

({
  requires: [
    { 'import-type': 'builtin', name: 'string-dict' },
    { 'import-type': 'dependency', protocol: 'js-file', args: ['ts-codegen-helpers']},
    { 'import-type': 'dependency', protocol: 'js-file', args: ['ts-compile-structs-helpers']},
    { 'import-type': 'dependency', protocol: 'file', args: ['type-structs.arr']},
    { 'import-type': 'dependency', protocol: 'file', args: ['ast.arr']},
    { 'import-type': 'dependency', protocol: 'file', args: ['compile-structs.arr']},
    { 'import-type': 'dependency', protocol: 'file', args: ['type-check-structs.arr']},
    { 'import-type': 'dependency', protocol: 'file', args: ['type-defaults.arr']},
 ],
  nativeRequires: ["escodegen", "path"],
  provides: {
    values: {
      "type-check": "tany"
    }
  },
  theModule: function(runtime, _, __, SD: SDExports, tj : TJ.Exports, TCSH : (TCSH.Exports), TS : (TS.Exports), A : (A.Exports), CS : (CS.Exports), TCS : (TCS.Exports)) {
    const {
      ExhaustiveSwitchError,
      InternalCompilerError,
      listToArray,
      nameToKey,
    } = tj;
    const { globalValueValue } = TCSH.dict.values.dict;
    const { ok } = CS.dict.values.dict;
    const { 's-global': sGlobal } = A.dict.values.dict;
    const { 
      typed,
      'tc-info': tcInfo,
      'empty-info': emptyInfo,
      'empty-context': emptyContext,
      'fold-result': foldResult,
      'fold-errors': foldErrors,
    } = TCS.dict.values.dict;
    function foldrFoldResult<X, Y>(f : (x: X, context: TCS.Context, acc: Y) => TCS.FoldResult<Y>, xs: X[], context: TCS.Context, base: Y): TCS.FoldResult<Y> {
      return xs.reduceRight((prev: TCS.FoldResult<Y>, cur: X): TCS.FoldResult<Y> => {
        switch(prev.$name) {
          case 'fold-errors': return prev;
          case 'fold-result': {
            return f(cur, prev.dict.context, prev.dict.v);
          }
          default: throw new ExhaustiveSwitchError(prev);
        }
      }, foldResult.app(base, context));
    }
    function setInferred(typ: TS.Type, inferred: boolean): TS.Type {
      return {
        ...typ,
        dict: {
          ...typ.dict,
          inferred
        }
      } as TS.Type;
    }
    function gatherProvides(provide: A.ProvideBlock, context: TCS.Context): TCS.TCInfo {
      switch(provide.$name) {
        case 's-provide-block': {
          const curTypes = SD.dict.values.dict['make-mutable-string-dict'].app<TS.Type>();
          const curAliases = context.dict.info.dict.aliases.dict.unfreeze.full_meth(context.dict.info.dict.aliases);
          const curData = context.dict.info.dict['data-types'].dict.unfreeze.full_meth(context.dict.info.dict['data-types']);
          // Note(Ben): I'm doing two things differently than the original Pyret code:
          // 1. I'm traversing the list of specs from first to last.  If this ultimately matters,
          //    we could reverse the array on the next line before traversing it.
          // 2. I'm mutably updatng the three dictionaries object above, rather than functionally 
          //    folding over a `TCInfo` object.  Since the original code never produced `fold-errors`
          //    objects, it's not necessary to mimic all of foldr-fold-result here.
          for (const spec of listToArray(provide.dict.specs)) {
            switch(spec.$name) {
              case 's-provide-name': {
                const nameSpec = spec.dict['name-spec'];
                switch(nameSpec.$name) {
                  case 's-local-ref': {
                    const valueKey = nameToKey(nameSpec.dict.name);
                    if (curTypes.dict['has-key-now'].full_meth(curTypes, valueKey)) {
                      break; // nothing more to do
                    } else {
                      // MARK(joe): test as-name here; it appears unused
                      const getValueFromContext = context.dict.info.dict.types.dict.get.full_meth(context.dict.info.dict.types, valueKey);
                      switch(getValueFromContext.$name) {
                        case 'some': {
                          const typ = setInferred(getValueFromContext.dict.value, false);
                          curTypes.dict['set-now'].full_meth(curTypes, valueKey, typ);
                          break;
                        }
                        case 'none': {
                          const typ = setInferred(context.dict['global-types'].dict['get-value'].full_meth(context.dict['global-types'], valueKey), false);
                          curTypes.dict['set-now'].full_meth(curTypes, valueKey, typ);
                          break;
                        }
                        default: throw new ExhaustiveSwitchError(getValueFromContext);
                      }
                    }
                  }
                  case 's-remote-ref': break;
                  case 's-module-ref':
                  case 's-star': throw new InternalCompilerError(`Unexpected require spec type ${spec.$name} / ${nameSpec.$name}`);
                  default: throw new ExhaustiveSwitchError(nameSpec);
                }
                break;
              }
              case 's-provide-type': {
                const nameSpec = spec.dict['name-spec'];
                switch(nameSpec.$name) {
                  case 's-local-ref': {
                    const aliasKey = nameToKey(nameSpec.dict.name);
                    if (curAliases.dict['has-key-now'].full_meth(curAliases, aliasKey)) {
                      break; // nothing to do
                    } else {
                      const typ = context.dict.aliases.dict['get-value'].full_meth(context.dict.aliases, aliasKey);
                      curAliases.dict['set-now'].full_meth(curAliases, aliasKey, typ);
                      break;
                    }
                  }
                  case 's-remote-ref': break;
                  case 's-module-ref':
                  case 's-star': throw new InternalCompilerError(`Unexpected require spec type ${spec.$name} / ${nameSpec.$name}`);
                  default: throw new ExhaustiveSwitchError(nameSpec);
                }
                break;
              }
              case 's-provide-module': {
                break; // nothing to do here
              }
              case 's-provide-data': {
                const nameSpec = spec.dict['name-spec'];
                switch(nameSpec.$name) {
                  case 's-local-ref': {
                    const dataKey = nameToKey(nameSpec.dict.name);
                    if (curData.dict['has-key-now'].full_meth(curData, dataKey)) {
                      break; // nothing to do
                    } else {
                      const typ = context.dict['data-types'].dict['get-value'].full_meth(context.dict['data-types'], dataKey);
                      curData.dict['set-now'].full_meth(curData, dataKey, typ);
                      break;
                    }
                  }
                  case 's-remote-ref': break;
                  case 's-module-ref':
                  case 's-star': throw new InternalCompilerError(`Unexpected require spec type ${spec.$name} / ${nameSpec.$name}`);
                  default: throw new ExhaustiveSwitchError(nameSpec);
                }
                break;
              }
              default: throw new ExhaustiveSwitchError(spec);
            }
          }
          return tcInfo.app(
            curTypes.dict.freeze.full_meth(curTypes),
            curAliases.dict.freeze.full_meth(curAliases),
            curData.dict.freeze.full_meth(curData));
        }
        default: throw new ExhaustiveSwitchError(provide.$name);
      }
    }
    function typeCheck(program: A.Program, compileEnv : CS.CompileEnvironment, postCompileEnv, modules, options) {
      // DEMO output: options.dict.log.app("Hi!", runtime.ffi.makeNone());
      const provides = listToArray(program.dict.provides);
      let context = emptyContext.app();

      const globVs = compileEnv.dict.globals.dict.values;
      const globTs = compileEnv.dict.globals.dict.types;

      const contextGlobTs = context.dict['aliases'].dict.unfreeze.full_meth(context.dict['aliases']);
      const contextGlobVs = context.dict['global-types'].dict.unfreeze.full_meth(context.dict['global-types']);

      for (const g of listToArray(globVs.dict['keys-list'].full_meth(globVs))) {
        const key = nameToKey(sGlobal.app(g));
        if (contextGlobVs.dict['has-key-now'].full_meth(contextGlobVs, key)) {
          continue;
        }
        else {
          if(g === "_") {
            continue;
          }
          else {
            const ve =  globalValueValue(compileEnv, g);
            contextGlobVs.dict['set-now'].full_meth(contextGlobVs, key, ve.dict.t);
          }
        }
      }

      const info = gatherProvides(provides[0], context);
      return ok.app(typed.app(program, info));
    }
    return runtime.makeModuleReturn({
      'type-check': runtime.makeFunction(typeCheck)
    }, {});
  }
})