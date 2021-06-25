import type * as TS from './ts-type-structs';
import type * as A from './ts-ast';
import type * as SL from './ts-srcloc';
import type * as CS from './ts-compile-structs';
import type * as TJ from './ts-codegen-helpers';
import type * as TCS from './ts-type-check-structs';
import type * as TCSH from './ts-compile-structs-helpers';
import type { List, MutableStringDict, PFunction, StringDict, Option } from './ts-impl-types';

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
    { 'import-type': 'builtin', name: 'srcloc'},
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
  theModule: function(runtime, _, __, SDin: SDExports, SL : SL.Exports, tj : TJ.Exports, TCSH : (TCSH.Exports), TSin : (TS.Exports), A : (A.Exports), CS : (CS.Exports), TCS : (TCS.Exports)) {
    const SD = SDin.dict.values.dict;
    const {
      ExhaustiveSwitchError,
      InternalCompilerError,
      listToArray,
      nameToKey,
      nameToName,
      formatSrcloc
    } = tj;
    const { builtin } = SL.dict.values.dict;
    const TS = TSin.dict.values.dict;
    const builtinUri = TS['module-uri'].app("builtin://global");
    const { globalValueValue, callMethod, providesByUri, valueByUriValue, resolveDatatypeByUri, resolveDatatypeByUriValue, typeByUri } = TCSH;
    const { ok } = CS.dict.values.dict;
    const { 's-global': sGlobal, 's-type-global': sTypeGlobal } = A.dict.values.dict;
    const { 
      typed,
      'tc-info': tcInfo,
      'empty-info': emptyInfo,
      'empty-context': emptyContext,
      'fold-result': foldResult,
      'fold-errors': foldErrors,
      "typing-context": typingContext
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
          const curTypes = SD['make-mutable-string-dict'].app<TS.Type>();
          const curAliases = callMethod(context.dict.info.dict.aliases, 'unfreeze');
          const curData = callMethod(context.dict.info.dict['data-types'], 'unfreeze');
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
                    if (callMethod(curTypes, 'has-key-now', valueKey)) {
                      break; // nothing more to do
                    } else {
                      // MARK(joe): test as-name here; it appears unused
                      const getValueFromContext = callMethod(context.dict.info.dict.types, 'get', valueKey);
                      switch(getValueFromContext.$name) {
                        case 'some': {
                          const typ = setInferred(getValueFromContext.dict.value, false);
                          callMethod(curTypes, 'set-now', valueKey, typ);
                          break;
                        }
                        case 'none': {
                          const typ = setInferred(callMethod(context.dict['global-types'], 'get-value', valueKey), false);
                          callMethod(curTypes, 'set-now', valueKey, typ);
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
                    if (callMethod(curAliases, 'has-key-now', aliasKey)) {
                      break; // nothing to do
                    } else {
                      const typ = callMethod(context.dict.aliases, 'get-value', aliasKey);
                      callMethod(curAliases, 'set-now', aliasKey, typ);
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
                    if (callMethod(curData, 'has-key-now', dataKey)) {
                      break; // nothing to do
                    } else {
                      const typ = callMethod(context.dict['data-types'], 'get-value', dataKey);
                      callMethod(curData, 'set-now', dataKey, typ);
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
            callMethod(curTypes, 'freeze'),
            callMethod(curAliases, 'freeze'),
            callMethod(curData, 'freeze'));
        }
        default: throw new ExhaustiveSwitchError(provide.$name);
      }
    }
    function typeCheck(program: A.Program, compileEnv : CS.CompileEnvironment, postCompileEnv : CS.ComputedEnvironment, modules : MutableStringDict<CS.Loadable>, options) {
      // DEMO output: options.dict.log.app("Hi!", runtime.ffi.makeNone());
      const provides = listToArray(program.dict.provides);
      let context = emptyContext.app();

      const globVs = compileEnv.dict.globals.dict.values;
      const globTs = compileEnv.dict.globals.dict.types;

      const contextGlobTs = callMethod(context.dict['aliases'], 'unfreeze');
      const contextGlobVs = callMethod(context.dict['global-types'], 'unfreeze');
      const contextGlobMods = SD["make-mutable-string-dict"].app<TS.ModuleType>();
      const contextGlobDTs = SD["make-mutable-string-dict"].app<TS.DataType>();

      for (const g of listToArray(callMethod(globVs, 'keys-list'))) {
        const key = nameToKey(sGlobal.app(g));
        if (callMethod(contextGlobVs, 'has-key-now', key)) {
          continue;
        }
        else {
          if(g === "_") {
            continue;
          }
          else {
            const ve =  globalValueValue(compileEnv, g);
            callMethod(contextGlobVs, 'set-now', key, ve.dict.t);
          }
        }
      }

      for (const g of listToArray(callMethod(globTs, "keys-list"))) {
        const key = nameToKey(sTypeGlobal.app(g));
        if (callMethod(contextGlobTs, 'has-key-now', key)) {
          continue;
        }
        else {
          const origin = callMethod(globTs, 'get-value', g);
          if (g === "_") { continue; }
          else {
            const provides = providesByUri(compileEnv, origin.dict['uri-of-definition']);
            switch(provides.$name) {
              case 'none':
                throw new InternalCompilerError(`Could not find module ${origin.dict['uri-of-definition']} in ${listToArray(callMethod(compileEnv.dict['all-modules'], 'keys-list-now'))} at ${formatSrcloc(program.dict.l, true)}}`);
              case 'some': {
                const provs = provides.dict.value;
                let t;
                const alias = callMethod(provs.dict.aliases, 'get', g);
                switch(alias.$name) {
                  case 'some': { t = alias.dict.value; break; }
                  case 'none': {
                    const dd = callMethod(provs.dict['data-definitions'], 'get', g);
                    switch(dd.$name) {
                      case 'none':
                        const keys = [
                          ...listToArray(callMethod(provs.dict.aliases, 'keys-list')),
                          ...listToArray(callMethod(provs.dict['data-definitions'], 'keys-list'))
                        ];
                        throw new InternalCompilerError(`Key ${g} not found in ${keys}`);
                      case 'some':
                        t = TS['t-name'].app(builtinUri, sTypeGlobal.app(g), builtin.app("global"), false);
                        break;
                      default: throw new ExhaustiveSwitchError(dd, "computing aliases from data defs");
                    }
                    break;
                  }
                  default: throw new ExhaustiveSwitchError(alias, "computing aliases");
                }
                callMethod(contextGlobTs, 'set-now', key, t);
                break;
              }
              default: throw new ExhaustiveSwitchError(provides, "computing aliases");
            }
          }
        }
      }

      for (let k of listToArray(callMethod(modules, 'keys-list-now'))) {
        if (callMethod(context.dict.modules, 'has-key', k)) {
          continue;
        }
        else {
          // NOTE/TODO/REVISIT(joe/ben/luna): Can we just resolve these with valueByUriValue/resolveDatatypeByUriValue
          const mod = callMethod(modules, 'get-value-now', k).dict.provides;
          const key = mod.dict['from-uri'];
          let valsTypesDict = SD['make-string-dict'].app<TS.Type>();
          for (let valKey of listToArray(callMethod(mod.dict.values, 'keys-list'))) {
            let typ : TS.Type;
            const ve = callMethod(mod.dict.values, 'get-value', valKey);
            switch(ve.$name) {
              case 'v-alias':
                const { origin } = ve.dict;
                typ = valueByUriValue(compileEnv, origin.dict['uri-of-definition'], nameToName(origin.dict['original-name'])).dict.t;
                break;
              default:
                typ = ve.dict.t;
            }
            valsTypesDict = callMethod(valsTypesDict, 'set', valKey, typ);
          }
          let dataDict = SD["make-string-dict"].app<TS.DataType>();
          for (let dataKey of listToArray(callMethod(mod.dict['data-definitions'], 'keys-list'))) {
            const de = callMethod(mod.dict['data-definitions'], 'get-value', dataKey);
            let typ : TS.DataType;
            switch(de.$name) {
              case 'd-alias':
                const { origin } = de.dict;
                typ = resolveDatatypeByUriValue(compileEnv, origin.dict['uri-of-definition'], nameToName(origin.dict['original-name']));
                break;
              default:
                typ = de.dict.typ;
            }
            dataDict = callMethod(dataDict, 'set', dataKey, typ);
          }
          const valProvides = TS['t-record'].app(valsTypesDict, program.dict.l, false);
          const moduleType = TS['t-module'].app(key, valProvides, dataDict, mod.dict.aliases);
          callMethod(contextGlobMods, 'set-now', key, moduleType);
          for(let dataKey of listToArray(callMethod(mod.dict['data-definitions'], 'keys-list'))) {
            // NOTE(joe): changed this to byUri***Value*** to not return an
            // Option, which conflicted with the type of the data-types field of
            // context (but evidently never triggered a dynamic error in our tests)
            const resolved = resolveDatatypeByUriValue(compileEnv, key, dataKey);
            callMethod(contextGlobDTs, 'set-now', dataKey, resolved);
          }
        }
      }

      if(postCompileEnv.$name === "computed-none") {
        throw new InternalCompilerError(`type-check got computed-none postCompileEnv in ${formatSrcloc(program.dict.l, true)}`);
      }

      const mbinds = postCompileEnv.dict['module-bindings'];
      const vbinds = postCompileEnv.dict.bindings;
      const tbinds = postCompileEnv.dict['type-bindings'];

      const contextGlobModnames = callMethod(context.dict['module-names'], 'unfreeze');

      for (let key of listToArray(callMethod(mbinds, 'keys-list-now'))) {
        callMethod(contextGlobModnames, 'set-now', key, callMethod(mbinds, 'get-value-now', key).dict.uri);
      }

      for (let key of listToArray(callMethod(vbinds, 'keys-list-now'))) {
        const vbind = callMethod(vbinds, 'get-value-now', key);
        if (vbind.dict.origin.dict['new-definition']) { continue; }
        else {
          const thismod = callMethod(contextGlobMods, 'get-value-now', vbind.dict.origin.dict['uri-of-definition']);
          const originalName = nameToName(vbind.dict.origin.dict['original-name']);
          const field = callMethod(thismod.dict.provides.dict.fields, 'get', originalName);
          switch(field.$name) {
            case 'none': throw new InternalCompilerError(`Cannot find value bind for ${originalName} in ${formatSrcloc(program.dict.l, true)}`);
            case 'some':
              callMethod(contextGlobVs, 'set-now', key, field.dict.value);
              break;
            default:
              throw new ExhaustiveSwitchError(field, "vbinds context setup");
          }
        }
      }

      for (let key of listToArray(callMethod(tbinds, 'keys-list-now'))) {
        const tbind = callMethod(tbinds, 'get-value-now', key);
        const origin = tbind.dict.origin;
        if (origin.dict['new-definition']) { continue; }
        else {
          const originalName = nameToName(origin.dict['original-name']);
          const originalType = typeByUri(compileEnv, origin.dict['uri-of-definition'], originalName);
          switch(originalType.$name) {
            case 'none': throw new InternalCompilerError(`Cannot find type bind for ${originalName} in ${formatSrcloc(program.dict.l, true)}`);
            case 'some': callMethod(contextGlobTs, 'set-now', key, originalType.dict.value)
          }
        }
      }


      const contextFromModules = typingContext.app(
          callMethod(contextGlobVs, 'freeze'),
          callMethod(contextGlobTs, 'freeze'),
          callMethod(contextGlobDTs, 'freeze'),
          callMethod(contextGlobMods, 'freeze'),
          callMethod(contextGlobModnames, 'freeze'),
          context.dict['binds'],
          context.dict['constraints'],
          context.dict['info'],
          context.dict['misc'],
      )


      const info = gatherProvides(provides[0], contextFromModules);
      return ok.app(typed.app(program, info));
    }
    return runtime.makeModuleReturn({
      'type-check': runtime.makeFunction(typeCheck)
    }, {});
  }
})