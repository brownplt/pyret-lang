import type * as TS from './ts-type-structs';
import type * as CS from './ts-compile-structs';
import type * as TJ from './ts-codegen-helpers';
import type * as TCS from './ts-type-check-structs';
import type * as A from './ts-ast';
import type { List, MutableStringDict, Option, PFunction, PMethod, StringDict } from './ts-impl-types';

export interface Exports {
  dict: {
    values: {
      dict: {
        valueByUri: (ce : CS.CompileEnvironment, uri: CS.URI, name : string) => Option<NonAliasValueExport>,
        valueByUriValue: (ce : CS.CompileEnvironment, uri: CS.URI, name : string) => NonAliasValueExport,
        datatypeByUri: (ce : CS.CompileEnvironment, uri: CS.URI, name : string) => Option<NonAliasDataExport>,
        datatypeByUriValue: (ce : CS.CompileEnvironment, uri: CS.URI, name : string) => NonAliasDataExport,
        resolveDatatypeByUri: (ce : CS.CompileEnvironment, uri: CS.URI, name : string) => Option<TS.DataType>,
        resolveDatatypeByUriValue: (ce : CS.CompileEnvironment, uri: CS.URI, name : string) => TS.DataType,
        valueByOrigin: (ce : CS.CompileEnvironment, origin : CS.BindOrigin) => Option<NonAliasValueExport>,
        valueByOriginValue: (ce : CS.CompileEnvironment, origin : CS.BindOrigin) => NonAliasValueExport,
        typeByUri: (ce : CS.CompileEnvironment, uri: CS.URI, name : string) => Option<TS.Type>,
        typeByUriValue: (ce : CS.CompileEnvironment, uri: CS.URI, name : string) => TS.Type,
        typeByOrigin: (ce : CS.CompileEnvironment, origin: CS.BindOrigin) => Option<TS.Type>,
        typeByOriginValue: (ce : CS.CompileEnvironment, origin: CS.BindOrigin) => TS.Type,
        globalValueValue: (ce : CS.CompileEnvironment, g : string) => NonAliasValueExport,
        uriByDepKey: (ce: CS.CompileEnvironment, depKey: string) => CS.URI,
        providesByUri: (ce: CS.CompileEnvironment, uri: CS.URI) => Option<CS.Provides>,
        providesByUriValue: (ce : CS.CompileEnvironment, uri: CS.URI) => CS.Provides,
        providesByOrigin: (ce : CS.CompileEnvironment, origin: CS.BindOrigin) => Option<CS.Provides>,
        providesByOriginValue: (ce : CS.CompileEnvironment, origin: CS.BindOrigin) => CS.Provides,
        providesByDepKey: (ce: CS.CompileEnvironment, depKey: string) => Option<CS.Provides>,
        providesByDepKeyValue: (ce: CS.CompileEnvironment, depKey: string) => CS.Provides,
        providesByValueName: (ce: CS.CompileEnvironment, name: string) => Option<CS.Provides>,
        providesByValueNameValue: (ce: CS.CompileEnvironment, name: string) => CS.Provides,
        providesByTypeName: (ce: CS.CompileEnvironment, name: string) => Option<CS.Provides>,
        providesByTypeNameValue: (ce: CS.CompileEnvironment, name: string) => CS.Provides,
        providesByModuleName: (ce: CS.CompileEnvironment, name: string) => Option<CS.Provides>,
        providesByModuleNameValue: (ce: CS.CompileEnvironment, name: string) => CS.Provides,
        valueByDepKey: (ce: CS.CompileEnvironment, depKey: string, name: string) => Option<NonAliasValueExport>,
        valueByDepKeyValue: (ce: CS.CompileEnvironment, depKey: string, name: string) => NonAliasValueExport,
        typeByDepKey: (ce: CS.CompileEnvironment, depKey: string, name: string) => Option<TS.Type>,
        uriByValueName: (ce: CS.CompileEnvironment, name: string) => Option<CS.URI>,
        uriByTypeName: (ce: CS.CompileEnvironment, name: string) => Option<CS.URI>,
        uriByModuleName: (ce: CS.CompileEnvironment, name: string) => Option<CS.URI>,
        uriByValueNameValue: (ce: CS.CompileEnvironment, name: string) => CS.URI,
        originByValueName: (ce: CS.CompileEnvironment, name: string) => Option<CS.BindOrigin>,
        originByTypeName: (ce: CS.CompileEnvironment, name: string) => Option<CS.BindOrigin>,
        originByModuleName: (ce: CS.CompileEnvironment, name: string) => Option<CS.BindOrigin>,
        callMethod: <Name extends string, O extends {dict: {[n in Name]: PMethod<any, (...args: any[]) => any>}}>(obj : O, name: Name, ...args: DropFirst<Parameters<O["dict"][Name]["full_meth"]>>) => ReturnType<O["dict"][Name]["full_meth"]>,
      }
    }
  }
}

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

export type NonAliasValueExport = TJ.Variant<CS.ValueExport, Exclude<CS.ValueExport["$name"], "v-alias">>

export type NonAliasDataExport = TJ.Variant<CS.DataExport, Exclude<CS.DataExport["$name"], "d-alias">>

// Based on https://stackoverflow.com/a/55344772/783424
type DropFirst<T extends unknown[]> = ((...p: T) => void) extends ((p1: infer P1, ...rest: infer R) => void) ? R : never

({
  requires: [
    { 'import-type': 'builtin', name: 'string-dict' },
    { 'import-type': 'dependency', protocol: 'js-file', args: ['ts-codegen-helpers']},
    { 'import-type': 'dependency', protocol: 'file', args: ['type-structs.arr']},
    { 'import-type': 'dependency', protocol: 'file', args: ['ast.arr']},
    { 'import-type': 'dependency', protocol: 'file', args: ['compile-structs.arr']},
    { 'import-type': 'dependency', protocol: 'file', args: ['type-check-structs.arr']},
    { 'import-type': 'dependency', protocol: 'file', args: ['type-defaults.arr']},
  ],
  provides: {},
  nativeRequires: [],
  theModule: function(runtime, _, __, SD: SDExports, tj : TJ.Exports, TS : (TS.Exports), A : (A.Exports), CS : (CS.Exports), TCS : (TCS.Exports)) {

    const { InternalCompilerError, ExhaustiveSwitchError, nameToName } = tj;

    function callMethod<Name extends string, O extends {dict: {[n in Name]: PMethod<any, (...args: any[]) => any>}}>(obj : O, name: Name, ...args: DropFirst<Parameters<O["dict"][Name]["full_meth"]>>) : ReturnType<O["dict"][Name]["full_meth"]> {
      return obj.dict[name].full_meth(obj, ...args);
    }

    function valueByUri(ce : CS.CompileEnvironment, uri: CS.URI, name : string) : Option<NonAliasValueExport> {
      const moduleByUri = callMethod(ce.dict['all-modules'], 'get-now', uri);
      switch(moduleByUri.$name) {
        case 'none': throw new InternalCompilerError(`Could not find module with uri ${uri} in valueByUri`);
        case 'some': {
          const val = callMethod(moduleByUri.dict.value.dict.provides.dict.values, 'get', name);
          switch(val.$name) {
            case 'none': return val;
            case 'some': {
              switch(val.dict.value.$name) {
                case 'v-alias': {
                  const { origin, 'original-name': originName } = val.dict.value.dict;
                  if(uri === origin.dict['uri-of-definition']) {
                    throw new InternalCompilerError(`Self-referential alias for ${originName} in module ${uri}`);
                  }
                  return valueByUri(ce, origin.dict['uri-of-definition'], originName);
                }
                case 'v-fun':
                case 'v-just-type':
                case 'v-var': {
                  return runtime.ffi.makeSome(val.dict.value);
                }
                default: throw new ExhaustiveSwitchError(val.dict.value, "valueByUri exports");
              }
            }
            default: throw new ExhaustiveSwitchError(val, "valueByUri");
          }
        }
        default: throw new ExhaustiveSwitchError(moduleByUri, "valueByUri");
      }
    }

    function valueByUriValue(ce : CS.CompileEnvironment, uri: CS.URI, name : string) : NonAliasValueExport {
      const ve = valueByUri(ce, uri, name);
      switch(ve.$name) {
        case 'none': throw new InternalCompilerError(`Could not find value ${name} on module ${uri}`);
        case 'some': return ve.dict.value;
        default: throw new ExhaustiveSwitchError(ve, "valueByUriValue");
      }
    }

    function datatypeByUri(ce: CS.CompileEnvironment, uri: CS.URI, name: string): Option<NonAliasDataExport> {
      const val = callMethod(callMethod(ce.dict['all-modules'], 'get-value-now', uri).dict.provides.dict['data-definitions'], 'get', name);
      switch(val.$name) {
        case 'none': return val;
        case 'some': {
          switch(val.dict.value.$name) {
            case 'd-alias': {
              const { origin, name } = val.dict.value.dict;
              if(uri === origin.dict['uri-of-definition']) {
                throw new InternalCompilerError(`Self-referential alias for ${name} in module ${uri}`);
              }
              return datatypeByUri(ce, origin.dict['uri-of-definition'], name);
            }
            case 'd-type': return runtime.ffi.makeSome(val.dict.value);
            default: throw new ExhaustiveSwitchError(val.dict.value, "datatypeByUri exports");
          }
        }
        default: throw new ExhaustiveSwitchError(val, "datatypeByUri");
      }
    }

    function datatypeByUriValue(ce : CS.CompileEnvironment, uri: CS.URI, name : string) : NonAliasDataExport {
      const de = datatypeByUri(ce, uri, name);
      switch(de.$name) {
        case 'none': throw new InternalCompilerError(`Could not find datatype ${name} on module ${uri}`);
        case 'some': return de.dict.value;
        default: throw new ExhaustiveSwitchError(de, "datatypeByUriValue");
      }
    }

    function resolveDatatypeByUri(ce: CS.CompileEnvironment, uri: CS.URI, name: string): Option<TS.DataType> {
      const de = datatypeByUri(ce, uri, name);
      switch(de.$name) {
        case 'none': return de;
        case 'some': return runtime.ffi.makeSome(de.dict.value.dict.typ);
        default: throw new ExhaustiveSwitchError(de, "resolveDatatypeByUriValue");
      }
    }

    function resolveDatatypeByUriValue(ce: CS.CompileEnvironment, uri: CS.URI, name: string): TS.DataType {
      const de = datatypeByUri(ce, uri, name);
      switch(de.$name) {
        case 'none': throw new InternalCompilerError(`Could not find datatype ${name} on module ${uri}`);
        case 'some': return de.dict.value.dict.typ;
        default: throw new ExhaustiveSwitchError(de, "resolveDatatypeByUriValue");
      }
    }

    function valueByOrigin(ce : CS.CompileEnvironment, origin : CS.BindOrigin) : Option<NonAliasValueExport> {
      return valueByUri(ce, origin.dict['uri-of-definition'], nameToName(origin.dict['original-name']));
    }

    function valueByOriginValue(ce : CS.CompileEnvironment, origin : CS.BindOrigin) : NonAliasValueExport {
      return valueByUriValue(ce, origin.dict['uri-of-definition'], nameToName(origin.dict['original-name']));
    }

    function typeByUri(ce : CS.CompileEnvironment, uri: CS.URI, name : string) : Option<TS.Type> {
      const providesOfAliased = callMethod(ce.dict['all-modules'], 'get-value-now', uri).dict.provides;
      const dataOption = callMethod(providesOfAliased.dict['data-definitions'], 'get', name);
      switch(dataOption.$name) {
        case 'some': {
          const remoteDatatype = dataOption.dict.value;
          let de: NonAliasDataExport;
          switch(remoteDatatype.$name) {
            case 'd-alias': {
              const resolved = datatypeByUri(ce, remoteDatatype.dict.origin.dict['uri-of-definition'], remoteDatatype.dict.name);
              switch(resolved.$name) {
                case 'none': throw new InternalCompilerError(`A datatype alias in an export was not found: ${JSON.stringify(remoteDatatype)}`);
                case 'some': {
                  de = resolved.dict.value;
                  break;
                }
                default: throw new ExhaustiveSwitchError(resolved, "typeByUri resolved");
              }
              break;
            }
            case 'd-type': {
              de = remoteDatatype;
              break;
            }
            default: throw new ExhaustiveSwitchError(remoteDatatype, "typeByUri remoteDatatype");
          }
          return runtime.ffi.makeSome(
            TS.dict.values.dict['t-name'].app(
              TS.dict.values.dict['module-uri'].app(de.dict.origin.dict['uri-of-definition']),
              A.dict.values.dict['s-type-global'].app(de.dict.typ.dict.name),
              de.dict.origin.dict['local-bind-site'],
              false,
            )
          );
        }
        case 'none': {
          const aliasOption = callMethod(providesOfAliased.dict.aliases, 'get', name);
          switch(aliasOption.$name) {
            case 'none': return aliasOption;
            case 'some': {
              const typ = aliasOption.dict.value;
              switch(typ.$name) {
                case 't-name': {
                  switch(typ.dict['module-name'].$name) {
                    case 'module-uri': return typeByUri(ce, typ.dict['module-name'].dict.uri, nameToName(typ.dict.id));
                    case 'dependency':
                    case 'local':
                      throw new InternalCompilerError(`A provided type alias referred to an unresolved module: ${JSON.stringify(typ)}`);
                    default: throw new ExhaustiveSwitchError(typ.dict['module-name'], "typeByUri aliasOption moduleName")
                  }
                }
                default: return runtime.ffi.makeSome(typ);
              }
            }
            default: throw new ExhaustiveSwitchError(aliasOption, 'typeByUri aliasOption');
          }
        }
        default: throw new ExhaustiveSwitchError(dataOption, "typeByUri dataOption");
      }
    }

    function typeByUriValue(ce : CS.CompileEnvironment, uri: CS.URI, name : string) : TS.Type {
      const te = typeByUri(ce, uri, name);
      switch(te.$name) {
        case 'none': throw new InternalCompilerError(`Could not find type ${name} on module ${uri}`);
        case 'some': return te.dict.value;
        default: throw new ExhaustiveSwitchError(te, "typeByUriValue");
      }
    }

    function typeByOrigin(ce: CS.CompileEnvironment, origin: CS.BindOrigin): Option<TS.Type> {
      return typeByUri(ce, origin.dict['uri-of-definition'], nameToName(origin.dict['original-name']));
    }

    function typeByOriginValue(ce : CS.CompileEnvironment, origin: CS.BindOrigin) : TS.Type {
      return typeByUriValue(ce, origin.dict['uri-of-definition'], nameToName(origin.dict['original-name']));
    }

    function globalValue(ce : CS.CompileEnvironment, name : string) : Option<NonAliasValueExport> {
      const val = callMethod(ce.dict.globals.dict.values, 'get', name);
      switch(val.$name) {
        case 'none': return val;
        case 'some': return valueByOrigin(ce, val.dict.value);
        default: throw new ExhaustiveSwitchError(val, "globalValue");
      }
    }

    function globalValueValue(ce : CS.CompileEnvironment, name : string) : NonAliasValueExport {
      const ve = globalValue(ce, name);
      switch(ve.$name) {
        case 'none': throw new InternalCompilerError(`Could not find value ${name} as a global`);
        case 'some': return ve.dict.value;
        default: throw new ExhaustiveSwitchError(ve, "globalValueValue");
      }
    }

    function uriByDepKey(ce: CS.CompileEnvironment, depKey: string): CS.URI {
      return callMethod(ce.dict['my-modules'], 'get-value', depKey);
    }

    function providesByUri(ce: CS.CompileEnvironment, uri: CS.URI): Option<CS.Provides> {
      const provides = callMethod(ce.dict['all-modules'], 'get-now', uri);
      switch(provides.$name) {
        case 'none': return provides;
        case 'some': return runtime.ffi.makeSome(provides.dict.value);
        default: throw new ExhaustiveSwitchError(provides, 'providesByUri');
      }
    }

    function providesByUriValue(ce : CS.CompileEnvironment, uri: CS.URI) : CS.Provides {
      const provides = providesByUri(ce, uri);
      switch(provides.$name) {
        case 'none': throw new InternalCompilerError(`Could not find module with uri ${uri}`);
        case 'some': return provides.dict.value;
        default: throw new ExhaustiveSwitchError(provides, "providesByUriValue");
      }
    }

    function providesByOrigin(ce : CS.CompileEnvironment, origin: CS.BindOrigin): Option<CS.Provides> {
      return providesByUri(ce, origin.dict['uri-of-definition']);
    }

    function providesByOriginValue(ce : CS.CompileEnvironment, origin: CS.BindOrigin): CS.Provides {
      return providesByUriValue(ce, origin.dict['uri-of-definition']);
    }

    function providesByDepKey(ce: CS.CompileEnvironment, depKey: string): Option<CS.Provides> {
      const mod = callMethod(ce.dict['my-modules'], 'get', depKey);
      switch(mod.$name) {
        case 'none': throw new InternalCompilerError(`Could not find module with dep key ${depKey}`);
        case 'some': {
          const val = callMethod(ce.dict['all-modules'], 'get-value-now', mod.dict.value);
          return runtime.ffi.makeSome(val.dict.provides);
        }
        default: throw new ExhaustiveSwitchError(mod, 'providesByDepKey');
      }
    }

    function providesByDepKeyValue(ce : CS.CompileEnvironment, depKey: string) : CS.Provides {
      const provides = providesByDepKey(ce, depKey);
      switch(provides.$name) {
        case 'none': throw new InternalCompilerError(`Could not find module with dep key ${depKey}`);
        case 'some': return provides.dict.value;
        default: throw new ExhaustiveSwitchError(provides, "providesByUriValue");
      }
    }

    function providesByValueName(ce: CS.CompileEnvironment, name: string): Option<CS.Provides> {
      const val = callMethod(ce.dict.globals.dict.values, 'get', name);
      switch(val.$name) {
        case 'none': return val;
        case 'some': return providesByOrigin(ce, val.dict.value);
        default: throw new ExhaustiveSwitchError(val, "providesByValueName");
      }
    }

    function providesByValueNameValue(ce: CS.CompileEnvironment, name: string): CS.Provides {
      const provides = providesByValueName(ce, name);
      switch(provides.$name) {
        case 'none': throw new InternalCompilerError(`Could not find value named ${name}`);
        case 'some': return provides.dict.value;
        default: throw new ExhaustiveSwitchError(provides, "providesByValueNameValue");
      }
    }

    function providesByTypeName(ce: CS.CompileEnvironment, name: string): Option<CS.Provides> {
      const typ = callMethod(ce.dict.globals.dict.types, 'get', name);
      switch(typ.$name) {
        case 'none': return typ;
        case 'some': return providesByOrigin(ce, typ.dict.value);
        default: throw new ExhaustiveSwitchError(typ, "providesByValueName");
      }
    }

    function providesByTypeNameValue(ce: CS.CompileEnvironment, name: string): CS.Provides {
      const provides = providesByTypeName(ce, name);
      switch(provides.$name) {
        case 'none': throw new InternalCompilerError(`Could not find type named ${name}`);
        case 'some': return provides.dict.value;
        default: throw new ExhaustiveSwitchError(provides, "providesByTypeNameValue");
      }
    }

    function providesByModuleName(ce: CS.CompileEnvironment, name: string): Option<CS.Provides> {
      const mod = callMethod(ce.dict.globals.dict.modules, 'get', name);
      switch(mod.$name) {
        case 'none': return mod;
        case 'some': return providesByOrigin(ce, mod.dict.value);
        default: throw new ExhaustiveSwitchError(mod, "providesByValueName");
      }
    }

    function providesByModuleNameValue(ce: CS.CompileEnvironment, name: string): CS.Provides {
      const provides = providesByModuleName(ce, name);
      switch(provides.$name) {
        case 'none': throw new InternalCompilerError(`Could not find module named ${name}`);
        case 'some': return provides.dict.value;
        default: throw new ExhaustiveSwitchError(provides, "providesByModuleNameValue");
      }
    }

    function valueByDepKey(ce: CS.CompileEnvironment, depKey: string, name: string): Option<NonAliasValueExport> {
      return valueByUri(ce, callMethod(ce.dict['my-modules'], 'get-value', depKey), name);
    }

    function valueByDepKeyValue(ce: CS.CompileEnvironment, depKey: string, name: string): NonAliasValueExport {
      return valueByUriValue(ce, callMethod(ce.dict['my-modules'], 'get-value', depKey), name);
    }

    function typeByDepKey(ce: CS.CompileEnvironment, depKey: string, name: string): Option<TS.Type> {
      return typeByUri(ce, callMethod(ce.dict['my-modules'], 'get-value', depKey), name);
    }

    function uriByValueName(ce: CS.CompileEnvironment, name: string): Option<CS.URI> {
      const uri = callMethod(ce.dict.globals.dict.values, 'get', name);
      switch(uri.$name) {
        case 'none': return uri;
        case 'some': return runtime.ffi.makeSome(uri.dict.value.dict['uri-of-definition']);
        default: throw new ExhaustiveSwitchError(uri, 'uriByValueName');
      }
    }

    function uriByTypeName(ce: CS.CompileEnvironment, name: string): Option<CS.URI> {
      const uri = callMethod(ce.dict.globals.dict.types, 'get', name);
      switch(uri.$name) {
        case 'none': return uri;
        case 'some': return runtime.ffi.makeSome(uri.dict.value.dict['uri-of-definition']);
        default: throw new ExhaustiveSwitchError(uri, 'uriByTypeName');
      }
    }

    function uriByModuleName(ce: CS.CompileEnvironment, name: string): Option<CS.URI> {
      const uri = callMethod(ce.dict.globals.dict.modules, 'get', name);
      switch(uri.$name) {
        case 'none': return uri;
        case 'some': return runtime.ffi.makeSome(uri.dict.value.dict['uri-of-definition']);
        default: throw new ExhaustiveSwitchError(uri, 'uriByModuleName');
      }
    }

    function uriByValueNameValue(ce: CS.CompileEnvironment, name: string): CS.URI {
      const uri = uriByValueName(ce, name);
      switch(uri.$name) {
        case 'none': throw new InternalCompilerError(`Could not find ${name} in global values`);
        case 'some': return uri.dict.value;
        default: throw new ExhaustiveSwitchError(uri, 'uriByValueNameValue');
      }
    }

    function originByValueName(ce: CS.CompileEnvironment, name: string): Option<CS.BindOrigin> {
      return callMethod(ce.dict.globals.dict.values, 'get', name);
    }

    function originByTypeName(ce: CS.CompileEnvironment, name: string): Option<CS.BindOrigin> {
      return callMethod(ce.dict.globals.dict.types, 'get', name);
    }

    function originByModuleName(ce: CS.CompileEnvironment, name: string): Option<CS.BindOrigin> {
      return callMethod(ce.dict.globals.dict.modules, 'get', name);
    }



    const exports : Exports['dict']['values']['dict'] = {
      valueByUri,
      valueByUriValue,
      datatypeByUri,
      datatypeByUriValue,
      resolveDatatypeByUri,
      resolveDatatypeByUriValue,
      valueByOrigin,
      valueByOriginValue,
      typeByUri,
      typeByUriValue,
      typeByOrigin,
      typeByOriginValue,
      globalValueValue,
      uriByDepKey,
      providesByUri,
      providesByUriValue,
      providesByOrigin,
      providesByOriginValue,
      providesByDepKey,
      providesByDepKeyValue,
      providesByValueName,
      providesByValueNameValue,
      providesByTypeName,
      providesByTypeNameValue,
      providesByModuleName,
      providesByModuleNameValue,
      valueByDepKey,
      valueByDepKeyValue,
      typeByDepKey,
      uriByValueName,
      uriByTypeName,
      uriByModuleName,
      uriByValueNameValue,
      originByValueName,
      originByTypeName,
      originByModuleName,
      callMethod,
    };
    return runtime.makeJSModuleReturn(exports);
  }
})