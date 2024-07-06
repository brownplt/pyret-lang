import type * as TS from './ts-type-structs';
import type * as CS from './ts-compile-structs';
import type * as TJ from './ts-codegen-helpers';
import type * as TCS from './ts-type-check-structs';
import type * as A from './ts-ast';
import type { Option, PMethod } from './ts-impl-types';

export interface Exports {
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
  callMethod: <Name extends string, O extends {dict: {[n in Name]?: PMethod<any, (...args: any[]) => any>}}>(obj : O, name: Name, ...args: DropFirst<Parameters<Exclude<O["dict"][Name], undefined>["full_meth"]>>) => ReturnType<Exclude<O["dict"][Name], undefined>["full_meth"]>,
  unwrap: <T>(opt: Option<T>, orElseMsg: string) => T,
}

export type NonAliasValueExport = TJ.Variant<CS.ValueExport, Exclude<CS.ValueExport["$name"], "v-alias">>

export type NonAliasDataExport = TJ.Variant<CS.DataExport, Exclude<CS.DataExport["$name"], "d-alias">>

// Based on https://stackoverflow.com/a/55344772/783424
type DropFirst<T extends unknown[]> = ((...p: T) => void) extends ((p1: infer P1, ...rest: infer R) => void) ? R : never

({
  requires: [
    { 'import-type': 'dependency', protocol: 'js-file', args: ['ts-codegen-helpers']},
    { 'import-type': 'dependency', protocol: 'file', args: ['type-structs.arr']},
    { 'import-type': 'dependency', protocol: 'file', args: ['ast.arr']},
    { 'import-type': 'dependency', protocol: 'file', args: ['compile-structs.arr']},
    { 'import-type': 'dependency', protocol: 'file', args: ['type-check-structs.arr']},
  ],
  provides: {},
  nativeRequires: [],
  theModule: function(runtime, _, __, tj : TJ.Exports, TS : (TS.Exports), A : (A.Exports), CS : (CS.Exports), TCS : (TCS.Exports)) {

    const { InternalCompilerError, ExhaustiveSwitchError, nameToName, mapFromMutableStringDict } = tj;

    function callMethod<Name extends string, O extends {dict: {[n in Name]?: PMethod<any, (...args: any[]) => any>}}>(obj : O, name: Name, ...args: DropFirst<Parameters<Exclude<O["dict"][Name], undefined>["full_meth"]>>) : ReturnType<Exclude<O["dict"][Name], undefined>["full_meth"]> {
      return obj.dict[name]!.full_meth(obj, ...args);
    }

    function unwrap<T>(opt: Option<T>, orElseMsg: string): T {
      switch(opt.$name) {
        case 'none': throw new InternalCompilerError(orElseMsg);
        case 'some': return opt.dict.value;
        default: throw new ExhaustiveSwitchError(opt);
      }
    }

    function valueByUri(ce : CS.CompileEnvironment, uri: CS.URI, name : string) : Option<NonAliasValueExport> {
      const moduleByUriOpt = callMethod(ce.dict['all-modules'], 'get-now', uri);
      const moduleByUri = unwrap(moduleByUriOpt, `Could not find module with uri ${uri} in valueByUri in ${[...mapFromMutableStringDict(ce.dict['all-modules']).keys()].join(',')}`);
      const val = callMethod(moduleByUri.dict.provides.dict.values, 'get', name);
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
      }
    }

    function valueByUriValue(ce : CS.CompileEnvironment, uri: CS.URI, name : string) : NonAliasValueExport {
      return unwrap(valueByUri(ce, uri, name), `Could not find value ${name} on module ${uri}`);
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
      return unwrap(datatypeByUri(ce, uri, name), `Could not find datatype ${name} on module ${uri}`);
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
      const de = unwrap(datatypeByUri(ce, uri, name), `Could not find datatype ${name} on module ${uri}`);
      return de.dict.typ;
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
              const resolvedOpt = datatypeByUri(ce, remoteDatatype.dict.origin.dict['uri-of-definition'], remoteDatatype.dict.name);
              de = unwrap(resolvedOpt, `A datatype alias in an export was not found: ${JSON.stringify(remoteDatatype)}`);
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
      return unwrap(typeByUri(ce, uri, name), `Could not find type ${name} on module ${uri}`);
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
      return unwrap(globalValue(ce, name), `Could not find value ${name} as a global`);
    }

    function uriByDepKey(ce: CS.CompileEnvironment, depKey: string): CS.URI {
      return callMethod(ce.dict['my-modules'], 'get-value', depKey);
    }

    function providesByUri(ce: CS.CompileEnvironment, uri: CS.URI): Option<CS.Provides> {
      const provides = callMethod(ce.dict['all-modules'], 'get-now', uri);
      switch(provides.$name) {
        case 'none': return provides;
        case 'some': return runtime.ffi.makeSome(provides.dict.value.dict.provides);
        default: throw new ExhaustiveSwitchError(provides, 'providesByUri');
      }
    }

    function providesByUriValue(ce : CS.CompileEnvironment, uri: CS.URI) : CS.Provides {
      return unwrap(providesByUri(ce, uri), `Could not find module with uri ${uri} among ${[...mapFromMutableStringDict(ce.dict['all-modules']).keys()].join(',')}`);
    }

    function providesByOrigin(ce : CS.CompileEnvironment, origin: CS.BindOrigin): Option<CS.Provides> {
      return providesByUri(ce, origin.dict['uri-of-definition']);
    }

    function providesByOriginValue(ce : CS.CompileEnvironment, origin: CS.BindOrigin): CS.Provides {
      return providesByUriValue(ce, origin.dict['uri-of-definition']);
    }

    function providesByDepKey(ce: CS.CompileEnvironment, depKey: string): Option<CS.Provides> {
      const mod = unwrap(callMethod(ce.dict['my-modules'], 'get', depKey), `Could not find module with dep key ${depKey}`);
      const val = callMethod(ce.dict['all-modules'], 'get-value-now', mod);
      return runtime.ffi.makeSome(val.dict.provides);
    }

    function providesByDepKeyValue(ce : CS.CompileEnvironment, depKey: string) : CS.Provides {
      return unwrap(providesByDepKey(ce, depKey), `Could not find module with dep key ${depKey}`);
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
      return unwrap(providesByValueName(ce, name), `Could not find value named ${name}`);
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
      return unwrap(providesByTypeName(ce, name), `Could not find type named ${name}`);
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
      return unwrap(providesByModuleName(ce, name), `Could not find module named ${name}`);
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
      return unwrap(uriByValueName(ce, name), `Could not find ${name} in global values`);
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



    const exports : Exports = {
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
      unwrap,
    };
    return runtime.makeJSModuleReturn(exports);
  }
})