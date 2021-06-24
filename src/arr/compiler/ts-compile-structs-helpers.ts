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
        globalValueValue: (ce : CS.CompileEnvironment, g : string) => NonAliasExport
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

export type NonAliasExport = TJ.Variant<CS.ValueExport, Exclude<CS.ValueExport["$name"], "v-alias">>

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

    function callMethod<Name extends string, M extends (...args : any[]) => any, O extends{dict: {[n in Name]: PMethod<any, M>}}>(obj : O, name: Name, ...args: Parameters<ReturnType<O["dict"][Name]["meth"]>>) : ReturnType<O["dict"][Name]["full_meth"]> {
      return obj.dict[name].full_meth(obj, ...args);
    }

    function valueByUri(ce : CS.CompileEnvironment, uri : string, name : string) : Option<NonAliasExport> {
      const val = callMethod(callMethod(ce.dict['all-modules'], 'get-value-now', uri).dict.provides.dict.values, 'get', name);
      switch(val.$name) {
        case 'none': return val;
        case 'some': {
          switch(val.dict.value.$name) {
            case 'v-alias': {
              const { origin, 'original-name': originName } = val.dict.value.dict;
              if(uri === origin.dict['uri-of-definition']) {
                throw new InternalCompilerError("Self-referential alias for " + originName + " in module " + uri);
              }
              return valueByUri(ce, origin.dict['uri-of-definition'], originName);
            }
            case 'v-fun':
            case 'v-just-type':
            case 'v-var':
              return runtime.ffi.makeSome(val.dict.value);
            default: throw new ExhaustiveSwitchError(val.dict.value, "valueByUri exports");
          }
        }
        default: throw new ExhaustiveSwitchError(val, "valueByUri");
      }
    }

    function valueByOrigin(ce : CS.CompileEnvironment, origin : CS.BindOrigin) : Option<NonAliasExport> {
      return valueByUri(ce, origin.dict['uri-of-definition'], nameToName(origin.dict['original-name']));
    }

    function globalValue(ce : CS.CompileEnvironment, name : string) : Option<NonAliasExport> {
      const val = callMethod(ce.dict.globals.dict.values, 'get', name);
      switch(val.$name) {
        case 'none': return val;
        case 'some': 
          const ve = valueByOrigin(ce, val.dict.value);
          return ve;
        default: throw new ExhaustiveSwitchError(val, "globalValue");
      }
    }

    function globalValueValue(ce : CS.CompileEnvironment, name : string) : NonAliasExport {
      const ve = globalValue(ce, name);
      switch(ve.$name) {
        case 'none': throw new InternalCompilerError("Could not find value " + name + " as a global");
        case 'some': return ve.dict.value;
        default: throw new ExhaustiveSwitchError(ve, "globalValueValue");
      }
    }

    return runtime.makeJSModuleReturn({globalValueValue});
  }
})