import type * as TS from './ts-type-structs';
import type * as TJ from './ts-codegen-helpers';
import type * as A from './ts-ast';
import type { List, MutableStringDict, PFunction, PMethod, StringDict } from './ts-impl-types';

export interface Exports {
  'default-types': ReadonlyMap<string, TS.Type>;
  'default-data-exprs': ReadonlyMap<string, TS.DataType>;
  'default-modules': ReadonlyMap<string, TS.ModuleType>;
  'default-aliases': ReadonlyMap<string, TS.Type>;
}

type SDExports = {
  dict: { values: { dict: {
    'make-mutable-string-dict': PFunction<<T>() => MutableStringDict<T>>
    'is-mutable-string-dict': PFunction<<T>(val: any) => val is MutableStringDict<T>>,
    'make-string-dict': PFunction<<T>() => StringDict<T>>,
    'is-string-dict': PFunction<<T>(val: any) => val is StringDict<T>>,
    'map-keys': PFunction<<T, U>(f: ((key: T) => U), isd: StringDict<T>) => List<U>>,
    'map-keys-now': PFunction<<T, U>(f: ((key: T) => U), msd: MutableStringDict<T>) => List<U>>,
    'fold-keys': PFunction<<T, U>(f: (key: string, acc: U) => U, init: U, isd: StringDict<T>) => U>,
    'fold-keys-now': PFunction<<T, U>(f: (key: string, acc: U) => U, init: U, msd: MutableStringDict<T>) => U>,
    'each-key': PFunction<<T>(f: ((key: T) => void), isd: StringDict<T>) => void>,
    'each-key-now': PFunction<<T>(f: ((key: T) => void), msd: MutableStringDict<T>) => void>,
  }}}
}

// Based on https://stackoverflow.com/a/55344772/783424
type DropFirst<T extends unknown[]> = ((...p: T) => void) extends ((p1: infer P1, ...rest: infer R) => void) ? R : never

({
  requires: [
    { 'import-type': 'builtin', name: 'string-dict' },
    { 'import-type': 'dependency', protocol: 'js-file', args: ['ts-codegen-helpers']},
    { 'import-type': 'dependency', protocol: 'file', args: ['type-structs.arr']},
    { 'import-type': 'dependency', protocol: 'file', args: ['ast.arr']},
  ],
  provides: {},
  nativeRequires: [],
  theModule: function(runtime, _, __, SD : SDExports, tj : TJ.Exports, TSin : (TS.Exports), Ain : (A.Exports)) {
    const TS = TSin.dict.values.dict;
    const A  = Ain.dict.values.dict;

    // Note: duplicated because I don't think this file can import ts-compile-structs-helpers,
    // without causing a cyclic import...
    function callMethod<Name extends string, O extends {dict: {[n in Name]: PMethod<any, (...args: any[]) => any>}}>(obj : O, name: Name, ...args: DropFirst<Parameters<O["dict"][Name]["full_meth"]>>) : ReturnType<O["dict"][Name]["full_meth"]> {
      return obj.dict[name].full_meth(obj, ...args);
    }

    const builtinUri = TS['module-uri'].app("builtin://global");
    const srclocUri = TS['module-uri'].app("builtin://srcloc");
    const optionUri = TS['module-uri'].app("builtin://option");
    const equalityUri = TS['module-uri'].app("builtin://equality");
    const imageUri = TS['module-uri'].app("builtin://image");
    const reactorUri = TS['module-uri'].app("builtin://reactors");
    const primitiveTypesUri = TS['module-uri'].app("builtin://primitive-types")

    function typeGlobal(name: string): A.Name {
      return A['s-type-global'].app(name);
    }
    function tName(origin: TS.NameOrigin, name: string): TS.Type {
      return TS['t-name'].app(origin, typeGlobal(name), A['dummy-loc'], false);
    }

    const tTop = TS['t-top'].app(A['dummy-loc'], false);
    const tBot = TS['t-bot'].app(A['dummy-loc'], false);
    const tNumber = tName(primitiveTypesUri, "Number");
    const tString = tName(primitiveTypesUri, "String");
    const tBoolean = tName(primitiveTypesUri, "Boolean");
    const tTable = tName(primitiveTypesUri, "Table");
    const tSrcloc = tName(builtinUri, "Loc");
    const tImage = tName(imageUri, "Image");
    const tEqualityResult = tName(equalityUri, "EqualityResult");

    function tApp(ty : TS.Type, args : TS.Type[]): TS.Type {
      return TS['t-app'].app(ty, runtime.ffi.makeList(args), A['dummy-loc'], false);
    }
    function tOptionApp(ty: TS.Type): TS.Type {
      return tApp(tName(optionUri, "Option"), [ty]);
    }

    function tReactorApp(ty: TS.Type): TS.Type {
      return tApp(tName(reactorUri, "Reactor"), [ty]);
    }

    function tForall(vars: TS.Type[], ty: TS.Type): TS.Type {
      return TS['t-forall'].app(runtime.ffi.makeList(vars), ty, A['dummy-loc'], false);
    }

    function tArrow(args: TS.Type[], ret: TS.Type): TS.Type {
      return TS['t-arrow'].app(runtime.ffi.makeList(args), ret, A['dummy-loc'], false);
    }

    function tRecord(fields: Record<string, TS.Type>): TS.Type {
      const sd = SD.dict.values.dict['make-mutable-string-dict'].app<TS.Type>();
      for (const field in fields) {
        callMethod(sd, 'set-now', field, fields[field]);
      }
      return TS['t-record'].app(callMethod(sd, 'freeze'), A['dummy-loc'], false);
    }

    function tArray(ty: TS.Type): TS.Type {
      return tApp(tName(primitiveTypesUri, "RawArray"), [ty]);
    }

    function tVar(name: A.Name): TS.Type {
      return TS['t-var'].app(name, A['dummy-loc'], false);
    }

    const tva = tVar(A['s-type-global'].app("A"));
    const tvb = tVar(A['s-type-global'].app("B"));
    const tvc = tVar(A['s-type-global'].app("C"));
    const tvd = tVar(A['s-type-global'].app("D"));
    const tve = tVar(A['s-type-global'].app("E"));
    const tvf = tVar(A['s-type-global'].app("F"));

    function defaultTypes(): Map<string, TS.Type> {
      const ret = new Map<string, TS.Type>();
      ret.set("makeSome", tForall([tva], tArrow([tva], tOptionApp(tva))))
      ret.set("makeNone", tForall([tva], tArrow([], tOptionApp(tva))))
      ret.set("checkWrapBoolean", tArrow([tBoolean], tBoolean))
      ret.set("checkTupleBind", tArrow([tTop, tNumber, tSrcloc], tBot))
      ret.set("throwNonBooleanCondition", tArrow([tSrcloc, tString, tTop], tBot))
      ret.set("throwNoBranchesMatched", tArrow([tSrcloc, tString], tBot))
      ret.set("throwUnfinishedTemplate", tArrow([tSrcloc], tBot))
      ret.set("makeReactor", tForall([tva], tArrow([
          tva,
          tRecord({
            "on-tick": tOptionApp(tArrow([tva], tva)),
            "on-mouse": tOptionApp(tArrow([tva, tNumber, tNumber, tString], tva)),
            "on-key": tOptionApp(tArrow([tva, tString], tva)),
            "to-draw": tOptionApp(tArrow([tva], tImage)),
            "stop-when": tOptionApp(tArrow([tva], tBoolean)),
            "seconds-per-tick": tOptionApp(tNumber),
            "close-when-stop": tOptionApp(tBoolean),
            "title": tOptionApp(tString)
          })],
        tReactorApp(tva))))
      ret.set("hasField", tArrow([tRecord({ }), tString], tBoolean))
      ret.set("makeSrcloc", tArrow([tSrcloc], tBot))
    
      ret.set("not", tArrow([tBoolean], tBoolean))
      ret.set("roughly-equal-always", tArrow([tTop, tTop], tBoolean))
      ret.set("roughly-equal-now", tArrow([tTop, tTop], tBoolean))
      ret.set("roughly-equal", tArrow([tTop, tTop], tBoolean))
      ret.set("equal-always", tArrow([tTop, tTop], tBoolean))
      ret.set("equal-now", tArrow([tTop, tTop], tBoolean))
      ret.set("identical", tArrow([tTop, tTop], tBoolean))
      ret.set("roughly-equal-always3", tArrow([tTop, tTop], tEqualityResult))
      ret.set("roughly-equal-now3", tArrow([tTop, tTop], tEqualityResult))
      ret.set("equal-always3", tArrow([tTop, tTop], tEqualityResult))
      ret.set("equal-now3", tArrow([tTop, tTop], tEqualityResult))
      ret.set("identical3", tArrow([tTop, tTop], tEqualityResult))
    
      ret.set("getMaker", tForall([tva, tvb], tArrow([tRecord({ "make": tArrow([tArray(tvb)], tva)}), tString, tSrcloc, tSrcloc], tArrow([tArray(tvb)], tva))))
      ret.set("getLazyMaker", tForall([tva, tvb], tArrow([tRecord({ "lazy-make": tArrow([tArray(tArrow([], tvb))], tva)}), tString, tSrcloc, tSrcloc], tArrow([tArray(tArrow([], tvb))], tva))))
      ret.set("getMaker0", tForall([tva], tArrow([tRecord({ "make0": tArrow([], tva)}), tString, tSrcloc, tSrcloc], tArrow([], tva))))
      ret.set("getMaker1", tForall([tva, tvb], tArrow([tRecord({ "make1": tArrow([tvb], tva)}), tString, tSrcloc, tSrcloc], tArrow([tvb], tva))))
      ret.set("getMaker2", tForall([tva, tvb, tvc], tArrow([tRecord({ "make2": tArrow([tvb, tvc], tva)}), tString, tSrcloc, tSrcloc], tArrow([tvb, tvc], tva))))
      ret.set("getMaker3", tForall([tva, tvb, tvc, tvd], tArrow([tRecord({ "make3": tArrow([tvb, tvc, tvd], tva)}), tString, tSrcloc, tSrcloc], tArrow([tvb, tvc, tvd], tva))))
      ret.set("getMaker4", tForall([tva, tvb, tvc, tvd, tve], tArrow([tRecord({ "make4": tArrow([tvb, tvc, tvd, tve], tva)}), tString, tSrcloc, tSrcloc], tArrow([tvb, tvc, tvd, tve], tva))))
      ret.set("getMaker5", tForall([tva, tvb, tvc, tvd, tve, tvf], tArrow([tRecord({ "make5": tArrow([tvb, tvc, tvd, tve, tvf], tva)}), tString, tSrcloc, tSrcloc], tArrow([tvb, tvc, tvd, tve, tvf], tva))))
    
      ret.set("makeTable", tArrow([tArray(tTop), tArray(tArray(tTop))], tTable))
      ret.set("$traceValue", tArrow([tTop, tTop], tTop))
    
    
      return ret;
    }

    const exports : Exports = {
      "default-types": defaultTypes(),
      "default-modules": new Map(),
      "default-data-exprs": new Map(),
      "default-aliases": new Map(),
    };
    return runtime.makeJSModuleReturn(exports);
  }
})