/*
  Ported from: src/js/trove/builtin-modules.js

  Reimplements the raw-module-record loading in plain TS, without the Pyret
  runtime. A builtin trove .js file is a single parenthesized object literal

      ({ requires, nativeRequires, provides, theModule })

  the original evaluates it with secure-loader's safeEval("define(" + src +
  ")", { define }); here we do the same with node's vm module. `theModule`
  functions are captured but never invoked by the compiler.

  Type expansion (shorthands, compact array/string forms -> tagged records)
  is done by the existing runtime-independent src/js/base/type-util.js,
  loaded through interop/amd.ts. Where the original passes RUNTIME so that
  toPyretType/bindToPyret build Pyret records and lists, we pass a pass-
  through "runtime" whose makeObject/makeList are the identity, producing
  the plain-JS raw shapes that compile-structs' providesFromRawProvides /
  typeFromRaw consume.

  NOTE: older versions of builtin-modules.js carried a known-modules table;
  the current source file does not, so there is nothing to port for it.
*/

import * as fs from 'fs';
import * as path from 'path';
import * as vm from 'vm';
import { amdRequire } from './interop/amd';

let typeUtilMod: any = undefined;
function typeUtil(): any {
  if (typeUtilMod === undefined) typeUtilMod = amdRequire('pyret-base/js/type-util');
  return typeUtilMod;
}

// Pass-through stand-in for the Pyret runtime: toPyretType/bindToPyret only
// use makeObject and ffi.makeList, so identity functions give us back plain
// JS objects/arrays in exactly the raw shape the original would hand to
// provides-from-raw-provides.
const passThroughRuntime = {
  makeObject: (o: any) => o,
  makeString: (s: string) => s,
  ffi: { makeList: (l: any[]) => l }
};

export interface RawBuiltinLocator {
  getRawDependencies(): any[];
  getRawNativeModules(): string[];
  getRawDatatypeProvides(): any[];
  getRawModuleProvides(): any[];
  getRawAliasProvides(): any[];
  getRawValueProvides(): any[];
  getRawCompiled(): string;
}

function safeEvalDefine(moduleString: string, define: (answer: any) => void): void {
  // Mirrors loader.safeEval("define(" + moduleString + ")", {define: setAns}).
  vm.runInNewContext('define(' + moduleString + ')', { define: define });
}

// Builds a raw locator from an already-evaluated module record (the
// parenthesized-object-literal value itself). This is the entry point a
// browser host uses: the page's staticModules are live JS objects, so no
// vm/fs is involved. getModule is a thunk so hosts can keep laziness.
export function builtinRawLocatorFromModule(
  getModule: () => any,
  getCompiled: () => string
): RawBuiltinLocator {
  const getData = (_ignored?: any) => getModule();

  const t = typeUtil();

  return {
    getRawDependencies(): any[] {
      const m = getData();
      if (m.requires) {
        return m.requires.map(function (req: any) {
          // NOTE(joe): This allows us to use builtin imports
          // without a bootstrap for the compiler re-inserting
          // import-type fields
          if (!req["import-type"]) {
            req["import-type"] = "dependency";
          }
          return req;
        });
      } else {
        return [];
      }
    },
    getRawNativeModules(): string[] {
      const m = getData();
      if (Array.isArray(m.nativeRequires)) {
        return m.nativeRequires.map((s: any) => passThroughRuntime.makeString(s));
      } else {
        return [];
      }
    },
    getRawDatatypeProvides(): any[] {
      const m = getData();
      if (m.provides && m.provides.datatypes) {
        const dts = m.provides.datatypes;
        if (typeof dts === "object") {
          return Object.keys(dts).map(function (k) {
            const shorthands = m.provides.shorthands || {};
            const expanded = t.expandType(dts[k], t.expandRecord(shorthands, {}));
            return {
              name: k,
              typ: t.toPyretType(passThroughRuntime, expanded)
            };
          });
        } else {
          throw new Error("Bad datatype specification: " + String(m.provides.datatypes));
        }
      }
      return [];
    },
    getRawModuleProvides(): any[] {
      const m = getData();
      if (typeof m.provides.modules === "object") {
        const mods = m.provides.modules;
        return Object.keys(mods).map(function (k) {
          return {
            name: k,
            uri: mods[k].uri
          };
        });
      } else {
        return [];
      }
    },
    getRawAliasProvides(): any[] {
      const m = getData();
      if (m.provides) {
        if (Array.isArray(m.provides.types)) {
          return m.provides.types;
        } else if (typeof m.provides.aliases === "object") {
          const aliases = m.provides.aliases;
          return Object.keys(aliases).map(function (k) {
            const shorthands = m.provides.shorthands || {};
            const expanded = t.expandType(aliases[k], t.expandRecord(shorthands, {}));
            return {
              name: k,
              typ: t.toPyretType(passThroughRuntime, expanded)
            };
          });
        }
      }
      return [];
    },
    getRawValueProvides(): any[] {
      const m = getData();
      if (m.provides) {
        if (Array.isArray(m.provides.values)) {
          return m.provides.values;
        } else if (typeof m.provides.values === "object") {
          const vals = m.provides.values;
          return Object.keys(vals).map(function (k) {
            const shorthands = m.provides.shorthands || {};
            const expanded = t.expandType(vals[k], t.expandRecord(shorthands, {}));
            return {
              name: k,
              value: t.bindToPyret(passThroughRuntime, expanded, shorthands)
            };
          });
        }
      }
      return [];
    },
    getRawCompiled(): string {
      return getCompiled();
    }
  };
}

export function builtinRawLocatorFromStr(content: string): RawBuiltinLocator {
  const noModuleContent = {};
  let moduleContent: any = noModuleContent;

  // Lazily evaluates the module text on first use, mirroring the original
  // loader.safeEval("define(" + content + ")", { define }) discipline.
  function getModule(): any {
    if (moduleContent === noModuleContent) {
      const setAns = (answer: any) => {
        moduleContent = answer;
      };
      try {
        safeEvalDefine(content, setAns);
      } catch (e) {
        console.error("Content was: ", content);
        throw e;
      }
    }
    return moduleContent;
  }

  return builtinRawLocatorFromModule(getModule, () => content);
}

// Mirrors getBuiltinLocator: resolves path + ".js" and reads it eagerly;
// the contents are only evaluated on first use (see getData above).
export function builtinRawLocator(p: string): RawBuiltinLocator {
  if (p === undefined) {
    console.error("Got undefined name in builtin locator");
    console.trace();
  }
  try {
    const fullPath = path.resolve(p + ".js");
    const content = fs.readFileSync(fullPath, 'utf8');
    return builtinRawLocatorFromStr(content);
  } catch (e) {
    console.error("Error in builtin locator: ", e);
    console.error("Path was: ", p);
    console.trace();
    throw e;
  }
}
