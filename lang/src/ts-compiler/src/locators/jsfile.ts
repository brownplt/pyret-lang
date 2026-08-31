/*
  Ported from: src/arr/compiler/locators/jsfile.arr
*/

import * as fs from 'fs';
import * as P from 'path';
import * as B from '../builtin-modules';
import * as BL from './builtin';
import * as CL from '../compile-lib';
import * as CM from '../compile-structs';
import * as JSP from '../js-of-pyret';
import { raise } from '../shared';

export const makeDep = BL.makeDep;
export const convertProvides = BL.convertProvides;
export const constDict = BL.constDict;

export function makeJsfileLocator(path: string): CL.Locator {
  const raw = B.builtinRawLocator(path);
  return {
    getUncached(): CL.Locator | undefined { return undefined; },
    needsCompile(_provides: Map<string, CM.Provides>): boolean { return false; },
    getModifiedTime(): number {
      return fs.statSync(path + ".js").mtimeMs;
    },
    getOptions(options: CM.CompileOptions): CM.CompileOptions {
      return { ...options, checkMode: false };
    },
    getModule(): CL.PyretCode {
      return raise("Should never fetch source for builtin module " + path);
    },
    getExtraImports(): CM.ExtraImports {
      return CM.standardImports;
    },
    getDependencies(): CM.AnyDependency[] {
      const deps = raw.getRawDependencies();
      return deps.map(makeDep);
    },
    getNativeModules(): CM.NativeModule[] {
      const natives = raw.getRawNativeModules();
      return natives.map((n) => new CM.Requirejs(n));
    },
    getGlobals(): CM.Globals {
      return CM.standardGlobals;
    },

    uri(): string { return "jsfile://" + P.resolve(path + ".js").split(P.sep).join("/"); },
    name(): string { return P.basename(path, ""); },

    setCompiled(_loadable: CM.Loadable, _provides: Map<string, CM.Provides>): void { return; },
    getCompiled(this: any): CM.Loadable | undefined {
      const provs = convertProvides(this.uri(), {
        uri: this.uri(),
        modules: raw.getRawModuleProvides(),
        values: raw.getRawValueProvides(),
        aliases: raw.getRawAliasProvides(),
        datatypes: raw.getRawDatatypeProvides()
      });
      return new CM.ModuleAsString(provs, CM.noBuiltins, CM.computedNone,
        CM.ok(new JSP.CCPFile(P.resolve(path + ".js"))));
    }
  };
}
