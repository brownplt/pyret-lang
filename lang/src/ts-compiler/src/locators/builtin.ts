/*
  Ported from: src/arr/compiler/locators/builtin.arr

  NOTE(joe): These conversions are done in Pyret-land because
  "builtin-modules" in JS is as spartan as possible to make module load-order
  dependencies as painless as possible; if importing builtin modules required
  access to compile-lib and sets and so on, it would be very difficult to
  bootstrap things.  So make-dep and make-provides handle this transition.

  Locators are object literals (as in the Pyret original); methods that the
  original wrote with `self` use `this` so that object-extension (spread plus
  method override, e.g. get-builtin-test-locator's uri override) rebinds the
  way Pyret's `obj.{ ... }` does.
*/

import * as fs from 'fs';
import * as P from 'path';
import * as B from '../builtin-modules';
import * as PP from '../parse-pyret';
import * as CL from '../compile-lib';
import * as CM from '../compile-structs';
import * as JSP from '../js-of-pyret';
import { raise } from '../shared';

export const makeDep = CM.makeDep;

export function convertProvides(uri: string, provides: any): CM.Provides {
  return CM.providesFromRawProvides(uri, provides);
}

export function constDict<A>(strs: string[], val: A): Map<string, A> {
  const d = new Map<string, A>();
  for (const s of strs) {
    d.set(s, val);
  }
  return d;
}

let builtinJsDirs: string[] = ["src/js/trove/"];
let builtinArrDirs: string[] = ["src/arr/trove/"];
let typableBuiltins: string[] = [];
let allowBuiltinOverrides = false;

export function setBuiltinJsDirs(paths: string[]): void {
  builtinJsDirs = paths;
}

export function setBuiltinArrDirs(paths: string[]): void {
  builtinArrDirs = paths;
}

export function setAllowBuiltinOverrides(flag: boolean): void {
  allowBuiltinOverrides = flag;
}

export function setTypableBuiltins(uris: string[]): void {
  typableBuiltins = uris;
}

export function makeBuiltinJsLocator(basedir: string, builtinName: string): CL.Locator {
  const raw = B.builtinRawLocator(P.join(basedir, builtinName));
  return {
    needsCompile(_provides: Map<string, CM.Provides>): boolean { return false; },
    getUncached(): CL.Locator | undefined { return undefined; },
    getModifiedTime(): number {
      return fs.statSync(P.join(basedir, builtinName + ".js")).mtimeMs;
    },
    getOptions(options: CM.CompileOptions): CM.CompileOptions {
      return { ...options, checkMode: false, typeCheck: false };
    },
    getModule(): CL.PyretCode {
      return raise("Should never fetch source for builtin module " + builtinName);
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

    uri(): string { return "builtin://" + builtinName; },
    name(): string { return builtinName; },

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
        CM.ok(new JSP.CCPFile(P.join(basedir, builtinName + ".js"))));
    }
  };
}

export function makeBuiltinArrLocator(basedir: string, builtinName: string): CL.Locator {
  const path = P.join(basedir, builtinName + ".arr");
  let ast: CL.PyretCode | undefined = undefined;
  return {
    getModifiedTime(): number {
      return fs.statSync(path).mtimeMs;
    },
    getUncached(): CL.Locator | undefined { return undefined; },
    getOptions(this: any, options: CM.CompileOptions): CM.CompileOptions {
      const typeCheck = typableBuiltins.includes(this.uri());
      return { ...options, checkMode: false, typeCheck: typeCheck };
    },
    getModule(this: any): CL.PyretCode {
      if (ast === undefined) {
        if (!fs.existsSync(path)) {
          raise("File " + path + " does not exist");
        }
        ast = new CL.PyretAst(PP.surfaceParse(fs.readFileSync(path, 'utf8'), this.uri()));
      }
      return ast;
    },
    getDependencies(this: any): CM.AnyDependency[] {
      return CL.getDependencies(this.getModule(), this.uri());
    },
    getNativeModules(): CM.NativeModule[] {
      return [];
    },
    getExtraImports(): CM.ExtraImports {
      return CM.minimalImports;
    },
    getGlobals(): CM.Globals {
      return CM.standardGlobals;
    },
    setCompiled(_loadable: CM.Loadable, _provides: Map<string, CM.Provides>): void {
      ast = undefined;
    },
    needsCompile(_provides: Map<string, CM.Provides>): boolean {
      // does not handle provides from dependencies currently
      // NOTE(joe): Until we serialize provides correctly, just return false here
      const cpath = path + ".js";
      if (fs.existsSync(path) && fs.existsSync(cpath)) {
        const stimes = fs.statSync(path);
        const ctimes = fs.statSync(cpath);
        return ctimes.mtimeMs <= stimes.mtimeMs;
      } else {
        return true;
      }
    },
    getCompiled(this: any): CM.Loadable | undefined {
      const cpath = path + ".js";
      if (fs.existsSync(path) && fs.existsSync(cpath)) {
        // NOTE(joe):
        // Since we're not explicitly acquiring locks on files, there is a race
        // condition in the next few lines – a user could potentially delete or
        // overwrite the original file for the source while this method is
        // running.  We can explicitly open and lock files with appropriate
        // APIs to mitigate this in the happy, sunny future.
        const stimes = fs.statSync(path);
        const ctimes = fs.statSync(cpath);
        if (ctimes.mtimeMs > stimes.mtimeMs) {
          const raw = B.builtinRawLocator(path);
          // NOTE: as in the Pyret original, the raw record here has no
          // `modules` field (and the original also passes one argument too
          // few to module-as-string); this branch errors upstream if ever
          // exercised. We pass computedNone so the call is well-formed.
          const provs = convertProvides(this.uri(), {
            uri: this.uri(),
            values: raw.getRawValueProvides(),
            aliases: raw.getRawAliasProvides(),
            datatypes: raw.getRawDatatypeProvides()
          });
          return new CM.ModuleAsString(provs, CM.noBuiltins, CM.computedNone,
            CM.ok(new JSP.CCPFile(cpath)));
        } else {
          return undefined;
        }
      } else {
        return undefined;
      }
    },
    uri(): string { return "builtin://" + builtinName; },
    name(): string { return builtinName; }
  };
}

export function maybeMakeBuiltinLocator(builtinName: string): CL.Locator | undefined {
  const matchingArrFiles: string[] = [];
  for (const p of builtinArrDirs) {
    const fullPath = P.join(p, builtinName + ".arr");
    if (fs.existsSync(fullPath)) {
      matchingArrFiles.push(fullPath);
    }
  }
  const matchingJsFiles: string[] = [];
  for (const p of builtinJsDirs) {
    const fullPath = P.join(p, builtinName + ".js");
    if (fs.existsSync(fullPath)) {
      matchingJsFiles.push(fullPath);
    }
  }
  if (!allowBuiltinOverrides) {
    if (matchingArrFiles.length > 1) {
      raise("The module " + builtinName + " is defined in several locations: " +
        matchingArrFiles.join(", ") + ".  Use --allow-builtin-overrides to permit this.");
    }
    if (matchingJsFiles.length > 1) {
      raise("The module " + builtinName + " is defined in several locations: " +
        matchingJsFiles.join(", ") + ".  Use --allow-builtin-overrides to permit this.");
    }
    if (matchingArrFiles.length > 0 && matchingJsFiles.length > 0) {
      raise("The module " + builtinName + " is defined in several locations: " +
        [...matchingArrFiles, ...matchingJsFiles].join(", ") + ".  Use --allow-builtin-overrides to permit this.");
    }
  }
  if (matchingArrFiles.length > 0) {
    return makeBuiltinArrLocator(P.dirname(matchingArrFiles[0]), builtinName);
  } else if (matchingJsFiles.length > 0) {
    return makeBuiltinJsLocator(P.dirname(matchingJsFiles[0]), builtinName);
  } else {
    return undefined;
  }
}

export function makeBuiltinLocator(builtinName: string): CL.Locator {
  const maybe = maybeMakeBuiltinLocator(builtinName);
  if (maybe === undefined) {
    return raise("Could not find module " + builtinName + " in any of " +
      [...builtinArrDirs, ...builtinJsDirs].join(", "));
  } else {
    return maybe;
  }
}
