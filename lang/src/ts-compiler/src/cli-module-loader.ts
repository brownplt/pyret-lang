/*
  Ported from: src/arr/compiler/cli-module-loader.arr

  Notes on ports of trove primitives used by the original:
  - crypto.sha256          -> node crypto sha256 hex digest (identical output)
  - Filesystem.stat.mtime  -> fs.statSync(...).mtimeMs (epoch ms, like
                              filesystem-internal.js)
  - Filesystem.resolve/join/is-absolute/dirname -> node path
  - URL.resolve            -> new URL(path, base).href (same as url trove)
  - JSON trove             -> JSON.parse / JSON.stringify

  Cache file format (verified against artifacts written by the Pyret-hosted
  compiler, e.g. /tmp/ptest/compiled/*.js): a module is cached as
    <name>-<sha256(uri)>-static.js : ({"theMap":...,"nativeRequires":[...],
                                       "provides":{...},"requires":[...]})
    <name>-<sha256(uri)>-module.js : same object plus "theModule"
  i.e. exactly CCPDict's print-js-static / print-js-runnable output; builtin
  pure-JS modules are cached single-file as <name>-<sha256(uri)>.js.
  set-loadable below writes precisely those bytes, so caches written here
  and by the Pyret-hosted pipeline are interchangeable.
*/

import { sha256 } from './sha256';
import * as fs from 'fs';
import * as os from 'os';
import * as P from 'path';
import { spawnSync } from 'child_process';
import * as A from './ast';
import * as J from './js-ast';
import * as C from './concat-lists';
import * as CL from './compile-lib';
import * as CS from './compile-structs';
import * as FL from './locators/file';
import * as UL from './locators/url';
import * as BL from './locators/builtin';
import * as JSF from './locators/jsfile';
import * as NPM from './locators/npm';
import * as JSP from './js-of-pyret';
import * as B from './builtin-modules';
import * as MS from './make-standalone';
import * as RED from './render-error-display';
import * as T from './type-structs';
import { Either, isLeft, mapGetValue, raise, TODOError } from './shared';

export type Loadable = CS.Loadable;

// Pyret torepr for the few values that end up in error messages here.
function toRepr(x: any): string {
  if (CS.isDependency(x)) {
    return 'dependency("' + x.protocol + '", [list: ' +
      x.arguments.map((a: string) => JSON.stringify(a)).join(', ') + '])';
  } else if (CS.isBuiltin(x)) {
    return 'builtin("' + x.modname + '")';
  } else if (x instanceof T.TypeBase) {
    // Some type errors ED.embed a Type directly (e.g. incorrect-number-of-args
    // embeds the applicant's TArrow). The .arr `torepr` renders these via the
    // data definition's _output -> "(Any, Any -> Number)"; the TypeBase
    // toString produces the identical form. Without this case the JSON
    // fallback below dumps the raw structure into the user's error message.
    return x.toString();
  } else {
    return JSON.stringify(x);
  }
}

export function uriToPath(uri: string, name: string): string {
  return name + "-" + sha256(uri);
}

// NOTE(joe): This is just a little one-off type to represent a simple
// situation: Builtin pure-JS files are stored in single files with a hash
// followed by .js, while builtin Pyret files are stored in two files – one with
// just static info and one with all the generated code. The CLI system needs to
// know which kind it is to look up the right cached files

export class Split {
  get $name(): 'split' { return 'split'; }
}
export class SingleFile {
  get $name(): 'single-file' { return 'single-file'; }
}
export type CachedType = Split | SingleFile;
export const split = new Split();
export const singleFile = new SingleFile();
export function isSplit(x: any): x is Split { return x instanceof Split; }
export function isSingleFile(x: any): x is SingleFile { return x instanceof SingleFile; }

// NOTE(joe): This has its arguments listed instead of taking a Locator because
// when we have cached, built standalones in releases, we need to do this check
// without constructing a locator that knows about the source. In that case,
// it's fine to pass a modified time of 0 to indicate that we're always happy
// with the compiled version of the file.

export function cachedAvailable(basedir: string, uri: string, name: string, modifiedTime: number): CachedType | undefined {
  const savedPath = P.join(basedir, uriToPath(uri, name));

  if (fs.existsSync(savedPath + "-static.js") &&
      (fs.statSync(savedPath + "-static.js").mtimeMs > modifiedTime)) {
    return split;
  } else if (fs.existsSync(savedPath + ".js") &&
      (fs.statSync(savedPath + ".js").mtimeMs > modifiedTime)) {
    return singleFile;
  } else {
    return undefined;
  }
}

export function getCached(basedir: string, uri: string, name: string, cacheType: CachedType): CL.Locator {
  const savedPath = P.join(basedir, uriToPath(uri, name));
  // NOTE(joe): leaving off .js because builtin-raw-locator below
  // expects no extension
  const staticPath = isSplit(cacheType) ? savedPath + "-static" : savedPath;
  const modulePath = isSplit(cacheType) ? savedPath + "-module" : savedPath;
  const raw = B.builtinRawLocator(staticPath);
  return {
    getUncached(): CL.Locator | undefined { return undefined; },
    needsCompile(_provides: Map<string, CS.Provides>): boolean { return false; },
    getModifiedTime(): number {
      return 0;
    },
    getOptions(options: CS.CompileOptions): CS.CompileOptions {
      return { ...options, checks: "none" };
    },
    getModule(): CL.PyretCode {
      return raise("Should never fetch source for builtin module " + staticPath);
    },
    getExtraImports(): CS.ExtraImports {
      return CS.standardImports;
    },
    getDependencies(): CS.AnyDependency[] {
      const deps = raw.getRawDependencies();
      return deps.map(CS.makeDep);
    },
    getNativeModules(): CS.NativeModule[] {
      const natives = raw.getRawNativeModules();
      return natives.map((n) => new CS.Requirejs(n));
    },
    getGlobals(): CS.Globals {
      return CS.standardGlobals;
    },

    uri(): string { return uri; },
    name(): string { return name; },

    setCompiled(_loadable: CS.Loadable, _provides: Map<string, CS.Provides>): void { return; },
    getCompiled(this: any): CS.Loadable | undefined {
      const provs = CS.providesFromRawProvides(this.uri(), {
        uri: this.uri(),
        values: raw.getRawValueProvides(),
        aliases: raw.getRawAliasProvides(),
        datatypes: raw.getRawDatatypeProvides(),
        modules: raw.getRawModuleProvides()
      });
      return new CS.ModuleAsString(provs, CS.noBuiltins, CS.computedNone,
        CS.ok(new JSP.CCPFile(P.resolve(modulePath + ".js"))));
    }
  };
}

export function getCachedIfAvailable(basedir: string, loc: CL.Locator): CL.Locator {
  return getCachedIfAvailableKnownMtimes(basedir, loc, new Map());
}

export function getCachedIfAvailableKnownMtimes(basedir: string, loc: CL.Locator, maxDepTimes: Map<string, number>): CL.Locator {
  const dependencyBasedMtime =
    maxDepTimes.has(loc.uri()) ? maxDepTimes.get(loc.uri())! : loc.getModifiedTime();
  const cachedType = cachedAvailable(basedir, loc.uri(), loc.name(), dependencyBasedMtime);
  if (cachedType === undefined) {
    const uncached = loc.getUncached !== undefined ? loc.getUncached() : undefined;
    if (uncached !== undefined) {
      return uncached;
    } else {
      return loc;
    }
  } else {
    return {
      ...getCached(basedir, loc.uri(), loc.name(), cachedType),
      getUncached(): CL.Locator | undefined { return loc; }
    };
  }
}

export function getFileLocator(basedir: string, realPath: string): CL.Locator {
  const loc = FL.fileLocator(realPath, CS.standardGlobals);
  return getCachedIfAvailable(basedir, loc);
}

export function getBuiltinLocator(basedir: string, readOnlyBasedirs: string[], modname: string): CL.Locator {
  const allDirs = readOnlyBasedirs;

  const firstAvailable = allDirs.find((rob) =>
    cachedAvailable(rob, "builtin://" + modname, modname, 0) !== undefined);
  if (firstAvailable === undefined) {
    const loc = BL.maybeMakeBuiltinLocator(modname);
    if (loc !== undefined) {
      return getCachedIfAvailable(basedir, loc);
    } else {
      return raise("Could not find builtin module " + modname + " in any of " + allDirs.join(", "));
    }
  } else {
    const ca = cachedAvailable(firstAvailable, "builtin://" + modname, modname, 0) ?? split;
    return getCached(firstAvailable, "builtin://" + modname, modname, ca);
  }
}

export function getBuiltinTestLocator(basedir: string, modname: string): CL.Locator {
  const loc: CL.Locator = {
    ...BL.makeBuiltinLocator(modname),
    uri(): string { return "builtin-test://" + modname; }
  };
  return getCachedIfAvailable(basedir, loc);
}

type ToCompile = CL.ToCompile;

export function getLoadable(basedir: string, readOnlyBasedirs: string[], l: ToCompile, maxDepTimes: Map<string, number>): Loadable | undefined {
  const locuri = l.locator.uri();
  const firstAvailable = [basedir, ...readOnlyBasedirs].find((rob) =>
    cachedAvailable(rob, l.locator.uri(), l.locator.name(), mapGetValue(maxDepTimes, locuri)) !== undefined);
  if (firstAvailable === undefined) {
    return undefined;
  } else {
    const c = cachedAvailable(firstAvailable, l.locator.uri(), l.locator.name(), mapGetValue(maxDepTimes, locuri));
    const savedPath = P.join(firstAvailable, uriToPath(locuri, l.locator.name()));
    const ct = c ?? singleFile;
    const staticPath = isSplit(ct) ? savedPath + "-static" : savedPath;
    const modulePath = isSplit(ct) ? savedPath + "-module.js" : savedPath + ".js";
    const rawStatic = B.builtinRawLocator(staticPath);
    const provs = CS.providesFromRawProvides(locuri, {
      uri: locuri,
      modules: rawStatic.getRawModuleProvides(),
      values: rawStatic.getRawValueProvides(),
      aliases: rawStatic.getRawAliasProvides(),
      datatypes: rawStatic.getRawDatatypeProvides()
    });
    return new CS.ModuleAsString(provs, CS.noBuiltins, CS.computedNone,
      CS.ok(new JSP.CCPFile(modulePath)));
  }
}

// Returns the module path of the cached file
export function setLoadable(basedir: string, locator: CL.Locator, loadable: Loadable): string {
  if (!fs.existsSync(basedir)) {
    fs.mkdirSync(basedir);
  }
  const locuri = loadable.provides.fromUri;
  const rp = loadable.resultPrinter;
  if (CS.isOk(rp)) {
    const ccp = rp.code;
    const saveStaticPath = P.join(basedir, uriToPath(locuri, locator.name()) + "-static.js");
    const saveModulePath = P.join(basedir, uriToPath(locuri, locator.name()) + "-module.js");
    const fsFd = fs.openSync(saveStaticPath, 'w');
    const fmFd = fs.openSync(saveModulePath, 'w');

    ccp.printJsRunnable((s: string) => { fs.writeSync(fmFd, s); });

    // NOTE(joe August 2017): This is a little bit dumb. When caching a file,
    // if we have enough information, split it into -static and -module
    // pieces.  If we don't have a dictionary of this information, save two
    // copies of it. We simply don't have enough metadata floating around to
    // make good decisions at fetch time. The copying is fairly innocuous,
    // because it only happens for hand-written JS files, which are smaller.
    // But this is a point to revisit.

    if (JSP.isCCPDict(ccp)) {
      ccp.printJsStatic((s: string) => { fs.writeSync(fsFd, s); });
    } else {
      ccp.printJsRunnable((s: string) => { fs.writeSync(fsFd, s); });
    }

    fs.fsyncSync(fsFd);
    fs.closeSync(fsFd);
    fs.fsyncSync(fmFd);
    fs.closeSync(fmFd);

    return saveModulePath;
  } else {
    return "";
  }
}

export interface CLIContext {
  currentLoadPath: string;
  cacheBaseDir: string;
  compiledReadOnlyDirs: string[];
  urlFileMode: CS.UrlFileMode;
  // Per-build cache of already-located url locators, keyed by full url. The
  // async finder fetches at construction, and the chase calls the finder once
  // per dependency *edge*, so without this a diamond-shaped url import graph
  // would fetch the same url twice. Shared by reference across the spread
  // copies of the context threaded through the chase. Absent => no caching
  // (only matters for url imports; local files are cheap to relocate).
  urlCache?: Map<string, CL.Locator>;
}

export function getRealPath(currentLoadPath: string, thisPath: string): string {
  if (P.isAbsolute(thisPath)) {
    return thisPath;
  } else {
    return P.join(currentLoadPath, thisPath);
  }
}

export function maybeAddSlash(s: string): string {
  const lastIndex = s.length - 1;
  if (s.charAt(lastIndex) === "/") {
    return s;
  } else {
    return s + "/";
  }
}

export function locateFile(ctxt: CLIContext, relPath: string): CL.Located<CLIContext> | undefined {
  const clp = ctxt.currentLoadPath;
  const realPath = getRealPath(clp, relPath);
  const newContext = { ...ctxt, currentLoadPath: P.dirname(realPath) };
  if (fs.existsSync(realPath)) {
    return new CL.Located(getFileLocator(ctxt.cacheBaseDir, realPath), newContext);
  } else {
    return undefined;
  }
}

// Locate (fetching up front) a url locator, memoized per build on ctxt.urlCache
// so a url reachable by multiple import edges is fetched once.
async function locateUrl(ctxt: CLIContext, url: string): Promise<CL.Located<CLIContext>> {
  const cached = ctxt.urlCache?.get(url);
  if (cached !== undefined) {
    return new CL.Located(cached, ctxt);
  }
  const locator = await UL.urlLocator(url, CS.standardGlobals);
  ctxt.urlCache?.set(url, locator);
  return new CL.Located(locator, ctxt);
}

export async function moduleFinder(ctxt: CLIContext, dep: CS.AnyDependency): Promise<CL.Located<CLIContext>> {
  if (CS.isDependency(dep)) {
    const protocol = dep.protocol;
    const args = dep.arguments;
    if (protocol === "file") {
      const located = locateFile(ctxt, args[0]);
      if (located !== undefined) {
        return located;
      } else {
        return raise("Cannot find import " + toRepr(dep));
      }
    } else if (protocol === "url") {
      return locateUrl(ctxt, dep.arguments[0]);
    } else if (protocol === "url-file") {
      const base = maybeAddSlash(args[0]);
      const fullUrl = new URL(args[1], base).href;
      if (CS.isAllRemote(ctxt.urlFileMode)) {
        return locateUrl(ctxt, fullUrl);
      } else if (CS.isAllLocal(ctxt.urlFileMode)) {
        const located = locateFile(ctxt, args[1]);
        if (located !== undefined) {
          const locatorWithUri = { ...located.locator, uri(): string { return fullUrl; } };
          return new CL.Located(locatorWithUri, located.context);
        } else {
          return raise("Cannot find import " + toRepr(dep));
        }
      } else if (CS.isLocalIfPresent(ctxt.urlFileMode)) {
        const located = locateFile(ctxt, args[1]);
        if (located !== undefined) {
          const locatorWithUri = { ...located.locator, uri(): string { return fullUrl; } };
          return new CL.Located(locatorWithUri, located.context);
        } else {
          return locateUrl(ctxt, fullUrl);
        }
      } else {
        return raise("Unknown url-file-mode");
      }
    } else if (protocol === "npm") {
      const packageName = args[0];
      const path = args[1];
      const locator = NPM.makeNpmLocator(packageName, path, ctxt.currentLoadPath);
      const clp = ctxt.currentLoadPath;
      const realPath = getRealPath(clp, locator.path);
      const newContext = { ...ctxt, currentLoadPath: P.dirname(realPath) };
      return new CL.Located(locator, newContext);
    } else if (protocol === "builtin-test") {
      const l = getBuiltinTestLocator(ctxt.cacheBaseDir, args[0]);
      const forceCheckMode = {
        ...l,
        getOptions(options: CS.CompileOptions): CS.CompileOptions {
          return { ...options, checks: "all", typeCheck: false };
        }
      };
      return new CL.Located(forceCheckMode, ctxt);
    } else if (protocol === "file-no-cache") {
      const clp = ctxt.currentLoadPath;
      const realPath = getRealPath(clp, args[0]);
      const newContext = { ...ctxt, currentLoadPath: P.dirname(realPath) };
      if (fs.existsSync(realPath)) {
        return new CL.Located(FL.fileLocator(realPath, CS.standardGlobals), newContext);
      } else {
        return raise("Cannot find import " + toRepr(dep));
      }
    } else if (protocol === "js-file") {
      const clp = ctxt.currentLoadPath;
      const realPath = getRealPath(clp, args[0]);
      const newContext = { ...ctxt, currentLoadPath: P.dirname(realPath) };
      const locator = JSF.makeJsfileLocator(realPath);
      return new CL.Located(locator, newContext);
    } else {
      return raise("Unknown import type: " + protocol);
    }
  } else {
    return new CL.Located(
      getBuiltinLocator(ctxt.cacheBaseDir, ctxt.compiledReadOnlyDirs, dep.modname), ctxt);
  }
}

export const defaultStartContext: CLIContext = {
  currentLoadPath: P.resolve("./"),
  cacheBaseDir: P.resolve("./compiled"),
  compiledReadOnlyDirs: [],
  urlFileMode: CS.allRemote
};

export const defaultTestContext: CLIContext = {
  currentLoadPath: P.resolve("./"),
  cacheBaseDir: P.resolve("./tests/compiled"),
  compiledReadOnlyDirs: [],
  urlFileMode: CS.allRemote
};

export async function compile(path: string, options: CS.CompileOptions): Promise<{ loadables: Loadable[]; modules: Map<string, Loadable> }> {
  const baseModule = new CS.Dependency("file", [path]);
  const base = await moduleFinder({
    currentLoadPath: P.resolve(options.baseDir),
    cacheBaseDir: options.compiledCache,
    compiledReadOnlyDirs: options.compiledReadOnly.map((d) => P.resolve(d)),
    urlFileMode: options.urlFileMode,
    urlCache: new Map()
  }, baseModule);
  const wl = await CL.compileWorklist(moduleFinder, base.locator, base.context);
  const compiled = CL.compileProgram(wl, options);
  return compiled;
}

export function handleCompilationErrors(problems: any[], options: CS.CompileOptions): never {
  for (const e of problems) {
    options.logError(RED.displayToString(e.renderReason(), toRepr, []));
    options.logError("\n");
  }
  return raise("There were compilation errors");
}

export function propagateExit(_result: any): void {
  // The Pyret original inspects load-lib ModuleResults for exit()/exit-quiet()
  // and terminates the process. In-process realm execution is not portable
  // without the Pyret runtime FFI; `run` below executes the standalone in a
  // child process instead, so exit codes propagate through spawnSync and this
  // helper is never needed.
  throw new TODOError("propagate-exit: in-process load-lib execution is not ported");
}

/*
  DEVIATION from the Pyret original: the original runs the program in-process
  through runtime-lib/load-lib realms (L.run-program on the generated source).
  That machinery is the Pyret runtime FFI and is not portable here. Instead we
  build a runnable standalone to a temporary file (the same code path as
  build-runnable-standalone, with a default config) and execute it with
  node in a child process, forwarding stdio. Check results / error messages
  are rendered by the standalone itself (handalone.js) directly onto the
  inherited stdio, so `message` is empty here; `exitCode` is the child's exit
  status. Note that the program sees the temp .jarr path (not the source
  path) as its zeroth command-line argument.
*/
export async function run(path: string, options: CS.CompileOptions, subsequentCommandLineArguments: string[]): Promise<{ message: string; exitCode: number }> {
  const stats = new Map<string, any>();
  const maybeProgram = await buildProgram(path, options, stats);
  if (isLeft(maybeProgram)) {
    return handleCompilationErrors((maybeProgram as any).v, options);
  } else {
    const program = (maybeProgram as any).v;
    const tmpDir = fs.mkdtempSync(P.join(os.tmpdir(), "pyret-run-"));
    const outfile = P.join(tmpDir, "program.jarr");
    const config = {
      baseUrl: options.compiledCache,
      out: outfile,
      "use-raw-files": true,
      "raw-js": {}
    };
    MS.makeStandalone(program.natives, program.jsAst, JSON.stringify(config), options);
    const res = spawnSync(process.execPath, [outfile, ...subsequentCommandLineArguments], {
      stdio: 'inherit'
    });
    const exitCode = res.status === null ? 1 : res.status;
    return { message: "", exitCode: exitCode };
  }
}

/*
  Returns the program as a JavaScript AST of module list and dependency map,
  and its native dependencies as a list of strings
*/
export async function buildProgram(
  path: string,
  options: CS.CompileOptions,
  stats: Map<string, any>
): Promise<Either<any[], { jsAst: any; natives: string[] }>> {
  const printProgressClearing = (s: string, toClear: number | undefined) => {
    if (options.displayProgress) {
      options.log(s, toClear);
    }
  };
  const printProgress = (s: string) => printProgressClearing(s, undefined);
  let str = "Gathering dependencies...";
  const clearAndPrint = (newStr: string) => {
    printProgressClearing(newStr, str.length);
    str = newStr;
  };
  printProgress(str);
  const baseModule = new CS.Dependency("file", [path]);
  const base = await moduleFinder({
    currentLoadPath: P.resolve(options.baseDir),
    cacheBaseDir: options.compiledCache,
    compiledReadOnlyDirs: options.compiledReadOnly.map((d) => P.resolve(d)),
    urlFileMode: options.urlFileMode,
    urlCache: new Map()
  }, baseModule);
  clearAndPrint("Compiling worklist...");
  let wl: ToCompile[] = await CL.compileWorklist(moduleFinder, base.locator, base.context);

  const maxDepTimes = CL.depTimesFromWorklist(wl);

  wl = wl.map((located) => ({
    ...located,
    locator: getCachedIfAvailableKnownMtimes(options.compiledCache, located.locator, maxDepTimes)
  }));

  clearAndPrint("Loading existing compiled modules...");

  const starterModules = CL.modulesFromWorklist(wl, (t, mdt) =>
    getLoadable(options.compiledCache, options.compiledReadOnly.map((d) => P.resolve(d)), t, mdt));

  const cachedModules = starterModules.size;
  const totalModules = wl.length - cachedModules;
  let numCompiled = 0;
  if (totalModules === 0) {
    clearAndPrint("All modules already compiled. Cleaning up and generating standalone...\n");
  }
  const extendedOptions: CS.CompileOptions = {
    ...options,
    shouldProfile: (locator: any) => {
      return options.addProfiling && (locator.uri() === base.locator.uri());
    },
    beforeCompile: (locator: any) => {
      numCompiled = numCompiled + 1;
      clearAndPrint("Compiling " + String(numCompiled) + "/" + String(totalModules)
        + ": " + locator.name());
    },
    onCompile: (locator: any, loadable: Loadable, trace: any) => {
      locator.setCompiled(loadable, new Map()); // TODO(joe): What are these supposed to be?
      clearAndPrint(String(numCompiled) + "/" + String(totalModules)
        + " modules compiled " + "(" + locator.name() + ")");
      if (options.collectTimes) {
        const comp = (trace as Array<{ name: string; time: number }>).map(
          (stage) => stage.name + ": " + String(stage.time) + "ms");
        stats.set(locator.name(), comp);
      }
      if (numCompiled === totalModules) {
        printProgress("\nCleaning up and generating standalone...\n");
      }
      const modulePath = setLoadable(options.compiledCache, locator, loadable);
      if ((numCompiled === totalModules) && options.collectAll) {
        // Don't squash the final JS-AST if we're collecting all of them, so
        // it can be pretty-printed after all
        return loadable;
      } else {
        if (CS.isModuleAsString(loadable)) {
          return new CS.ModuleAsString(
            loadable.provides, loadable.compileEnv, loadable.postCompileEnv,
            CS.ok(new JSP.CCPFile(modulePath)));
        } else {
          return loadable;
        }
      }
    }
  };
  const ans = CL.compileStandalone(wl, starterModules, extendedOptions);
  return ans;
}

export async function buildRunnableStandalone(path: string, requireConfigPath: string, outfile: string, options: CS.CompileOptions): Promise<boolean> {
  const stats = new Map<string, any>();
  const config = JSON.parse(fs.readFileSync(requireConfigPath, 'utf8'));
  const tb = config["typable-builtins"];
  if (tb !== undefined) {
    if (Array.isArray(tb)) {
      BL.setTypableBuiltins(tb);
    } else {
      raise("Expected a list for typable-builtins, but got: " + JSON.stringify(tb));
    }
  }
  const maybeProgram = await buildProgram(path, options, stats);
  if (isLeft(maybeProgram)) {
    return handleCompilationErrors((maybeProgram as any).v, options);
  } else {
    const program = (maybeProgram as any).v;

    config["out"] = getRealPath(options.baseDir, outfile);
    if (!("baseUrl" in config)) {
      config["baseUrl"] = options.compiledCache;
    }

    if (options.collectTimes) { stats.set("standalone", Date.now()); }
    const makeStandaloneRes = MS.makeStandalone(program.natives, program.jsAst,
      JSON.stringify(config), options);

    const htmlRes = options.htmlFile !== undefined
      ? MS.makeHtmlFile(outfile, options.htmlFile)
      : true;

    const ans = makeStandaloneRes && htmlRes;

    if (options.collectTimes) {
      const standaloneEnd = Date.now() - (stats.get("standalone") as number);
      stats.set("standalone", ["Outputing JS: " + String(standaloneEnd) + "ms"]);
      for (const key of stats.keys()) {
        process.stdout.write(key + ": \n" + (stats.get(key) as string[]).join(", \n") + "\n" + "\n");
      }
    }
    return ans;
  }
}

export async function buildRequireStandalone(path: string, options: CS.CompileOptions): Promise<void> {
  const stats = new Map<string, any>();
  // NOTE: faithful to the Pyret original, which reads .natives/.js-ast
  // directly off the Either returned by build-program (and passes the raw
  // native strings where j-exprs are expected, and one argument too few to
  // j-fun); this function is bit-rotted upstream and ported structurally.
  const program: any = await buildProgram(path, options, stats);

  const natives = new J.JList(true, C.mapList((n: any) => n, program.natives));

  const defineName = new J.JId(new A.SName(A.dummyLoc, "define"));

  const prog = new J.JBlock(C.clist<any>(
    new J.JApp(defineName, C.clist<any>(natives,
      new J.JFun(J.nextJFunId(), "", C.clist(),
        new J.JBlock(C.clist<any>(
          new J.JReturn(program.jsAst)
        )))
    ))
  ));

  process.stdout.write(prog.toUglySource() + "\n");
}
