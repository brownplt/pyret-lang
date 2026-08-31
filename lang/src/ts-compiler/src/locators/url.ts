/*
  Ported from: src/arr/compiler/locators/url.arr

  Network fetch: the original uses the `fetch` trove (src/js/trove/fetch.js),
  an async cross-fetch with a 20s AbortSignal timeout resumed onto the Pyret
  stack. We have no Pyret stack to pause, but the dependency chase
  (CL.compileWorklist) awaits each module-finder step, so the fetch happens
  up front -- in the finder, before construction -- with an in-process
  `await fetch(...)`. The locator is then built around the already-fetched
  text and is fully synchronous, so the compiler backend stays pure/sync.
  The left/right payloads and error message strings mirror fetch.js exactly.
*/

import * as P from 'path';
import * as PP from '../parse-pyret';
import * as CL from '../compile-lib';
import * as CS from '../compile-structs';
import { Either, left, right, raise } from '../shared';

const FETCH_TIMEOUT = 20000;

// In-process async fetch. left(text) on success, right(message) on failure,
// like F.fetch. Called from the (async) module finder, so it can await
// directly -- no child process needed to fake a blocking fetch.
export async function fetchContent(url: string): Promise<Either<string, string>> {
  try {
    const result = await fetch(url, { signal: AbortSignal.timeout(FETCH_TIMEOUT) });
    if (result.ok) {
      return left(await result.text());
    } else {
      return right(`Fetching ${url} failed with status ${result.status}: ${result.statusText}`);
    }
  } catch (e) {
    return right(`Fetch of ${url} failed with an error. This may mean that the server you're fetching from does not support fetch requests from the browser, the URL has a formatting issue, or the request took longer than ${FETCH_TIMEOUT}ms. The system-level error was "${String(e)}"`);
  }
}

export type UrlLocator = CL.Locator & { url: string; globals: CS.Globals };

// Build a fully-synchronous locator around already-fetched source text.
function urlLocatorFromText(url: string, globals: CS.Globals, text: string): UrlLocator {
  const ast: CL.PyretCode = new CL.PyretAst(PP.surfaceParse(text, url));
  return {
    url: url,
    globals: globals,
    getUncached(): CL.Locator | undefined { return undefined; },
    getModifiedTime(): number { return Date.now(); },
    getOptions(options: CS.CompileOptions): CS.CompileOptions { return options; },
    getModule(): CL.PyretCode { return ast; },
    getDependencies(this: any): CS.AnyDependency[] {
      return CL.getStandardDependencies(this.getModule(), this.uri());
    },
    getNativeModules(): CS.NativeModule[] { return []; },
    getExtraImports(): CS.ExtraImports { return CS.standardImports; },
    getGlobals(this: any): CS.Globals { return this.globals; },
    setCompiled(_loadable: CS.Loadable, _provides: Map<string, CS.Provides>): void { return; },
    needsCompile(_provides: Map<string, CS.Provides>): boolean { return true; },
    getCompiled(): CS.Loadable | undefined { return undefined; },
    uri(this: any): string { return this.url; },
    name(this: any): string { return P.basename(this.url, ""); }
  };
}

// Fetch (via the injected fetcher) then construct the sync locator. The fetcher
// is a seam for tests; production wires in the in-process fetchContent above.
export function mockableUrlLocator(fetcher: { fetch(url: string): Promise<Either<string, string>> }): (url: string, globals: CS.Globals) => Promise<UrlLocator> {
  return async (url: string, globals: CS.Globals): Promise<UrlLocator> => {
    const result = await fetcher.fetch(url);
    if (result.$name === 'left') {
      return urlLocatorFromText(url, globals, result.v);
    } else {
      return raise("Error fetching " + url + ": " + result.v);
    }
  };
}

export const urlLocator = mockableUrlLocator({ fetch: fetchContent });
