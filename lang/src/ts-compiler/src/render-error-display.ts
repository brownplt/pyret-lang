/*
  Ported from: src/arr/trove/render-error-display.arr
*/

import { InternalCompilerError } from './shared';
import * as ED from './error-display';
import * as S from './srcloc';

// lists.join-str-last: join with sep, but use last-sep between the final
// two elements.
function joinStrLast(strs: string[], sep: string, lastSep: string): string {
  if (strs.length === 0) { return ''; }
  if (strs.length === 1) { return strs[0]; }
  return strs.slice(0, -1).join(sep) + lastSep + strs[strs.length - 1];
}

// Pyret's global exn-unwrap: unwraps a runtime exception wrapper to the
// inner value; non-exception values pass through unchanged.
function exnUnwrap(val: any): any {
  if (val !== null && typeof val === 'object' && 'exn' in val) { return val.exn; }
  return val;
}

export function nthStackFrame(n: number, userFramesOnly: boolean, stack: S.Loc[]): S.Loc | undefined {
  const usableFrames = userFramesOnly ? stack.filter(S.isSrcloc) : stack;
  if (usableFrames.length > n) { return usableFrames[n]; }
  return undefined;
}

export function displayToString(e: ED.ErrorDisplay, embedDisplay: (val: any) => string, stack: S.Loc[]): string {
  const help = (x: ED.ErrorDisplay): string => displayToString(x, embedDisplay, stack);
  switch (e.$name) {
    case 'paragraph':
      return '\n' + e.contents.map(help).join('');
    case 'text':
      return e.str;
    case 'embed': {
      // Pyret: run-task(lam(): exn-unwrap(val).render-reason() end); on
      // success, render the result; if it raised, fall back to
      // embed-display(val).
      let rendered: ED.ErrorDisplay | undefined;
      let ok = false;
      try {
        rendered = exnUnwrap(e.val).renderReason();
        ok = true;
      } catch (_err) {
        ok = false;
      }
      if (ok) { return help(rendered!); }
      return embedDisplay(e.val);
    }
    case 'loc':
      return e.loc.format(true);
    case 'maybe-stack-loc': {
      const frame = nthStackFrame(e.n, e.userFramesOnly, stack);
      if (frame === undefined) { return help(e.contentsWithoutLoc); }
      return help(e.contentsWithLoc(frame));
    }
    case 'loc-display': {
      const contents = e.contents;
      if (ED.isLoc(contents)) {
        if (contents.loc.equals(e.loc)) { return help(contents); }
        return help(contents) + ' (at ' + e.loc.format(true) + ')';
      }
      return help(contents) + ' (at ' + e.loc.format(true) + ')';
    }
    case 'code':
      return '`' + help(e.contents) + '`';
    case 'h-sequence':
      return e.contents.filter((c) => !ED.isOptional(c)).map(help).join(e.sep);
    case 'h-sequence-sep':
      return joinStrLast(e.contents.filter((c) => !ED.isOptional(c)).map(help), e.sep, e.last);
    case 'v-sequence':
      return e.contents.filter((c) => !ED.isOptional(c)).map(help).join('\n');
    case 'bulleted-sequence':
      return e.contents.map((elt) => '* ' + help(elt)).join('\n');
    case 'optional':
      return '';
    case 'cmcode':
      return String(e.loc);
    case 'highlight':
      return help(new ED.LocDisplay(e.locs[0], '', e.contents));
    default:
      throw new InternalCompilerError(`displayToString: unknown ErrorDisplay ${(e as any).$name}`);
  }
}
