import type * as ED from './error-display';
import type * as TCH from '../compiler/ts-codegen-helpers';
import type { List, PFunction } from '../compiler/ts-impl-types';
import type * as S from '../compiler/ts-srcloc';

export interface Exports {
  dict: { values: { dict: {
    'display-to-string': PFunction<(e: ED.ErrorDisplay, embedDisplay: PFunction<(val: any) => string>, stack: List<S.Srcloc>) => string>,
    'display-to-json': PFunction<(e: ED.ErrorDisplay, embedDisplay: PFunction<(val: any) => string>, stack: List<S.Srcloc>) => object>,
  }}}
}


({
  requires: [
    { 'import-type': 'builtin', name: 'error-display'},
    { 'import-type': 'dependency', protocol: 'js-file', args: ['ts-codegen-helpers']},
    { 'import-type': 'builtin', name: 'srcloc'},
  ],
  provides: { values: { 'display-to-string': 'tany', 'display-to-json': 'tany' } },
  nativeRequires: [],
  theModule: function(runtime, _, __, EDimp : ED.Exports, TCH: TCH.Exports, Simp: S.Exports) {
    const { ExhaustiveSwitchError, listToArray, formatSrcloc } = TCH;
    const ED = EDimp.dict.values.dict;
    const S = Simp.dict.values.dict;
    function nthStackFrame(n: number, userFramesOnly: boolean, stack: S.Srcloc[]): S.Srcloc | undefined {
      if (userFramesOnly) {
        return stack.filter((v) => S['is-srcloc'].app(v))[n]
      } else {
        return stack[n];
      }
    }
    function sameSrcloc(l1: S.Srcloc, l2: S.Srcloc): boolean {
      switch(l1.$name) {
        case 'builtin':
          return (l1.$name === l2.$name
            && l1.dict['module-name'] === l2.dict['module-name']);
        case 'srcloc':
          return (l1.$name === l2.$name
            && l1.dict.source === l2.dict.source
            && l1.dict['end-char'] === l2.dict['end-char']
            && l1.dict['end-column'] === l2.dict['end-column']
            && l1.dict['end-line'] === l2.dict['end-line']
            && l1.dict['start-char'] === l2.dict['start-char']
            && l1.dict['start-column'] === l2.dict['start-column']
            && l1.dict['start-line'] === l2.dict['start-line']);
      }      
    }
    function displayToStringInternal(e: ED.ErrorDisplay, embedDisplay: (val: any) => string, stack: S.Srcloc[], acc: string[]) {
      switch(e.$name) {
        case 'paragraph': {
          for (const c of listToArray(e.dict.contents)) {
            displayToStringInternal(c, embedDisplay, stack, acc);
          }
          break;
        }
        case 'text': {
          acc.push(e.dict.str);
          break;
        }
        case 'embed': {
          try {
            const val = e.dict.val;
            if (val?.dict?.val?.dict?.['render-reason']?.full_meth) {
              const disp = (val.dict.val.dict['render-reason'].full_meth(val.dict.val));
              displayToStringInternal(disp, embedDisplay, stack, acc);
            } else {
              acc.push(embedDisplay(e.dict.val));
            }
          } catch (e) {
            acc.push(embedDisplay(e.dict.val));
          }
          break;
        }
        case 'loc': {
          acc.push(formatSrcloc(e.dict.loc, true));
          break;
        }
        case 'maybe-stack-loc': {
          const maybeLoc = nthStackFrame(e.dict.n, e.dict['user-frames-only'], stack);
          if (maybeLoc) {
            displayToStringInternal(e.dict['contents-with-loc'].app(maybeLoc), embedDisplay, stack, acc);
          } else {
            displayToStringInternal(e.dict['contents-without-loc'], embedDisplay, stack, acc);
          }
          break;
        }
        case 'loc-display': {
          switch(e.dict.contents.$name) {
            case 'loc': {
              if (sameSrcloc(e.dict.loc, e.dict.contents.dict.loc)) {
                displayToStringInternal(e.dict.contents, embedDisplay, stack, acc);
              } else {
                displayToStringInternal(e.dict.contents, embedDisplay, stack, acc);
                acc.push("(at ", formatSrcloc(e.dict.loc, true), ")");
              }
              break;
            }
            default: {
              displayToStringInternal(e.dict.contents, embedDisplay, stack, acc);
              acc.push("(at ", formatSrcloc(e.dict.loc, true), ")");
            }
          }
          break;
        }
        case 'code': {
          acc.push("`");
          displayToStringInternal(e.dict.contents, embedDisplay, stack, acc);
          acc.push("`");
          break;
        }
        case 'h-sequence': {
          const contents = listToArray(e.dict.contents).filter((c) => !ED['is-optional'].app(c));
          contents.forEach((c, index) => {
            if (index > 0) { acc.push(e.dict.sep); }
            displayToStringInternal(c, embedDisplay, stack, acc);
          });
          break;
        }
        case 'h-sequence-sep': {
          const contents = listToArray(e.dict.contents).filter((c) => !ED['is-optional'].app(c));
          contents.forEach((c, index) => {
            if (contents.length > 2 && index === contents.length - 2) { acc.push(e.dict.last); }
            else if (index > 0) { acc.push(e.dict.sep); }
            displayToStringInternal(c, embedDisplay, stack, acc);
          });
          break;
        }
        case 'v-sequence': {
          const contents = listToArray(e.dict.contents).filter((c) => !ED['is-optional'].app(c));
          contents.forEach((c, index) => {
            if (index > 0) { acc.push("\n"); }
            displayToStringInternal(c, embedDisplay, stack, acc);
          });
          break;
        }
        case 'bulleted-sequence': {
          const contents = listToArray(e.dict.contents).filter((c) => !ED['is-optional'].app(c));
          contents.forEach((c, index) => {
            if (index > 0) { acc.push("\n"); }
            acc.push("* ");
            displayToStringInternal(c, embedDisplay, stack, acc);
          });
          break;
        }
        case 'optional': break;
        case 'cmcode': {
          acc.push(formatSrcloc(e.dict.loc, true));
          break;
        }
        case 'highlight': {
          displayToStringInternal(ED['loc-display'].app(e.dict.locs.dict.first, "", e.dict.contents), embedDisplay, stack, acc);
          break;
        }
        default: throw new ExhaustiveSwitchError(e);
      }
    }

    // https://github.com/microsoft/TypeScript/issues/1897
    type JSONPrim = string|number|null;
    type JSONObj = { [k: string]: JSONPrim|JSONObj|JSONArr };
    type JSONArr = Array<JSONPrim|JSONObj|JSONArr>;
    function serializeSrcLoc(loc: S.Srcloc): JSONObj {
      return { $name: loc.$name, ...loc.dict };
    }
    // Some of these maybe should be tagged but aren't and vice versa
    function displayToJSON(e: ED.ErrorDisplay, embedDisplay: (val: any) => string, stack: S.Srcloc[]): { $name: string } & JSONObj {
      const recur = (error: ED.ErrorDisplay) => displayToJSON(error, embedDisplay, stack);
      switch(e.$name) {
        case 'paragraph':
        // TODO(luna): filter is-optional?
        case 'v-sequence':
        case 'bulleted-sequence': {
          return { $name: e.$name, contents: listToArray(e.dict.contents).map(recur) };
        }
        case 'text': {
          return { $name: 'text', str: e.dict.str };
        }
        case 'embed': {
          try {
            const val = e.dict.val;
            if (val?.dict?.val?.dict?.['render-reason']?.full_meth) {
              const disp = (val.dict.val.dict['render-reason'].full_meth(val.dict.val));
              return displayToJSON(disp, embedDisplay, stack);
            } else {
              return { $name: 'embed', val: embedDisplay(e.dict.val) };
            }
            // Why is this an exception catch?
          } catch (e) {
            return { $name: 'embed', val: embedDisplay(e.dict.val) };
          }
        }
        case 'loc':
        case 'cmcode': {
          return { $name: e.$name, loc: serializeSrcLoc(e.dict.loc) };
        }
        case 'maybe-stack-loc': {
          const maybeLoc = nthStackFrame(e.dict.n, e.dict['user-frames-only'], stack);
          if (maybeLoc) {
            return displayToJSON(e.dict['contents-with-loc'].app(maybeLoc), embedDisplay, stack);
          } else {
            return displayToJSON(e.dict['contents-without-loc'], embedDisplay, stack);
          }
        }
        case 'loc-display': {
          if(e.dict.contents.$name === 'loc' && sameSrcloc(e.dict.loc, e.dict.contents.dict.loc)) {
            return displayToJSON(e.dict.contents, embedDisplay, stack);
          } else {
            return { $name: 'loc-display', 'contents': displayToJSON(e.dict.contents, embedDisplay, stack), loc: serializeSrcLoc(e.dict.loc) };
          }
        }
        case 'code':
        // TODO(luna): Why does optional originally give us nothing?
        case 'optional': {
          return { $name: e.$name, contents: recur(e.dict.contents) };
        }
        case 'h-sequence': {
          // TODO(luna): filter is-optional?
          return { $name: 'h-sequence', contents: listToArray(e.dict.contents).map(recur), sep: e.dict.sep };
        }
        case 'h-sequence-sep': {
          // TODO(luna): filter is-optional?
          return { $name: 'h-sequence-sep', contents: listToArray(e.dict.contents).map(recur), sep: e.dict.sep, last: e.dict.last };
        }
        case 'highlight': {
          return {
            $name: 'highlight',
            contents: recur(e.dict.contents),
            locs: listToArray(e.dict.locs).map(serializeSrcLoc),
            color: e.dict.color.valueOf(),
          };
        }
        default: throw new ExhaustiveSwitchError(e);
      }
    }

    function displayToString(e: ED.ErrorDisplay, embedDisplay: PFunction<(val: any) => string>, stack: List<S.Srcloc>): string {
      const ans: string[] = [];
      displayToStringInternal(e, embedDisplay.app, listToArray(stack), ans);
      return ans.join("");
    }

    function displayToJSONString(e: ED.ErrorDisplay, embedDisplay: PFunction<(val: any) => string>, stack: List<S.Srcloc>): string {
      return JSON.stringify(displayToJSON(e, embedDisplay.app, listToArray(stack)))
    }

    const exports : Exports['dict']['values']['dict'] = {
      'display-to-string': runtime.makeFunction(displayToString),
      'display-to-json': runtime.makeFunction(displayToJSONString),
    };
    return runtime.makeModuleReturn(exports, {});
  }
})