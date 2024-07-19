import { Srcloc, ExhaustiveSwitchError } from './common-runtime-types';
import * as runtime from './runtime';
import type * as ED_TYPES from '../runtime-arr/error-display.arr';
const ED = require('./error-display' + '.arr.js') as typeof ED_TYPES;

export function displayToString(e: ED_TYPES.ErrorDisplay, embedDisplay: (val: any) => string): string {
  const acc: string[] = [];
  displayToStringInternal(e, embedDisplay, [], acc);
  return acc.join("");
}

function displayToStringInternal(e: ED_TYPES.ErrorDisplay, embedDisplay: (val: any) => string, stack: Srcloc[], acc: string[]) {
  switch(e.$name) {
    case 'paragraph': {
      for (const c of e.contents) {
        displayToStringInternal(c, embedDisplay, stack, acc);
      }
      break;
    }
    case 'text': {
      acc.push(e.str);
      break;
    }
    case 'embed': {
      try {
        const val = e.val;
        if (val?.val?.['render-reason']?.full_meth) {
          const disp = (val.val['render-reason'].full_meth(val.val));
          displayToStringInternal(disp, embedDisplay, stack, acc);
        } else {
          acc.push(embedDisplay(e.val));
        }
      } catch (e) {
        acc.push(embedDisplay((e as any).val));
      }
      break;
    }
    case 'loc': {
      acc.push(e.loc.format(true));
      break;
    }
    case 'maybe-stack-loc': {
      displayToStringInternal(e['contents-without-loc'], embedDisplay, stack, acc);
      break;
    }
    case 'loc-display': {
      switch(e.contents.$name) {
        case 'loc': {
          if (runtime.$equalNow(e.loc, e.contents.loc)) {
            displayToStringInternal(e.contents, embedDisplay, stack, acc);
          } else {
            displayToStringInternal(e.contents, embedDisplay, stack, acc);
            acc.push("(at ", e.loc.format(true), ")");
          }
          break;
        }
        default: {
          displayToStringInternal(e.contents, embedDisplay, stack, acc);
          acc.push("(at ", e.loc.format(true), ")");
        }
      }
      break;
    }
    case 'code': {
      acc.push("`");
      displayToStringInternal(e.contents, embedDisplay, stack, acc);
      acc.push("`");
      break;
    }
    case 'h-sequence': {
      const contents = e.contents;
      contents.forEach((c, index) => {
        if (index > 0) { acc.push(e.sep); }
        displayToStringInternal(c, embedDisplay, stack, acc);
      });
      break;
    }
    case 'h-sequence-sep': {
      const contents = e.contents;
      contents.forEach((c, index) => {
        if (contents.length > 2 && index === contents.length - 2) { acc.push(e.last); }
        else if (index > 0) { acc.push(e.sep); }
        displayToStringInternal(c, embedDisplay, stack, acc);
      });
      break;
    }
    case 'v-sequence': {
      const contents = e.contents;
      contents.forEach((c, index) => {
        if (index > 0) { acc.push("\n"); }
        displayToStringInternal(c, embedDisplay, stack, acc);
      });
      break;
    }
    case 'bulleted-sequence': {
      const contents = e.contents;
      contents.forEach((c, index) => {
        if (index > 0) { acc.push("\n"); }
        acc.push("* ");
        displayToStringInternal(c, embedDisplay, stack, acc);
      });
      break;
    }
    case 'optional': break;
    case 'cmcode': {
      acc.push(e.loc.format(true));
      break;
    }
    case 'highlight': {
      displayToStringInternal(ED['loc-display'](e.locs[0], "", e.contents), embedDisplay, stack, acc);
      break;
    }
    default: throw new ExhaustiveSwitchError(e as never);
  }
}