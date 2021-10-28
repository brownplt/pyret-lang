import { Position } from 'vscode-languageserver';
import { Location } from 'vscode-languageserver-types';
import { Variant } from '../../../../src/arr/compiler/ts-codegen-helpers';
import { List, Runtime } from '../../../../src/arr/compiler/ts-impl-types';
import { Srcloc } from '../../../../src/arr/compiler/ts-srcloc';
import { Nonterm, ParserCST, Token } from './external-types';

export type Module<T> = { dict: { 'provide-plus-types': T } };
export function unpackModule<T>(module: Module<{ dict: { values: { dict: T } } }>): T {
  return module.dict["provide-plus-types"].dict.values.dict;
}

export function cstWalk(cst: ParserCST, leaf: (tok: Token, path: Nonterm[]) => void) {
  const path: Nonterm[] = [];
  function helper(cst: ParserCST) {
    if ("kids" in cst) {
      path.push(cst);
      cst.kids.forEach((kid) => helper(kid));
      path.pop();
    } else {
      leaf(cst, path);
    }
  }
  helper(cst);
}

export function peek<A>(arr: A[]): A { return arr[arr.length - 1]; }
export function hasTop<A>(arr: A[], wanted: A | A[]): boolean {
  if (wanted instanceof Array) {
    for (let i = 0; i < wanted.length; i++) {
      if (arr[arr.length - 1 - i] !== wanted[i]) {
        return false;
      }
    }
    return true;
  } else {
    return arr[arr.length - 1] === wanted;
  }
}

export function listToArray<T>(list: List<T>): T[] {
  const ret = [];
  let cur = list;
  while (cur.$name === 'link') {
    ret.push(cur.dict.first);
    cur = cur.dict.rest;
  }
  return ret;
}

export function deleteStart(source: string, toRemove: string) {
  return source.substring(source.startsWith(toRemove) ? toRemove.length : 0);
}

export function deleteEnd(source: string, toRemove: string) {
  return source.substring(0, source.length - (source.endsWith(toRemove) ? toRemove.length : 0));
}

export function pyretCall<A>(runtime: Runtime, thunk: () => A): Promise<A> {
  return new Promise((resolve, reject) => {
    runtime.runThunk(thunk, (result) => {
      if (runtime.isSuccessResult(result)) {
        resolve(result.result);
      } else {
        reject(result.exn);
      }
    });
  })
}

export function locationFromSrcloc(s: Variant<Srcloc, 'srcloc'>): Location {
  return {
    uri: s.dict.source,
    range: {
      start: {
        line: s.dict['start-line'] - 1,
        character: s.dict['start-column'],
      },
      end: {
        line: s.dict['end-line'] - 1,
        character: s.dict['end-column'],
      }
    }
  }
}

export function slocContains(loc: Variant<Srcloc, 'srcloc'>, pos: Position): boolean {
  // +1 to fix indexing between pos and srcloc
  return (pos.line + 1) >= loc.dict['start-line']
    && (pos.line + 1) <= loc.dict['end-line']
    && (pos.character) >= loc.dict['start-column']
    && (pos.character) <= loc.dict['end-column'];
}

export function slocLte(a: Variant<Srcloc, 'srcloc'>, b: Variant<Srcloc, 'srcloc'>) {
  if (!a) return false;
  if (!b) return true;
  return (a.dict['end-char'] - a.dict['start-char']) <= (b.dict['end-char'] - b.dict['start-char']);
}