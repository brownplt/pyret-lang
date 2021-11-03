import { Location, Range } from 'vscode-languageserver-types';
import { Variant } from '../../../../src/arr/compiler/ts-codegen-helpers';
import { List, Runtime } from '../../../../src/arr/compiler/ts-impl-types';
import { Srcloc } from '../../../../src/arr/compiler/ts-srcloc';
import { Nonterm, ParserCST, Token } from './external-types';
import NumberTree from 'node-interval-tree';

export type Module<T> = { dict: { 'provide-plus-types': T; }; };
export function unpackModule<T>(module: Module<{ dict: { values: { dict: T; }; }; }>): T {
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
  });
}

export function locationFromSrcloc(s: Variant<Srcloc, 'srcloc'>): Location {
  return {
    uri: s.dict.source,
    range: rangeFromSrcloc(s)
  };
}

export function rangeFromSrcloc(s: Variant<Srcloc, 'srcloc'>): Range {
  return {
    start: {
      line: s.dict['start-line'] - 1,
      character: s.dict['start-column'],
    },
    end: {
      line: s.dict['end-line'] - 1,
      character: s.dict['end-column'],
    }
  };
}

export class IntervalTree<P, T> {
  tree: NumberTree<T>;
  map: (p: P) => number;
  constructor(map: (p: P) => number) {
    this.tree = new NumberTree();
    this.map = map;
  }
  
  insert(low: P, high: P, data: T): boolean {
    return this.tree.insert(this.map(low), this.map(high), data);
  }

  remove(low: P, high: P, data: T): boolean {
    return this.tree.remove(this.map(low), this.map(high), data);
  }

  search(low: P, high?: P): T[] {
    return this.tree.search(this.map(low), this.map(high ? high : low));
  }

  inOrder() { return this.tree.inOrder() }
  preOrder() { return this.tree.inOrder() }
  get count() { return this.tree.count }
}