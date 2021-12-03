import { Srcloc } from '../../../../src/arr/compiler/ts-ast';
import { rangeFromSrcloc } from './util';
import { Position } from 'vscode-languageserver-types';
import { TextDocument } from 'vscode-languageserver-textdocument';
import { Documents } from './document-manager';
import { Runtime } from '../../../../src/arr/compiler/ts-impl-types';
import type * as TCH from '../../../../src/arr/compiler/ts-codegen-helpers';
import NumberTree from 'node-interval-tree';
import { BindInfo } from '../../../../src/arr/compiler/ts-compile-structs';

export function makeIntervalTree(doc: TextDocument, names: [string, BindInfo[]][]) {
	const tree = new IntervalTree<Position, [string, BindInfo]>((pos) => doc.offsetAt(pos));

	for (const [name, locs] of names) {
		for (const loc of locs) {
			if (loc.dict.loc.$name !== 'srcloc') continue;
			const range = rangeFromSrcloc(loc.dict.loc);
			tree.insert(range.start, range.end, [name, loc]);
		}
	}
	return tree;
}

export async function bestKey(runtime: Runtime, documents: Documents, uri: string, pos: Position) {
	return (await documents.getIntervalTree(runtime, uri)).search(pos)
		.map(([key, loc]): [string, number] => [key, loc.dict['end-char'] - loc.dict['start-char']])
		.reduce((prev, curr) => prev[1] < curr[1] ? prev : curr, ["", Infinity])[0];
}

export async function getBindings(runtime: Runtime, documents: Documents, key: string, uri: string) {
  const TCH: TCH.Exports = runtime.modules['jsfile://pyret-lang/src/arr/compiler/ts-codegen-helpers.js'].jsmod;
  const nameResolution = (await documents.getTrace(runtime, uri))[4].dict.result
	const env = nameResolution.dict.env;
  const bindings = TCH.mapFromMutableStringDict(env.dict.bindings);
  const modBindings = TCH.mapFromMutableStringDict(env.dict['module-bindings']);
  const typeBindings = TCH.mapFromMutableStringDict(env.dict['type-bindings']);
  return bindings.get(key) ?? modBindings.get(key) ?? typeBindings.get(key);
}

export async function getLocations(runtime:Runtime, documents: Documents, uri: string) {
  const TCH: TCH.Exports = runtime.modules['jsfile://pyret-lang/src/arr/compiler/ts-codegen-helpers.js'].jsmod;
  const trace = await documents.getTrace(runtime, uri);
  return TCH.mapFromMutableStringDict(trace[4].dict.result.dict.env.dict['lsp-binding-info']);
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
    return this.tree.search(this.map(low), this.map(high ?? low));
  }

  inOrder() { return this.tree.inOrder() }
  preOrder() { return this.tree.preOrder() }
  get count() { return this.tree.count }
}