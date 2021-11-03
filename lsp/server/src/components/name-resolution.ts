import type * as TCH from '../../../../src/arr/compiler/ts-codegen-helpers';
import type * as TCSH from '../../../../src/arr/compiler/ts-compile-structs-helpers';
import type * as A from '../../../../src/arr/compiler/ts-ast';
import { Runtime } from '../../../../src/arr/compiler/ts-impl-types';
import { BindOrigin, NameResolution } from '../../../../src/arr/compiler/ts-compile-structs';
import { Srcloc } from '../../../../src/arr/compiler/ts-ast';
import { IntervalTree, rangeFromSrcloc } from './util';
import { Position } from 'vscode-languageserver-types';
import { TextDocument } from 'vscode-languageserver-textdocument';

function getNames(runtime: Runtime, nameResolution: NameResolution) {
	const TCH: TCH.Exports = runtime.modules['jsfile://pyret-lang/src/arr/compiler/ts-codegen-helpers.js'].jsmod;
	const TCSH: TCSH.Exports = runtime.modules['jsfile://pyret-lang/src/arr/compiler/ts-compile-structs-helpers.js'].jsmod;
  const names = new Map<string, Srcloc[]>();
  
  const nameVisitor: TCH.Visitor<A.Name | A.Expr, void, Srcloc> = {
		's-atom': (_, atom, loc) => {
			const key = TCSH.callMethod(atom, 'key');
			if (!names.has(key))
				names.set(key, []);
			names.get(key).push(loc);
		},
		's-module': (visitor, mod, loc) => {
			TCH.visit(visitor, mod.dict.answer, loc, locExtractor);
		}
	}

	const locExtractor = (d: TCH.PyretDataValue, l: Srcloc) => {
		return d.dict?.l ?? l;
	}

  TCH.visit(nameVisitor, nameResolution.dict.ast, undefined, locExtractor);
	return names;
}

function makeIntervalTree(doc: TextDocument, names: [string, Srcloc[]][]) {
	const tree = new IntervalTree<Position, [string, Srcloc]>(doc.offsetAt);

	for (let [name, locs] of names) {
		for (let loc of locs) {
			if (loc.$name !== 'srcloc') continue;
			const range = rangeFromSrcloc(loc);
			tree.insert(range.start, range.end, [name, loc]);
		}
	}
	return tree;
}

export function getKeyAtPos(runtime: Runtime, nameResolution: NameResolution, doc: TextDocument, position: Position): string  {
  const TCH: TCH.Exports = runtime.modules['jsfile://pyret-lang/src/arr/compiler/ts-codegen-helpers.js'].jsmod;
  const locations = Array.from(TCH.mapFromMutableStringDict(nameResolution.dict.env.dict.locations), ([k, v]): [string, Srcloc[]] => [k, TCH.listToArray(v)]);
	const tree = makeIntervalTree(doc, locations);
	const ranges = tree.search(position);
	return ranges
		.map(([key, loc]): [string, number] => [key, loc.dict['end-char'] - loc.dict['start-char']])
		.reduce((prev, curr) => prev[1] < curr[1] ? prev : curr, ["", Infinity])[0];
}

export function getBindOrigin(runtime: Runtime, nameResolution: NameResolution, key: string): BindOrigin {
  const TCH: TCH.Exports = runtime.modules['jsfile://pyret-lang/src/arr/compiler/ts-codegen-helpers.js'].jsmod;
	const env = nameResolution.dict.env;
  const bindings = TCH.mapFromMutableStringDict(env.dict.bindings);
  const modBindings = TCH.mapFromMutableStringDict(env.dict['module-bindings']);
  const typeBindings = TCH.mapFromMutableStringDict(env.dict['type-bindings']);
  const bind = bindings.get(key) ?? modBindings.get(key) ?? typeBindings.get(key);
  return bind?.dict.origin;
}