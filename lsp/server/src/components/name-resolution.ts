import { Srcloc } from '../../../../src/arr/compiler/ts-ast';
import { IntervalTree, rangeFromSrcloc } from './util';
import { Position } from 'vscode-languageserver-types';
import { TextDocument } from 'vscode-languageserver-textdocument';

export function makeIntervalTree(doc: TextDocument, names: [string, Srcloc[]][]) {
	const tree = new IntervalTree<Position, [string, Srcloc]>((pos) => doc.offsetAt(pos));

	for (const [name, locs] of names) {
		for (const loc of locs) {
			if (loc.$name !== 'srcloc') continue;
			const range = rangeFromSrcloc(loc);
			tree.insert(range.start, range.end, [name, loc]);
		}
	}
	return tree;
}
