import { tokenModifiers, tokenTypes } from './lsp-server';

interface SrcLoc {
	startCol: number;
	startRow: number;
	startChar: number;
	endRow: number;
	endCol: number;
	endChar: number;
}
interface Token {
	name: string;
	value: string;
	key: string;
	asString: string;
	pos: SrcLoc;
}
interface Nonterm {
	kids: ParserCST[];
	name: string;
	pos: SrcLoc;
	toString: () => string;
}

type ParserCST = Token | Nonterm;

export { SrcLoc, Token, Nonterm, ParserCST }

export function unpackModule(module: any): any {
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

interface PathMatch {
	paths: string[],
	type: string,
	modifiers: string[]
}
export interface SemTok {
	type: string,
	modifiers: string[],
	tok: Token
}

export function matchPath(tok: Token, path: Nonterm[], spec: PathMatch[]): SemTok | null {
	let npath = path.map((v) => v.name);
	for (const rule of spec) {
		for (const optionString of rule.paths) {
			let option = optionString.split("/");
			let match = true;
			let i = npath.length - 1;
			while (option.length > 0 && i >= 0) {
				let next = option.pop();
				if (next === "*") {
					next = option.pop();
					i = npath.lastIndexOf(next ?? "/");
					if (i < 0) {
						match = false;
						break;
					}
				}
				else if (npath[i] !== next) {
					match = false;
					break;
				}
				i--;
			}
			if (match && option.length == 0) {
				return {
					type: rule.type,
					modifiers: rule.modifiers,
					tok: tok
				};
			}
		}
	}
	return null;
}

export function merge(list1: SemTok[], list2: SemTok[]): SemTok[] {
	let i1 = 0;
	let i2 = 0;
	let out: SemTok[] = [];
	while (i1 < list1.length && i2 < list2.length) {
		let cmp = list1[i1].tok.pos.startChar - list2[i2].tok.pos.startChar;
		if (cmp < 0) {
			out.push(list1[i1]);
			i1++;
		} else {
			out.push(list2[i2]);
			i2++;
			if (cmp == 0) {
				i1++; //don't duplicate tokens, prefer the second list
			}
		}
	}
	if (i1 < list1.length) {
		out = out.concat(list1.slice(i1));
	}
	else if (i2 < list2.length) {
		out = out.concat(list2.slice(i2));
	}
	return out;
}

export function mergeAll(lists: SemTok[][]): SemTok[] {
	return lists.reduce((prev, curr) => merge(prev, curr));
}

export function toDataArray(toks: SemTok[], allowMultiline: boolean): number[] {
	let lastline = 1; // LSP starts from 0, parser starts from 1
	let lastcol = 0;
	return toks.flatMap((stok) => {
		let deltaline: number;
		let deltacol: number;
		const pos = stok.tok.pos;
		if (pos.startRow == lastline) {
			deltaline = 0;
			deltacol = pos.startCol - lastcol;
		} else {
			deltaline = pos.startRow - lastline;
			deltacol = pos.startCol;
		}

		let type = tokenTypes.indexOf(stok.type);
		let modifiers = 0;
		for (let i = 0; i < tokenModifiers.length; i++) {
			if (stok.modifiers.includes(tokenModifiers[i])) {
				modifiers |= (1 << i);
			}
		}

		if (!allowMultiline && pos.startRow !== pos.endRow) {
			lastline = pos.endRow;
			lastcol = 0;
			return stok.tok.value.split("\n").flatMap((line, i) => {
				if (i == 0) {
					return [deltaline, deltacol, line.length, type, modifiers];
				} else {
					return [1, 0, line.length, type, modifiers];
				}
			});
		} else {
			let length = pos.endCol - pos.startCol;
			// some keyword tokens (e.g. CHECKCOLON) include the colon. 
			// We don't want to highlight it in some places and not in others, so remove it if it is present
			if (stok.type === 'keyword' && stok.tok.value.endsWith(':')) {
				length--;
			}
			lastline = pos.startRow;
			lastcol = pos.startCol;
			return [
				deltaline,
				deltacol,
				length,
				type,
				modifiers
			];
		}
	})
}

export function splitMultilineToken(tok: Token): Token[] {
	if (tok.pos.startRow !== tok.pos.endRow) {
		return tok.value.split("\n").map((val, i, arr) => {
			let name = tok.name + (i == 0 ? "-START" : i == arr.length-1 ? "-END" : "");
			let asString = "'" + name;
			let startChar = tok.value.indexOf(val) + tok.pos.startChar;
			let startRow = tok.pos.startRow + i;
			let pos: SrcLoc = {
				startChar: startChar,
				startCol: i == 0 ? tok.pos.startCol : 0,
				startRow: startRow,
				endChar: startChar + val.length,
				endCol: i == 0 ? tok.pos.endCol : val.length,
				endRow: startRow
			}
			return {
				name: name,
				value: val,
				key: asString + ":" + val,
				asString: asString,
				pos: pos
			}
		});
	} else {
		return [tok];
	}
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