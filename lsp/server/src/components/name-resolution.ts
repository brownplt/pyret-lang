import type * as TCH from '../../../../src/arr/compiler/ts-codegen-helpers';
import type * as TCSH from '../../../../src/arr/compiler/ts-compile-structs-helpers';
import type * as A from '../../../../src/arr/compiler/ts-ast';
import { Runtime } from '../../../../src/arr/compiler/ts-impl-types';
import { NameResolution } from '../../../../src/arr/compiler/ts-compile-structs';
import { Srcloc } from '../../../../src/arr/compiler/ts-ast';

export function getNames(runtime: Runtime, nameResolution: NameResolution) {
	const TCH: TCH.Exports = runtime.modules['jsfile://pyret-lang/src/arr/compiler/ts-codegen-helpers.js'].jsmod;
	const TCSH: TCSH.Exports = runtime.modules['jsfile://pyret-lang/src/arr/compiler/ts-compile-structs-helpers.js'].jsmod;
  const names = new Map<string, A.Srcloc[]>();
  
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