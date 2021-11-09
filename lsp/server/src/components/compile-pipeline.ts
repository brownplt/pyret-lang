import { List, Runtime } from '../../../../src/arr/compiler/ts-impl-types';
import { DocumentManager, Documents } from './document-manager';
import { listToArray, Module, pyretCall, unpackModule } from './util';
import * as path from 'path';
import { Loadable } from '../../../../src/arr/compiler/ts-compile-structs';
import type * as TCSH from '../../../../src/arr/compiler/ts-compile-structs-helpers';
import type * as TAU from '../../../../src/arr/compiler/ts-ast-util';
import type * as TCS from '../../../../src/arr/compiler/ts-compile-structs';
import type * as TCL from '../../../../src/arr/compiler/ts-compiler-lib-impl';
import type * as TCH from '../../../../src/arr/compiler/ts-codegen-helpers';
import type * as TCO from '../../../../src/arr/compiler/ts-compile-options';

type CLIContext = {
	dict: {
		'current-load-path' : string,
		'cache-base-dir': string,
		'compiled-read-only-dirs': List<string>,
		'options': any,
	}
}

type Dependency = {
	$name: 'dependency',
	dict: {
		protocol: string,
		arguments: List<string>
	}
} | {
	$name: 'builtin',
	dict: {
		modname: string
	}
}

// this may run into issues when the compiler caches locators
function makeModuleFinder(documents: Documents, runtime: Runtime) {
	return runtime.makeFunction((ctxt: CLIContext, dep: Dependency) => {
		const FL: any = unpackModule(runtime.modules['file://pyret-lang/src/arr/compiler/locators/file.arr']);
		const ML: any = unpackModule(runtime.modules['file://pyret-lang/src/arr/compiler/cli-module-loader.arr']);
		const CL: any = unpackModule(runtime.modules['file://pyret-lang/src/arr/compiler/compile-lib.arr']);
		const CS: any = unpackModule(runtime.modules['file://pyret-lang/src/arr/compiler/compile-structs.arr']);

		// we have to add the protocol here because Documents uses uri while locators use path
		const fileLocator = FL['mockable-file-locator'].app(runtime.makeObject({
			'input-file': null,
			'output-file': null,
			'file-exists': runtime.makeFunction((p: string) => documents.has('file://' + path.resolve(p))),
			'file-times': null,
      'mtimes': runtime.makeFunction((p: string) => runtime.makeObject({ mtime: documents.get('file://' + path.resolve(p))!.mtime })),
			'real-path': null,
			'read-file-path': runtime.makeFunction((p: string) => documents.get('file://' + path.resolve(p))!.document.getText())
		}));

		if (dep.$name === 'dependency' && (dep.dict.protocol === 'file' || dep.dict.protocol === 'file-no-cache')) {
			let clp = ctxt.dict['current-load-path'];
			let realPath = ML['get-real-path'].app(clp, dep);
			let newContext = runtime.makeObject({
				'current-load-path': path.dirname(realPath),
				'cache-base-dir': ctxt.dict['cache-base-dir'],
				'compiled-read-only-dirs': ctxt.dict['compiled-read-only-dirs'],
				'options': ctxt.dict.options,
			});
			if (documents.has('file://' + (dep.dict.arguments.$name === 'link' ? dep.dict.arguments.dict.first : ""))) {
				return CL.located.app(fileLocator.app(realPath, CS['standard-globals']), newContext);
			}
		}
		return ML['module-finder'].app(ctxt, dep);
	});
}

export async function makeWorklist(runtime: Runtime, filename: string, documents: Documents, options: TCL.CompileOptions): Promise<TCL.ToCompile[]> {
	const TCL = unpackModule(runtime.modules['jsfile://pyret-lang/src/arr/compiler/ts-compiler-lib-impl.js'] as Module<TCL.Exports>);
	const CS = unpackModule(runtime.modules['file://pyret-lang/src/arr/compiler/compile-structs.arr'] as Module<TCS.Exports>);


	const baseModule = CS.dependency.app("file-no-cache", runtime.ffi.makeList([filename]));
	const moduleFinder = makeModuleFinder(documents, runtime);
	const base = moduleFinder.app(
		runtime.makeObject({
			'current-load-path': "./",
			'cache-base-dir': options.dict['base-dir'],
			'compiled-read-only-dirs': options.dict['builtin-js-dirs'],
			options: options
		}),
		baseModule,
	);

	return listToArray(await pyretCall(runtime, () => TCL['compile-worklist'].app(moduleFinder, base.dict.locator, base.dict.context)));
}

export function makeOptions(runtime: Runtime) {
	const TCH: TCH.Exports = runtime.modules['jsfile://pyret-lang/src/arr/compiler/ts-codegen-helpers.js'].jsmod;
	const TCO = unpackModule(runtime.modules['jsfile://pyret-lang/src/arr/compiler/ts-compile-options.js'] as Module<TCO.Exports>);
	
	// TODO: maybe move to LSP level
	const baseDir = path.resolve(__dirname, '../../build/runtime');
	const optMap = TCH.stringDictFromMap(new Map<string, any>([
		['type-check', true],
		['base-dir', baseDir],
		['builtin-js-dir', baseDir],
	]));
	return TCO['populate-options'].app(optMap, './');
}

export async function needsRecompile(runtime: Runtime, document: DocumentManager, worklist: TCL.ToCompile[]): Promise<boolean> {
	const TCSH: TCSH.Exports = runtime.modules['jsfile://pyret-lang/src/arr/compiler/ts-compile-structs-helpers.js'].jsmod;
	
	for (const w of worklist) {
		const mtime = await pyretCall(runtime, () => TCSH.callMethod(w.dict.locator, 'get-modified-time'));
		if (mtime > document.pipelineCache.mtime) {
			return true;
		}
	}
	return false;
}

export async function analyzeFile(runtime: Runtime, filename: string, documents: Documents) {
	const options = makeOptions(runtime);
	return analyzeWorklist(runtime, await makeWorklist(runtime, filename, documents, options), options);
}

export async function analyzeWorklist(runtime: Runtime, wlist: TCL.ToCompile[], options: TCL.CompileOptions) {
	const CS = unpackModule(runtime.modules['file://pyret-lang/src/arr/compiler/compile-structs.arr'] as Module<TCS.Exports>);
	const TCL = unpackModule(runtime.modules['jsfile://pyret-lang/src/arr/compiler/ts-compiler-lib-impl.js'] as Module<TCL.Exports>);
	const TCSH: TCSH.Exports = runtime.modules['jsfile://pyret-lang/src/arr/compiler/ts-compile-structs-helpers.js'].jsmod;
	const AU = unpackModule(runtime.modules['file://pyret-lang/src/arr/compiler/ast-util.arr'] as Module<TAU.Exports>);
	const TCH: TCH.Exports = runtime.modules['jsfile://pyret-lang/src/arr/compiler/ts-codegen-helpers.js'].jsmod;

	let modules = new Map<string, Loadable>();
	const out: [TCH.Variant<Loadable, 'module-as-string'>, TCL.PhaseTuple | []][] = [];
	for (let w of wlist) {
		const loadables: Loadable[] = [];
		const uri = TCSH.callMethod(w.dict.locator, 'uri');
		if (modules.has(uri)) {
			loadables.push(modules.get(uri));
		} else {
			const provideMap = TCSH.callMethod(w.dict['dependency-map'], 'freeze');
			// modules isn't mutated, so we can make a new dict each time (if it does get mutated and we didn't notice it, we have a problem)
			const compiledModule = await pyretCall(runtime, () => TCL['compile-module'].app(TCL['analyze-arr-file'], w.dict.locator, provideMap, TCH.mutableStringDictFromMap(modules), options));
			const [loadable, trace] = compiledModule.vals;
			modules.set(uri, loadable);
			const { "compile-env": compileEnv, "post-compile-env": postCompileEnv, provides, "result-printer": result } = loadable.dict;
			const localLoadable = CS['module-as-string'].app(
				AU['localize-provides'].app(provides, compileEnv),
				compileEnv, postCompileEnv, result);
			out.push([localLoadable, listToArray(trace) as any]);
		}
	}
	return out;
}