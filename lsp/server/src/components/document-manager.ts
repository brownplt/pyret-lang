import { DidChangeTextDocumentParams } from 'vscode-languageserver-protocol';
import { TextDocument } from 'vscode-languageserver-textdocument';
import { Position, TextDocumentItem } from 'vscode-languageserver-types';
import { PhaseTuple } from '../../../../src/arr/compiler/ts-compiler-lib-impl';
import { Runtime } from '../../../../src/arr/compiler/ts-impl-types';
import { analyzeWorklist, makeOptions, makeWorklist, needsRecompile } from './compile-pipeline';
import { FormatCache } from './format-cache';
import { deleteStart, IntervalTree } from './util';
import type * as TCH from '../../../../src/arr/compiler/ts-codegen-helpers';
import { Srcloc } from '../../../../src/arr/compiler/ts-ast';
import { makeIntervalTree } from './name-resolution';

export class DocumentManager {
	document: TextDocument;
	formatCache: FormatCache;
	pipelineCache: PipelineCache;
	mtime: number;


	constructor(textDocument: TextDocumentItem) {
		this.document = TextDocument.create(
			textDocument.uri,
			textDocument.languageId,
			textDocument.version,
			textDocument.text,
		);
		this.formatCache = new FormatCache(this.document);
		this.pipelineCache = new PipelineCache();
		this.mtime = Date.now();
	}

	update(params: DidChangeTextDocumentParams) {
		TextDocument.update(this.document, params.contentChanges, params.textDocument.version);
		this.mtime = Date.now();
		const minLine =
			params.contentChanges
				.map((a) => 'range' in a ? a.range.start.line : 0)
				.reduce((a, b) => Math.min(a, b));
		this.formatCache.invalidateLinesAfter(minLine);
	}
}

class PipelineCache {
	trace: PhaseTuple;
	names: IntervalTree<Position, [string, Srcloc]>;
	mtime: number;

	constructor() {
		this.mtime = 0;
	}

	update(trace: PhaseTuple) {
		this.trace = trace;
		this.mtime = Date.now();
		this.names = undefined;
	}
}

export class Documents {
	docs: Map<string, DocumentManager>;

	constructor() {
		this.docs = new Map();
	}

	get(uri: string) {
		return this.docs.get(uri);
	}
	has(uri: string) {
		return this.docs.has(uri);
	}
	add(doc: TextDocumentItem) {
		return this.docs.set(doc.uri, new DocumentManager(doc));
	}
	remove(uri: string) {
		return this.docs.delete(uri);
	}

	async getTrace(runtime: Runtime, uri: string): Promise<PhaseTuple> {
		const options = makeOptions(runtime);
		const wlist = await makeWorklist(runtime, deleteStart(uri, 'file://'), this, options);
		if (needsRecompile(runtime, this.get(uri), wlist)) {
			const traces = await analyzeWorklist(runtime, wlist, options);
			for (const trace of traces) {
				if (trace[1].length !== 0) {
					const uri = trace[0].dict.provides.dict['from-uri'];
					this.get(uri)?.pipelineCache.update(trace[1]);
				}
			}
		}
		return this.get(uri).pipelineCache.trace;
	}

	async getIntervalTree(runtime: Runtime, uri: string) {
		const TCH: TCH.Exports = runtime.modules['jsfile://pyret-lang/src/arr/compiler/ts-codegen-helpers.js'].jsmod;
		if (this.has(uri)) {
			const doc = this.get(uri);
			// this has the side-effect of refreshing the pipeline cache if it is out of date
			const nameResolution = (await this.getTrace(runtime, uri))[4].dict.result;
			// so when we do this test, we know wether we need to make a new tree
			if (!doc.pipelineCache.names) {
				const locations = Array.from(TCH.mapFromMutableStringDict(nameResolution.dict.env.dict.locations), ([k, v]): [string, Srcloc[]] => [k, TCH.listToArray(v)]);
				doc.pipelineCache.names = makeIntervalTree(this.get(uri).document, locations);
			}
			return doc.pipelineCache.names;
		}
	}
}