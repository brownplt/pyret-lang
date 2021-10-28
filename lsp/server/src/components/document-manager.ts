import { DidChangeTextDocumentParams } from 'vscode-languageserver-protocol';
import { TextDocument } from 'vscode-languageserver-textdocument';
import { TextDocumentItem } from 'vscode-languageserver-types';
import { FormatCache } from './format-cache';

export class DocumentManager {
	document: TextDocument;
	formatCache: FormatCache;
	mtime: number;


	constructor(textDocument: TextDocumentItem) {
		this.document = TextDocument.create(
			textDocument.uri,
			textDocument.languageId,
			textDocument.version,
			textDocument.text,
		);
		this.formatCache = new FormatCache(this.document);
		this.mtime = Date.now();
	}

	update(params: DidChangeTextDocumentParams) {
		TextDocument.update(this.document, params.contentChanges, params.textDocument.version);
		this.mtime = Date.now();
	}
}