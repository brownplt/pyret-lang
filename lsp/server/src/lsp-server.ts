import * as lsp from '../external/lsp.js';

import {
	createConnection,
	TextDocuments,
	ProposedFeatures,
	InitializeParams,
	TextDocumentSyncKind,
	InitializeResult,
	ClientCapabilities,
} from 'vscode-languageserver/node.js';

import {
	TextDocument
} from 'vscode-languageserver-textdocument';

import { cstWalk, matchPath, mergeAll, SemTok, toDataArray, tokenModifiers, tokenTypes, unpackModule } from './util.js';

// Create a connection for the server, using Node's IPC as a transport.
// Also include all preview / proposed LSP features.
const connection = createConnection(ProposedFeatures.all);

// Create a simple text document manager.
const documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);

const numbers = ["NUMBER"]; //also roughnum, etc. but I need to ask about that

let capabilities: ClientCapabilities;

connection.onInitialize((params: InitializeParams) => {
	capabilities = params.capabilities;
	console.log(capabilities);

	const result: InitializeResult = {
		capabilities: {
			textDocumentSync: TextDocumentSyncKind.Incremental,
			hoverProvider: true,
			semanticTokensProvider: {
				legend: {
					tokenTypes: tokenTypes,
					tokenModifiers: tokenModifiers
				},
				//some features disabled for now; will probably change later
				range: false,
				full: {
					delta: false
				}
			}
		}
	};
	return result;
});

connection.onInitialized(() => {
	// empty for now.
});

connection.onHover((params, _, __, ___) => {
	return { contents: "hello world!" };
});

connection.languages.semanticTokens.on(async (params, _, __, ___) => {
	const runtime = await (lsp.result) as any;
	const tokLib = unpackModule(runtime.modules["jsfile://pyret-lang/src/arr/compiler/pyret-tokenizer.js"]);
	const parseLib = unpackModule(runtime.modules["jsfile://pyret-lang/src/arr/compiler/pyret-parser.js"]);
	const tokenizer = tokLib.Tokenizer;
	const commentTokenizer = tokLib.CommentTokenizer;
	const keywords = tokLib.keywords;
	const parser = parseLib.PyretGrammar;
	//console.log(tokenizer);

	const doc = documents.get(params.textDocument.uri)?.getText();
	if (doc === undefined) {
		return { data: [] };
	}

	const toks: SemTok[] = [];
	commentTokenizer.tokenizeFrom(doc);
	while (commentTokenizer.hasNext()) {
		let tok = commentTokenizer.next();
		if (tok.name === "COMMENT") {
			toks.push({
				type: "comment",
				modifiers: [],
				tok: tok
			})
		} else if (keywords.map((v: any) => v.name).includes(tok.name)) {
			toks.push({
				type: "keyword",
				modifiers: [],
				tok: tok
			})
		} else if (numbers.includes(tok.name)) {
			toks.push({
				type: "number",
				modifiers: [],
				tok: tok
			})
		} else if (tok.name === "STRING") {
			toks.push({
				type: "string",
				modifiers: [],
				tok: tok
			})
		} else if (tok.name === "NAME") {
			toks.push({
				type: "variable",
				modifiers: ["readonly"],
				tok: tok
			})
		}
	}

	const names: SemTok[] = [];
	tokenizer.tokenizeFrom(doc);
	const parsed = parser.parse(tokenizer);
	const cst = parser.constructUniqueParse(parsed);
	//console.log(cst);
	if (cst !== undefined) {
		cstWalk(cst, (tok, path) => {
			console.log(path.map((node) => node.name).join("/") + "/" + tok.key);
			if (tok.name === "NAME") {
				const stok = matchPath(tok, path, [
					{
						paths: ["name-ann", "dot-ann"],
						type: "type",
						modifiers: []
					},
					{
						paths: ["name-binding", "id-expr", "assign-expr", "import-stmt", "provide-types-stmt/*/ann-field"],
						type: "variable",
						modifiers: ["readonly"]
					},
					{
						paths: ["fun-expr"],
						type: "function",
						modifiers: []
					},
					{
						paths: ["data-expr"],
						type: "data",
						modifiers: []
					},
					{
						paths: ["ty-params/comma-names"],
						type: "typeParameter",
						modifiers: []
					},
					{
						paths: ["cases-branch", "variant-constructor", "data-variant"],
						type: "variant",
						modifiers: []
					},
					{
						paths: ["dot-expr", "key", "ann-field"],
						type: "property",
						modifiers: []
					},
					{
						paths: ["import-name"],
						type: "namespace",
						modifiers: []
					}
				]);
				if (stok !== null) {
					names.push(stok);
				}
			}
		});
	}
	const allToks = mergeAll([toks, names]);
	console.log(allToks);
	const data = toDataArray(allToks, capabilities.textDocument?.semanticTokens?.multilineTokenSupport ?? false);
	//console.log(data);
	return { data: data }
});


// Make the text document manager listen on the connection
// for open, change and close text document events
documents.listen(connection);
// Listen on the connection
connection.listen();

