import * as lsp from '../external/lsp.js';

import {
	createConnection,
	TextDocuments,
	ProposedFeatures,
	InitializeParams,
	TextDocumentSyncKind,
	InitializeResult,
	ClientCapabilities,
	TextEdit,
} from 'vscode-languageserver/node.js';

import {
	TextDocument
} from 'vscode-languageserver-textdocument';

import { cstWalk, matchPath, mergeAll, peek, SemTok, splitMultilineToken, SrcLoc, toDataArray, Token, unpackModule } from './util.js';
import { indent, parse, State } from './pyret-mode.js';

export const tokenTypes = ['function', 'type', 'typeParameter', 'keyword', 'number', 'variable', 'data', 'variant', 'string', 'property', 'namespace', 'comment'];
export const tokenModifiers = ['readonly', 'invalid'];

// Create a connection for the server, using Node's IPC as a transport.
// Also include all preview / proposed LSP features.
const connection = createConnection(ProposedFeatures.all);

// Create a simple text document manager.
const documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);

let capabilities: ClientCapabilities;

connection.onInitialize((params: InitializeParams) => {
	capabilities = params.capabilities;
	console.log(capabilities);

	const result: InitializeResult = {
		capabilities: {
			textDocumentSync: TextDocumentSyncKind.Incremental,
			hoverProvider: true,
			documentFormattingProvider: true,
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

connection.onDocumentFormatting(async (params, token, workDoneProgress, resultProgress) => {
	const runtime = await (lsp.result) as any;
	const tokLib = unpackModule(runtime.modules["jsfile://pyret-lang/src/arr/compiler/pyret-tokenizer.js"]);
	const tokenizer = tokLib.CommentTokenizer;

	let tabSize = params.options.tabSize;
	let useSpaces = params.options.insertSpaces;

	const doc = documents.get(params.textDocument.uri)?.getText() ?? "";
	tokenizer.tokenizeFrom(doc);
	const lines = doc.split("\n");

	// create an array of Token arrays, each representing a single line
	const tokLines: Token[][] = lines.map(() => []);
	let tok: Token;
	while ((tok = tokenizer.next()).name !== "EOF") {
		for (tok of splitMultilineToken(tok)) {
			tokLines[tok.pos.startRow-1].push(tok);
		}
	}
	//console.log(tokLines);

	let state = State.startState();
	const edits: TextEdit[] = [];
	for (let i = 0; i < lines.length; i++) {
		//process the line of tokens
		tokLines[i].forEach((tok, i, arr) => {
			parse(state, arr[i-1], tok, arr[i+1]);
		});
		//feed the resulting state to indent
		let indentAmount = indent(lines[i], state, useSpaces ? tabSize : 1);
		//compare the calculated indentation to the current one, add an edit if necessary
		let calcIndentation = (useSpaces ? " " : "\t").repeat(indentAmount);
		let currIndentation = lines[i].match(/^\s*/)?.[0];
		if (currIndentation !== calcIndentation) {
			edits.push({
				range: {
					start: {line: i, character: 0},
					end: {line: i, character: currIndentation?.length ?? 0}
				},
				newText: calcIndentation,
			});
		}
	}
	//console.log(edits);
	return edits;
});

connection.languages.semanticTokens.on(async (params, _, __, ___) => {
	const runtime = await (lsp.result) as any;
	const tokLib = unpackModule(runtime.modules["jsfile://pyret-lang/src/arr/compiler/pyret-tokenizer.js"]);
	const parseLib = unpackModule(runtime.modules["jsfile://pyret-lang/src/arr/compiler/pyret-parser.js"]);
	const tokenizer = tokLib.Tokenizer;
	const commentTokenizer = tokLib.CommentTokenizer;
	const keywords = tokLib.keywords ?? [];
	const parser = parseLib.PyretGrammar;
	const doc = documents.get(params.textDocument.uri)?.getText() ?? "";

	const toks: SemTok[] = [];
	commentTokenizer.tokenizeFrom(doc);
	while (commentTokenizer.hasNext()) {
		let tok = commentTokenizer.next();
		if (tok.name === "COMMENT" || tok.name === "UNTERMINATED-BLOCK-COMMENT") {
			toks.push({
				type: 'comment',
				modifiers: (tok.name === "UNTERMINATED-BLOCK-COMMENT" ? ['illegal'] : []),
				tok: tok
			})
		} else if (keywords.map((v: any) => v.name).includes(tok.name)) {
			toks.push({
				type: 'keyword',
				modifiers: [],
				tok: tok
			})
		} else if (tok.name === "NUMBER" || tok.name === "BAD-NUMBER") {
			toks.push({
				type: 'number',
				modifiers: (tok.name === "BAD-NUMBER" ? ['invalid'] : []),
				tok: tok
			})
		} else if (tok.name === "STRING" || tok.name === "UNTERMINATED-STRING") {
			console.log(tok);
			toks.push({
				type: 'string',
				modifiers: (tok.name === "UNTERMINATED-STRING" ? ['invalid'] : []),
				tok: tok
			})
		} else if (tok.name === "NAME") {
			toks.push({
				type: 'variable',
				modifiers: ['readonly'],
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
			//console.log(path.map((node) => node.name).join("/") + "/" + tok.key);
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
	//console.log(allToks);
	const data = toDataArray(allToks, capabilities.textDocument?.semanticTokens?.multilineTokenSupport ?? false);
	//console.log(data);
	return { data: data }
});


// Make the text document manager listen on the connection
// for open, change and close text document events
documents.listen(connection);
// Listen on the connection
connection.listen();

