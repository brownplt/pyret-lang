"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.tokenModifiers = exports.tokenTypes = void 0;
const lsp = require("../external/lsp.js");
const node_js_1 = require("vscode-languageserver/node.js");
const vscode_languageserver_textdocument_1 = require("vscode-languageserver-textdocument");
const util_js_1 = require("./util.js");
const pyret_mode_js_1 = require("./pyret-mode.js");
exports.tokenTypes = ['function', 'type', 'typeParameter', 'keyword', 'number', 'variable', 'data', 'variant', 'string', 'property', 'namespace', 'comment'];
exports.tokenModifiers = ['readonly', 'invalid'];
// Create a connection for the server, using Node's IPC as a transport.
// Also include all preview / proposed LSP features.
const connection = node_js_1.createConnection(node_js_1.ProposedFeatures.all);
// Create a simple text document manager.
//const documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);
let capabilities;
connection.onInitialize((params) => {
    capabilities = params.capabilities;
    //console.log(capabilities);
    const result = {
        capabilities: {
            textDocumentSync: node_js_1.TextDocumentSyncKind.Incremental,
            hoverProvider: true,
            documentFormattingProvider: true,
            documentRangeFormattingProvider: true,
            documentOnTypeFormattingProvider: {
                firstTriggerCharacter: '\n',
                moreTriggerCharacter: ['|', '}', ']', '+', '-', '*', '/', '=', '<', '>', 's', 'e', 'n', 'd', 'f', 't', 'y', ':', '.', '^', '`']
            },
            semanticTokensProvider: {
                legend: {
                    tokenTypes: exports.tokenTypes,
                    tokenModifiers: exports.tokenModifiers
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
class FormatCache {
    constructor() {
        this.files = new Map();
    }
    add(uri) {
        this.files.set(uri, [pyret_mode_js_1.State.startState()]);
    }
    remove(uri) {
        this.files.delete(uri);
    }
    invalidateLines(uri, afterLine) {
        var _a;
        if (!this.files.has(uri)) {
            this.add(uri);
        }
        (_a = this.files.get(uri)) === null || _a === void 0 ? void 0 : _a.splice(Math.floor(afterLine / FormatCache.CACHE_INTERVAL) + 1);
    }
    async forEachLine(uri, start, end, proc) {
        var _a, _b;
        const runtime = await (lsp.result);
        const tokLib = util_js_1.unpackModule(runtime.modules["jsfile://pyret-lang/src/arr/compiler/pyret-tokenizer.js"]);
        const tokenizer = tokLib.CommentTokenizer;
        if (!this.files.has(uri)) {
            this.add(uri);
        }
        const states = this.files.get(uri);
        const startIdx = Math.min(states.length - 1, Math.floor(start / FormatCache.CACHE_INTERVAL));
        const state = states[startIdx].copy();
        const startLine = startIdx * FormatCache.CACHE_INTERVAL;
        const doc = (_b = (_a = documents.get(uri)) === null || _a === void 0 ? void 0 : _a.getText({ start: { line: startLine, character: 0 }, end: { line: end + 1, character: 0 } })) !== null && _b !== void 0 ? _b : "";
        tokenizer.tokenizeFrom(doc);
        let lineTokenizer = util_js_1.lineTokenizerFrom(tokenizer);
        for (let [tokLine, lineNum] of lineTokenizer) {
            const curLine = lineNum + startLine;
            // cache state if needed
            if (curLine % FormatCache.CACHE_INTERVAL === 0 && curLine / FormatCache.CACHE_INTERVAL >= states.length) {
                states.push(state.copy());
            }
            // process the line of tokens
            if (tokLine.length === 0) {
                state.lineState.nestingsAtLineStart = state.lineState.nestingsAtLineEnd.copy();
            }
            else {
                tokLine.forEach((tok, i, arr) => {
                    pyret_mode_js_1.parse(state, arr[i - 1], tok, arr[i + 1]);
                });
            }
            // if in range, call procedure
            if (curLine >= start && curLine <= end) {
                proc(state, curLine);
            }
        }
    }
}
FormatCache.CACHE_INTERVAL = 20;
const formatCache = new FormatCache();
const documents = new Map();
connection.onDidOpenTextDocument((params) => {
    const doc = vscode_languageserver_textdocument_1.TextDocument.create(params.textDocument.uri, params.textDocument.languageId, params.textDocument.version, params.textDocument.text);
    documents.set(params.textDocument.uri, doc);
    formatCache.add(doc.uri);
});
connection.onDidCloseTextDocument((params) => {
    documents.delete(params.textDocument.uri);
    formatCache.remove(params.textDocument.uri);
});
connection.onDidChangeTextDocument((params) => {
    const doc = documents.get(params.textDocument.uri);
    if (!doc)
        return;
    vscode_languageserver_textdocument_1.TextDocument.update(doc, params.contentChanges, params.textDocument.version);
    let minLine = params.contentChanges.map((a) => 'range' in a ? a.range.start.line : 0).reduce((a, b) => Math.min(a, b));
    formatCache.invalidateLines(doc.uri, minLine);
});
async function formatRange(document, range, options) {
    var _a, _b;
    const doc = (_b = (_a = documents.get(document.uri)) === null || _a === void 0 ? void 0 : _a.getText()) !== null && _b !== void 0 ? _b : "";
    const lines = doc.split("\n");
    const edits = [];
    formatCache.forEachLine(document.uri, range.start.line, range.end.line, (state, lineNum) => {
        var _a, _b;
        //compare the calculated indentation to the current one
        let calcIndentation = pyret_mode_js_1.indent(lines[lineNum], state, options.insertSpaces, options.tabSize);
        let currIndentation = (_a = lines[lineNum].match(/^\s*/)) === null || _a === void 0 ? void 0 : _a[0];
        // push edit if needed
        if (currIndentation !== calcIndentation) {
            edits.push({
                range: {
                    start: { line: lineNum, character: 0 },
                    end: { line: lineNum, character: (_b = currIndentation === null || currIndentation === void 0 ? void 0 : currIndentation.length) !== null && _b !== void 0 ? _b : 0 }
                },
                newText: calcIndentation,
            });
        }
    });
    //console.log(edits);
    return edits;
}
connection.onDocumentFormatting(async (params, token, workDoneProgress, resultProgress) => {
    const fullRange = {
        start: { line: 0, character: 0 },
        end: { line: Infinity, character: 0 }
    };
    return formatRange(params.textDocument, fullRange, params.options);
});
connection.onDocumentRangeFormatting(async (params, token, workDoneProgress, resultProgress) => {
    return formatRange(params.textDocument, params.range, params.options);
});
const electricChars = new RegExp("(?:[de.\\]}|:]|\|#|[-enst\\*\\+/=<>^~]\\s|is%|is-not%)$");
connection.onDocumentOnTypeFormatting(async (params) => {
    var _a, _b;
    let lineRange = {
        start: { line: params.position.line, character: 0 },
        end: { line: params.position.line, character: params.position.character }
    };
    let line = (_b = (_a = documents.get(params.textDocument.uri)) === null || _a === void 0 ? void 0 : _a.getText(lineRange)) !== null && _b !== void 0 ? _b : "";
    if (params.ch === "\n" || line.match(electricChars)) {
        return (formatRange(params.textDocument, lineRange, params.options));
    }
    return;
});
connection.languages.semanticTokens.on(async (params, _, __, ___) => {
    var _a, _b, _c, _d, _e, _f;
    const runtime = await (lsp.result);
    const tokLib = util_js_1.unpackModule(runtime.modules["jsfile://pyret-lang/src/arr/compiler/pyret-tokenizer.js"]);
    const parseLib = util_js_1.unpackModule(runtime.modules["jsfile://pyret-lang/src/arr/compiler/pyret-parser.js"]);
    const tokenizer = tokLib.Tokenizer;
    const commentTokenizer = tokLib.CommentTokenizer;
    const keywords = (_a = tokLib.keywords) !== null && _a !== void 0 ? _a : [];
    const parser = parseLib.PyretGrammar;
    const doc = (_c = (_b = documents.get(params.textDocument.uri)) === null || _b === void 0 ? void 0 : _b.getText()) !== null && _c !== void 0 ? _c : "";
    const toks = [];
    commentTokenizer.tokenizeFrom(doc);
    while (commentTokenizer.hasNext()) {
        let tok = commentTokenizer.next();
        //console.log(tok);
        if (tok.name === "COMMENT" || tok.name === "UNTERMINATED-BLOCK-COMMENT") {
            toks.push({
                type: 'comment',
                modifiers: (tok.name === "UNTERMINATED-BLOCK-COMMENT" ? ['invalid'] : []),
                tok: tok
            });
        }
        else if (keywords.map((v) => v.name).includes(tok.name)) {
            toks.push({
                type: 'keyword',
                modifiers: [],
                tok: tok
            });
        }
        else if (tok.name === "NUMBER" || tok.name === "BAD-NUMBER") {
            toks.push({
                type: 'number',
                modifiers: (tok.name === "BAD-NUMBER" ? ['invalid'] : []),
                tok: tok
            });
        }
        else if (tok.name === "STRING" || tok.name === "UNTERMINATED-STRING") {
            toks.push({
                type: 'string',
                modifiers: (tok.name === "UNTERMINATED-STRING" ? ['invalid'] : []),
                tok: tok
            });
        }
        else if (tok.name === "NAME") {
            toks.push({
                type: 'variable',
                modifiers: ['readonly'],
                tok: tok
            });
        }
    }
    const names = [];
    tokenizer.tokenizeFrom(doc);
    const parsed = parser.parse(tokenizer);
    const cst = parser.constructUniqueParse(parsed);
    //console.log(cst);
    if (cst !== undefined) {
        util_js_1.cstWalk(cst, (tok, path) => {
            //console.log(path.map((node) => node.name).join("/") + "/" + tok.key);
            if (tok.name === "NAME") {
                const stok = util_js_1.matchPath(tok, path, [
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
    const allToks = util_js_1.mergeAll([toks, names]);
    //console.log(allToks);
    const data = util_js_1.toDataArray(allToks, (_f = (_e = (_d = capabilities.textDocument) === null || _d === void 0 ? void 0 : _d.semanticTokens) === null || _e === void 0 ? void 0 : _e.multilineTokenSupport) !== null && _f !== void 0 ? _f : false);
    //console.log(data);
    return { data: data };
});
// Make the text document manager listen on the connection
// for open, change and close text document events
//documents.listen(connection);
// Listen on the connection
connection.listen();
