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
const documents = new node_js_1.TextDocuments(vscode_languageserver_textdocument_1.TextDocument);
let capabilities;
connection.onInitialize((params) => {
    capabilities = params.capabilities;
    console.log(capabilities);
    const result = {
        capabilities: {
            textDocumentSync: node_js_1.TextDocumentSyncKind.Incremental,
            hoverProvider: true,
            documentFormattingProvider: true,
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
connection.onDocumentFormatting(async (params, token, workDoneProgress, resultProgress) => {
    var _a, _b, _c, _d;
    const runtime = await (lsp.result);
    const tokLib = util_js_1.unpackModule(runtime.modules["jsfile://pyret-lang/src/arr/compiler/pyret-tokenizer.js"]);
    const tokenizer = tokLib.CommentTokenizer;
    let tabSize = params.options.tabSize;
    let useSpaces = params.options.insertSpaces;
    const doc = (_b = (_a = documents.get(params.textDocument.uri)) === null || _a === void 0 ? void 0 : _a.getText()) !== null && _b !== void 0 ? _b : "";
    tokenizer.tokenizeFrom(doc);
    const lines = doc.split("\n");
    // create an array of Token arrays, each representing a single line
    const tokLines = lines.map(() => []);
    let tok;
    while ((tok = tokenizer.next()).name !== "EOF") {
        for (tok of util_js_1.splitMultilineToken(tok)) {
            tokLines[tok.pos.startRow - 1].push(tok);
        }
    }
    //console.log(tokLines);
    let state = pyret_mode_js_1.State.startState();
    const edits = [];
    for (let i = 0; i < lines.length; i++) {
        //process the line of tokens
        tokLines[i].forEach((tok, i, arr) => {
            pyret_mode_js_1.parse(state, arr[i - 1], tok, arr[i + 1]);
        });
        //feed the resulting state to indent
        let indentAmount = pyret_mode_js_1.indent(lines[i], state, useSpaces ? tabSize : 1);
        //compare the calculated indentation to the current one, add an edit if necessary
        let calcIndentation = (useSpaces ? " " : "\t").repeat(indentAmount);
        let currIndentation = (_c = lines[i].match(/^\s*/)) === null || _c === void 0 ? void 0 : _c[0];
        if (currIndentation !== calcIndentation) {
            edits.push({
                range: {
                    start: { line: i, character: 0 },
                    end: { line: i, character: (_d = currIndentation === null || currIndentation === void 0 ? void 0 : currIndentation.length) !== null && _d !== void 0 ? _d : 0 }
                },
                newText: calcIndentation,
            });
        }
    }
    //console.log(edits);
    return edits;
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
        if (tok.name === "COMMENT" || tok.name === "UNTERMINATED-BLOCK-COMMENT") {
            toks.push({
                type: 'comment',
                modifiers: (tok.name === "UNTERMINATED-BLOCK-COMMENT" ? ['illegal'] : []),
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
            console.log(tok);
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
documents.listen(connection);
// Listen on the connection
connection.listen();
