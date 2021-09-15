"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const lsp = require("../external/lsp.js");
const node_js_1 = require("vscode-languageserver/node.js");
const vscode_languageserver_textdocument_1 = require("vscode-languageserver-textdocument");
const util_js_1 = require("./util.js");
// Create a connection for the server, using Node's IPC as a transport.
// Also include all preview / proposed LSP features.
const connection = node_js_1.createConnection(node_js_1.ProposedFeatures.all);
// Create a simple text document manager.
const documents = new node_js_1.TextDocuments(vscode_languageserver_textdocument_1.TextDocument);
const numbers = ["NUMBER"]; //also roughnum, etc. but I need to ask about that
let capabilities;
connection.onInitialize((params) => {
    capabilities = params.capabilities;
    console.log(capabilities);
    const result = {
        capabilities: {
            textDocumentSync: node_js_1.TextDocumentSyncKind.Incremental,
            hoverProvider: true,
            semanticTokensProvider: {
                legend: {
                    tokenTypes: util_js_1.tokenTypes,
                    tokenModifiers: util_js_1.tokenModifiers
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
    var _a, _b, _c, _d;
    const runtime = await (lsp.result);
    const tokLib = util_js_1.unpackModule(runtime.modules["jsfile://pyret-lang/src/arr/compiler/pyret-tokenizer.js"]);
    const parseLib = util_js_1.unpackModule(runtime.modules["jsfile://pyret-lang/src/arr/compiler/pyret-parser.js"]);
    const tokenizer = tokLib.Tokenizer;
    const commentTokenizer = tokLib.CommentTokenizer;
    const keywords = tokLib.keywords;
    const parser = parseLib.PyretGrammar;
    //console.log(tokenizer);
    const doc = (_a = documents.get(params.textDocument.uri)) === null || _a === void 0 ? void 0 : _a.getText();
    if (doc === undefined) {
        return { data: [] };
    }
    const toks = [];
    commentTokenizer.tokenizeFrom(doc);
    while (commentTokenizer.hasNext()) {
        let tok = commentTokenizer.next();
        if (tok.name === "COMMENT") {
            toks.push({
                type: "comment",
                modifiers: [],
                tok: tok
            });
        }
        else if (keywords.map((v) => v.name).includes(tok.name)) {
            toks.push({
                type: "keyword",
                modifiers: [],
                tok: tok
            });
        }
        else if (numbers.includes(tok.name)) {
            toks.push({
                type: "number",
                modifiers: [],
                tok: tok
            });
        }
        else if (tok.name === "STRING") {
            toks.push({
                type: "string",
                modifiers: [],
                tok: tok
            });
        }
        else if (tok.name === "NAME") {
            toks.push({
                type: "variable",
                modifiers: ["readonly"],
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
            console.log(path.map((node) => node.name).join("/") + "/" + tok.key);
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
    console.log(allToks);
    const data = util_js_1.toDataArray(allToks, (_d = (_c = (_b = capabilities.textDocument) === null || _b === void 0 ? void 0 : _b.semanticTokens) === null || _c === void 0 ? void 0 : _c.multilineTokenSupport) !== null && _d !== void 0 ? _d : false);
    //console.log(data);
    return { data: data };
});
// Make the text document manager listen on the connection
// for open, change and close text document events
documents.listen(connection);
// Listen on the connection
connection.listen();
//# sourceMappingURL=lsp-server.js.map