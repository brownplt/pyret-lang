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
  TextDocumentIdentifier,
  Range,
  FormattingOptions,
} from 'vscode-languageserver/node.js';

import {
  TextDocument
} from 'vscode-languageserver-textdocument';

import { cstWalk, lineTokenizerFrom, matchPath, mergeAll, SemTok, toDataArray, unpackModule } from './util.js';
import { indent, parse, State } from './pyret-mode.js';

export const tokenTypes = ['function', 'type', 'typeParameter', 'keyword', 'number', 'variable', 'data', 'variant', 'string', 'property', 'namespace', 'comment'];
export const tokenModifiers = ['readonly', 'invalid'];

// Create a connection for the server, using Node's IPC as a transport.
// Also include all preview / proposed LSP features.
const connection = createConnection(ProposedFeatures.all);

// Create a simple text document manager.
//const documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);

let capabilities: ClientCapabilities;

connection.onInitialize((params: InitializeParams) => {
  capabilities = params.capabilities;
  //console.log(capabilities);

  const result: InitializeResult = {
    capabilities: {
      textDocumentSync: TextDocumentSyncKind.Incremental,
      hoverProvider: true,
      documentFormattingProvider: true,
      documentRangeFormattingProvider: true,
      documentOnTypeFormattingProvider: {
        firstTriggerCharacter: '\n',
        moreTriggerCharacter: ['|', '}', ']', '+', '-', '*', '/', '=', '<', '>', 's', 'e', 'n', 'd', 'f', 't', 'y', ':', '.', '^', '`']
      },
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

class FormatCache {
  static CACHE_INTERVAL = 20;
  files: Map<string, State[]> = new Map();

  add(uri: string) {
    this.files.set(uri, [State.startState()]);
  }

  remove(uri: string) {
    this.files.delete(uri);
  }

  invalidateLines(uri: string, afterLine: number) {
    if (!this.files.has(uri)) { this.add(uri); }
    this.files.get(uri)?.splice(Math.floor(afterLine / FormatCache.CACHE_INTERVAL) + 1);
  }

  async forEachLine(uri: string, start: number, end: number, proc: (state: State, lineNum: number) => void) {
    const runtime = await (lsp.result) as any;
    const tokLib = unpackModule(runtime.modules["jsfile://pyret-lang/src/arr/compiler/pyret-tokenizer.js"]);
    const tokenizer = tokLib.CommentTokenizer;

    if (!this.files.has(uri)) { this.add(uri); }
    const states = this.files.get(uri) as State[];
    const startIdx = Math.min(states.length-1, Math.floor(start / FormatCache.CACHE_INTERVAL));
    const state = states[startIdx].copy();
    const startLine = startIdx * FormatCache.CACHE_INTERVAL;

    const doc = documents.get(uri)?.getText({ start: { line: startLine, character: 0 }, end: { line: end + 1, character: 0 } }) ?? "";
    tokenizer.tokenizeFrom(doc);
    let lineTokenizer = lineTokenizerFrom(tokenizer);

    for (let [tokLine, lineNum] of lineTokenizer) {
      const curLine = lineNum + startLine;
      // cache state if needed
      if (curLine % FormatCache.CACHE_INTERVAL === 0 && curLine / FormatCache.CACHE_INTERVAL >= states.length) {
        states.push(state.copy());
      }

      // process the line of tokens
      if (tokLine.length === 0) {
        state.lineState.nestingsAtLineStart = state.lineState.nestingsAtLineEnd.copy();
      } else {
        tokLine.forEach((tok, i, arr) => {
          parse(state, arr[i - 1], tok, arr[i + 1]);
        });
      }
      
      // if in range, call procedure
      if (curLine >= start && curLine <= end) {
        proc(state, curLine);
      }
    }
  }
}

const formatCache = new FormatCache();

const documents: Map<string, TextDocument> = new Map();

connection.onDidOpenTextDocument((params) => {
  const doc = TextDocument.create(
    params.textDocument.uri,
    params.textDocument.languageId,
    params.textDocument.version,
    params.textDocument.text,
  );
  documents.set(params.textDocument.uri, doc);
  formatCache.add(doc.uri);
});

connection.onDidCloseTextDocument((params) => {
  documents.delete(params.textDocument.uri);
  formatCache.remove(params.textDocument.uri);
});

connection.onDidChangeTextDocument((params) => {
  const doc = documents.get(params.textDocument.uri);
  if (!doc) return;
  TextDocument.update(doc, params.contentChanges, params.textDocument.version);
  let minLine = params.contentChanges.map((a) => 'range' in a ? a.range.start.line : 0).reduce((a, b) => Math.min(a, b));
  formatCache.invalidateLines(doc.uri, minLine);
});

async function formatRange(document: TextDocumentIdentifier, range: Range, options: FormattingOptions) {
  const doc = documents.get(document.uri)?.getText() ?? "";
  const lines = doc.split("\n");
  const edits: TextEdit[] = [];
  formatCache.forEachLine(document.uri, range.start.line, range.end.line, (state, lineNum) => {
    //compare the calculated indentation to the current one
    let calcIndentation = indent(lines[lineNum], state, options.insertSpaces, options.tabSize);
    let currIndentation = lines[lineNum].match(/^\s*/)?.[0];
    // push edit if needed
    if (currIndentation !== calcIndentation) {
      edits.push({
        range: {
          start: { line: lineNum, character: 0 },
          end: { line: lineNum, character: currIndentation?.length ?? 0 }
        },
        newText: calcIndentation,
      });
    }
  });
  
  //console.log(edits);
  return edits;
}

connection.onDocumentFormatting(async (params, token, workDoneProgress, resultProgress) => {
  const fullRange: Range = {
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
  let lineRange: Range = {
    start: { line: params.position.line, character: 0 },
    end: { line: params.position.line, character: params.position.character }
  };
  let line = documents.get(params.textDocument.uri)?.getText(lineRange) ?? "";
  if (params.ch === "\n" || line.match(electricChars)) {
    return (formatRange(params.textDocument, lineRange, params.options));
  }
  return;
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
    //console.log(tok);
    if (tok.name === "COMMENT" || tok.name === "UNTERMINATED-BLOCK-COMMENT") {
      toks.push({
        type: 'comment',
        modifiers: (tok.name === "UNTERMINATED-BLOCK-COMMENT" ? ['invalid'] : []),
        tok: tok
      });
    } else if (keywords.map((v: any) => v.name).includes(tok.name)) {
      toks.push({
        type: 'keyword',
        modifiers: [],
        tok: tok
      });
    } else if (tok.name === "NUMBER" || tok.name === "BAD-NUMBER") {
      toks.push({
        type: 'number',
        modifiers: (tok.name === "BAD-NUMBER" ? ['invalid'] : []),
        tok: tok
      });
    } else if (tok.name === "STRING" || tok.name === "UNTERMINATED-STRING") {
      toks.push({
        type: 'string',
        modifiers: (tok.name === "UNTERMINATED-STRING" ? ['invalid'] : []),
        tok: tok
      });
    } else if (tok.name === "NAME") {
      toks.push({
        type: 'variable',
        modifiers: ['readonly'],
        tok: tok
      });
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
  return { data: data };
});

// Make the text document manager listen on the connection
// for open, change and close text document events
//documents.listen(connection);
// Listen on the connection
connection.listen();