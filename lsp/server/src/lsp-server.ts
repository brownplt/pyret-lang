import * as lsp from '../external/lsp';

import {
  createConnection,
  ProposedFeatures,
  InitializeParams,
  TextDocumentSyncKind,
  InitializeResult,
  ClientCapabilities,
  TextEdit,
  TextDocumentIdentifier,
  Range,
  FormattingOptions,
} from 'vscode-languageserver/node';

import { cstWalk, deleteStart, locationFromSrcloc, slocContains, slocLte, unpackModule } from './components/util';
import { indent } from './components/pyret-mode';
import { matchPath, mergeAll, SemTok, toDataArray } from './components/semantic-tokens';
import { DocumentManager } from './components/document-manager';
import { analyzeFile } from './components/compile-pipeline';

import type * as TCH from '../../../src/arr/compiler/ts-codegen-helpers';
import type * as A from '../../../src/arr/compiler/ts-ast';
import { getNames } from './components/name-resolution';

export const tokenTypes = ['function', 'type', 'typeParameter', 'keyword', 'number', 'variable', 'data', 'variant', 'string', 'property', 'namespace', 'comment'];
export const tokenModifiers = ['readonly', 'invalid'];

// Create a connection for the server, using Node's IPC as a transport.
// Also include all preview / proposed LSP features.
const connection = createConnection(ProposedFeatures.all);

const documents = new Map<string, DocumentManager>();

let capabilities: ClientCapabilities;

connection.onInitialize((params: InitializeParams) => {
  capabilities = params.capabilities;
  //console.log(capabilities);

  const result: InitializeResult = {
    capabilities: {
      textDocumentSync: TextDocumentSyncKind.Incremental,
      documentFormattingProvider: true,
      documentRangeFormattingProvider: true,
      documentOnTypeFormattingProvider: {
        firstTriggerCharacter: '\n',
        moreTriggerCharacter: ['|', '}', ']', '+', '-', '*', '/', '=', '<', '>', 's', 'e', 'n', 'd', 'f', 't', 'y', ':', '.', '^', '`']
      },
      definitionProvider: true,
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

connection.onDidOpenTextDocument((params) => {
  const doc = new DocumentManager(params.textDocument);
  documents.set(params.textDocument.uri, doc);
});

connection.onDidCloseTextDocument((params) => {
  documents.delete(params.textDocument.uri);
});

connection.onDidChangeTextDocument((params) => {
  const doc = documents.get(params.textDocument.uri);
  if (!doc) return;
  doc.update(params);
  const minLine =
    params.contentChanges
    .map((a) => 'range' in a ? a.range.start.line : 0)
    .reduce((a, b) => Math.min(a, b));
  doc.formatCache.invalidateLinesAfter(minLine);
});

async function formatRange(document: TextDocumentIdentifier, range: Range, options: FormattingOptions) {
  const doc = documents.get(document.uri);
  if (!doc) return;
  const lines = doc.document.getText().split("\n");
  const edits: TextEdit[] = [];
  doc.formatCache.forEachLine(range.start.line, range.end.line, (state, lineNum) => {
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

connection.onDocumentFormatting(async (params, _, __, ___) => {
  const fullRange: Range = {
    start: { line: 0, character: 0 },
    end: { line: Infinity, character: 0 }
  };
  return formatRange(params.textDocument, fullRange, params.options);
});

connection.onDocumentRangeFormatting(async (params, _, __, ___) => {
  return formatRange(params.textDocument, params.range, params.options);
});

const electricChars = new RegExp("(?:[de.\\]}|:]|\|#|[-enst\\*\\+/=<>^~]\\s|is%|is-not%)$");

connection.onDocumentOnTypeFormatting(async (params) => {
  let lineRange: Range = {
    start: { line: params.position.line, character: 0 },
    end: { line: params.position.line, character: params.position.character }
  };
  let line = documents.get(params.textDocument.uri)?.document.getText(lineRange) ?? "";
  if (params.ch === "\n" || line.match(electricChars)) {
    return (formatRange(params.textDocument, lineRange, params.options));
  }
  return;
});

// go to definition request
connection.onDefinition(async (params, _, __, ___) => {
  const runtime = await lsp.result;
	const TCH: TCH.Exports = runtime.modules['jsfile://pyret-lang/src/arr/compiler/ts-codegen-helpers.js'].jsmod;

  const filename = deleteStart(params.textDocument.uri, 'file://');
  const wlist = await analyzeFile(filename, documents, runtime);
  const trace = wlist.pop()[1];
  if (trace.length === 0) return [];
  const nameResolution = trace[4].dict.result;
  const names = getNames(runtime, nameResolution);
  
  let match: TCH.Variant<A.Srcloc, 'srcloc'>;
  let ans: string;
  for (let [name, uses] of names.entries()) {
    for (let loc of uses) {
      if (loc.$name === 'srcloc' && slocContains(loc, params.position) && slocLte(loc, match)) {
        match = loc;
        ans = name;
      }
    }
  }

  const env = nameResolution.dict.env.dict;
  const bindings = TCH.mapFromMutableStringDict(env.bindings);
  const modBindings = TCH.mapFromMutableStringDict(env['module-bindings']);
  const srcloc = bindings.get(ans) ?? modBindings.get(ans);
  if (!srcloc) return [];
  return [
    srcloc.dict.origin.dict['local-bind-site'],
    srcloc.dict.origin.dict['definition-bind-site']
  ]
    .filter((v): v is TCH.Variant<A.Srcloc, 'srcloc'> => v.$name === 'srcloc') 
    .map((v) => locationFromSrcloc(v));
});

connection.languages.semanticTokens.on(async (params, _, __, ___) => {
  const runtime = await lsp.result;
  const PT: any = unpackModule(runtime.modules["jsfile://pyret-lang/src/arr/compiler/pyret-tokenizer.js"]);
  const PP: any = unpackModule(runtime.modules["jsfile://pyret-lang/src/arr/compiler/pyret-parser.js"]);
  const tokenizer = PT.Tokenizer;
  const commentTokenizer = PT.CommentTokenizer;
  const keywords = PT.keywords ?? [];
  const parser = PP.PyretGrammar;
  const doc = documents.get(params.textDocument.uri)?.document.getText() ?? "";

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

// Listen on the connection
connection.listen();