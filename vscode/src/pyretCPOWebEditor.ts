import * as vscode from 'vscode';
import * as path from 'path';
import { URI, Utils } from 'vscode-uri';
import { Buffer } from 'buffer';
import { mark } from './diagnostics';
// See cross-file dependencies with code.pyret.org/src/scripts/inline-selfcontained.js
const code = require('../build/web/views/editor.selfcontained.html');

const WEBVIEW_BASE_URL = '__PYRET_WEBVIEW_BASE_URL__';
const WEBVIEW_HASH = '__PYRET_WEBVIEW_HASH__';
const WEBVIEW_URL_FILE_MODE = '__PYRET_WEBVIEW_URL_FILE_MODE__';
const WEBVIEW_COMPILER = '__PYRET_WEBVIEW_COMPILER__';

// import * as fs from 'fs';
// import * as path from 'path';

export function getNonce() {
  let text = '';
  const possible = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
  for (let i = 0; i < 32; i++) {
    text += possible.charAt(Math.floor(Math.random() * possible.length));
  }
  return text;
}

// We support a small subset of the actual fs opts, which we grow as needed
type ReadFileOpts =
    'utf8'
  | { encoding?: 'utf8' };


export function makeCommandHandler(context: vscode.ExtensionContext) {
  const repls = new Map<string, PyretPane>();
  return async (...args : any[]) => {
    const activeEditor = vscode.window.activeTextEditor;
    if (activeEditor) {
      const uri = activeEditor.document.uri.toString();
      console.log("Active editor URI: ", uri);
      console.log("Repls: ", repls);
      if (repls.has(uri)) {
        console.log("A REPL for this document already exists.");
        const repl = repls.get(uri)!;
        repl.pane.reveal(vscode.ViewColumn.Two);
        repl.reset();
        return;
      }
      else {
        const document = activeEditor.document;
        const panel = vscode.window.createWebviewPanel(
          `pyretRun-${document.uri.toString()}`,
          `Run ${document.fileName}`,
          vscode.ViewColumn.Two,
          { enableScripts: true, retainContextWhenHidden: true }
        );
        const repl = makePyretPane(panel, context, document, 'repl');
        repls.set(uri, repl);
        repl.pane.onDidDispose(() => { repls.delete(uri); });
      }
    } else {
      console.log("No active text editor found.");
    }
    console.log("Command handler args: ", args);
  };
}


export class PyretCPOWebProvider implements vscode.CustomTextEditorProvider {

  public static register(context: vscode.ExtensionContext): vscode.Disposable {
    const provider = new PyretCPOWebProvider(context);
    const providerRegistration = vscode.window.registerCustomEditorProvider(PyretCPOWebProvider.viewType, provider, {
      webviewOptions: {
        retainContextWhenHidden: true,
      }
    });
    mark('provider-registered');
    return providerRegistration;
  }

  private static readonly viewType = 'pyret-parley.cpo';

  constructor(
    private readonly context: vscode.ExtensionContext
  ) { }

  /**
   * Called when our custom editor is opened.
   * 
   * 
   */
  public async resolveCustomTextEditor(
    document: vscode.TextDocument,
    webviewPanel: vscode.WebviewPanel,
    _token: vscode.CancellationToken
  ): Promise<void> {
    mark('resolve-enter');
    console.log("Pyret: resolving custom text editor at: ", document.uri);
    makePyretPane(webviewPanel, this.context, document, 'cpo');
    mark('resolve-done');
  }
}

function getTheme(vscodeTheme: vscode.ColorThemeKind): string {
  switch (vscodeTheme) {
    case vscode.ColorThemeKind.Light:
      return 'default';
    case vscode.ColorThemeKind.HighContrastLight:
      return 'high-contrast-light';
    case vscode.ColorThemeKind.Dark:
      return 'monokai';
    case vscode.ColorThemeKind.HighContrast:
      return 'high-contrast-dark';
    default:
      return 'default';
  }
}

/**
 * Get the static html used for the editor webviews.
 */
export function getHtmlForWebview(context: vscode.ExtensionContext, webview: vscode.Webview, showDefinitions = true): string {
  const config = vscode.workspace.getConfiguration('pyret-parley');
  const theme = getTheme(vscode.window.activeColorTheme.kind);
  const urlFileMode = config.get('urlFileMode');
  const baseURI = webview.asWebviewUri(vscode.Uri.joinPath(context.extensionUri, 'dist', 'web', 'build', 'web')).toString();
  const view = showDefinitions === false ? "hideDefinitions=true&headerStyle=hide" : "hideInteractions=true";
  const hashOptions = `#footerStyle=hide&${view}&theme=${theme}`;
  // The compiler backend is chosen by the pyret-parley.compiler setting --
  // the same knob as code.pyret.org's ?compiler= flag. The selfcontained
  // template bakes both flavors' asset paths (relative to the BASE_URL
  // sentinel); this fill is only the choice between them.
  const compiler = config.get('compiler') === 'ts' ? 'ts' : 'pyret';
  // Plain string replacement of the build's literal placeholders. split/join,
  // not String.replace, so a `$` in a filled value can't be read as a
  // replacement pattern.
  return (code as string)
    .split(WEBVIEW_BASE_URL).join(baseURI)
    .split(WEBVIEW_HASH).join(hashOptions)
    .split(WEBVIEW_URL_FILE_MODE).join(String(urlFileMode ?? ""))
    .split(WEBVIEW_COMPILER).join(compiler);
}


type PyretPaneType = 'repl' | 'cpo';

type PyretPane = {
  pane: vscode.WebviewPanel;
  context: vscode.ExtensionContext;
  document: vscode.TextDocument;
  type: PyretPaneType;
  reset: () => void;
}

export function makePyretPane(
  pane : vscode.WebviewPanel,
  context: vscode.ExtensionContext,
  document: vscode.TextDocument,
  type: PyretPaneType
): PyretPane {
    // Single source of truth for path resolution: every fs path that arrives
    // over the RPC is resolved against the open document's directory with
    // vscode-uri, so imports (`import url-file(...)`) and runtime file ops
    // (read-file, image-file, ...) resolve a given path identically. The pure
    // lexical helpers (join/dirname/...) use the bundled posix `path`, which
    // matches vscode-uri's posix semantics on every platform.
    const docDir = () => Utils.dirname(document.uri);
    const resolveAgainstDoc = (p: string) => Utils.resolvePath(docDir(), p);
    const knownModules = {
      'fs': {
        'writeFile': async (p: string, buffer : Buffer) => {
          await vscode.workspace.fs.writeFile(resolveAgainstDoc(p), buffer);
          return;
        },
        'readFile': async (p: string, opts : ReadFileOpts) => {
          const contents = await vscode.workspace.fs.readFile(resolveAgainstDoc(p));
          if(opts && (opts === 'utf8' || opts.encoding === 'utf8')) {
            return Buffer.from(contents).toString('utf8');
          }
          else {
            return contents;
          }
        },
        'stat': async (p: string) => {
          const stat = await vscode.workspace.fs.stat(resolveAgainstDoc(p));
          return {
            mtime: stat.mtime,
            ctime: stat.ctime,
            size: stat.size,
            native: stat
          };
        },
        'createDir': async (p: string) => {
          await vscode.workspace.fs.createDirectory(resolveAgainstDoc(p));
          return;
        }
      },
      'path': {
        'join': path.join,
        // Resolve against the document dir with the SAME mechanism as the fs
        // methods above (vscode-uri), returning the absolute path string.
        'resolve': (p : string) => resolveAgainstDoc(p).path,
        'basename': (p: string) => path.basename(p),
        'dirname': (p: string) => path.dirname(p),
        'extname': (p: string) => path.extname(p),
        'relative': (from: string, to: string) => path.relative(knownModules.path.resolve(from), knownModules.path.resolve(to)),
        // camelCase to match the method name filesystem-internal.js sends over
        // the RPC (sendRpc('path', 'isAbsolute', ...)).
        'isAbsolute': (p: string) => path.isAbsolute(p),
      },
      'process': {
        'cwd': () => process.cwd()
      }
    }

    // Setup initial content for the webview
    pane.webview.options = {
      enableScripts: true,
    };
    const showDefinitions = type === 'cpo';
    pane.webview.html = getHtmlForWebview(context, pane.webview, showDefinitions);


    /*
    State/event management for the bidirectional mapping between the
    TextDocument (VScode's abstraction) and the webview's CodeMirror instance.

    We maintain a queue of edits to apply to the TextDocument, and process them
    one after another – they are asynchronous so we need to avoid basic races of
    our own creation.

    We take a bit of a strong position on edits coming from the webview getting
    priority:

    - isProcessingEdits is set to true while we are applying edits. If any edits
      come in from VScode, we ignore them, counting ours as more important.
    - If we get out of sync, or our edits fail to apply, we override everything
      with a full replacement of the text that came from the webview with that
      edit.

    This means that if the user is editing in VScode at the same time as the
    webview is trying to make edits, the webview will win. Same for doing
    undo/redo events triggered through VScode's menu options that happen at the
    same time the user is typing in the CodeMirror view – the webview's version
    of things wins.
    */
    const editQueue : [vscode.WorkspaceEdit, string][] = [];
    let isProcessingEdits = false;

    const changeDocumentSubscription = vscode.workspace.onDidChangeTextDocument(e => {
      const hasChanges = e.contentChanges.length > 0;
      const isOurDocument = e.document.uri.toString() === document.uri.toString();
      if (hasChanges && isOurDocument && !isProcessingEdits) {
        updateWebview(e.contentChanges);
      }
    });

    // Make sure we get rid of the listener when our editor is closed.
    pane.onDidDispose(() => {
      changeDocumentSubscription.dispose();
    });

    function updateWebview(contentChanges?: readonly vscode.TextDocumentContentChangeEvent[]) {
      if(!contentChanges) {
        pane.webview.postMessage({
          protocol: "pyret",
          data: {
            type: 'setContents',
            text: document.getText(),
          }
        });
        return;
      }
      for(const change of contentChanges) {
        pane.webview.postMessage({
          protocol: "pyret",
          data: {
            type: 'change',
            change: {
              from: { line: change.range.start.line, ch: change.range.start.character },
              to: { line: change.range.end.line, ch: change.range.end.character },
              text: change.text.split('\n'),
              origin: null
            }
          }
        });
      }
    }

    function enqueueEdit(edit: vscode.WorkspaceEdit, source: string) {
      editQueue.push([edit, source]);
      processEditQueue();
    }
    async function processEditQueue() {
      function enqueueFullEdit(source : string) {
        const fullEdit = new vscode.WorkspaceEdit();
        fullEdit.replace(
          document.uri,
          new vscode.Range(0, 0, document.lineCount, 0),
          source);
        editQueue.push([fullEdit, source]);
      }
      if (editQueue.length === 0) { return; }
      else if (isProcessingEdits) { return; }
      else {
        const [edit, source] = editQueue.shift()!;
        try {
          isProcessingEdits = true;
          console.log("Processing edit from ", source);
          const ok = await vscode.workspace.applyEdit(edit);
          // If something went wrong with the edit, try again but just force the
          // whole document contents to match
          if(!ok) {
            console.error("applyEdit returned false, updating full contents", edit, source);
            enqueueFullEdit(source);
          }
        }
        catch (e) {
          console.error("Error applying edit: ", e);
          enqueueFullEdit(source);
        }
        finally {
          isProcessingEdits = false;
          processEditQueue();
        }
      }

    }

    type RPCResponse = { resultType: 'value', result: any, } | { resultType: 'exception', exception: any };
    function sendRpcResponse(data: { callbackId: string }, result: RPCResponse) {
      pane.webview.postMessage({
        protocol: 'pyret-rpc',
        data: {
          type: 'rpc-response',
          callbackId: data.callbackId,
          ...result
        }
      });
    }

    // Receive message from the webview.
    pane.webview.onDidReceiveMessage(async e => {
      console.log("Message: ", e);
      if (e.protocol === 'pyret-rpc') {
        /**
         * data: { module: string, method: string, args: string[], callbackId: string }
         * 
         * { type: 'rpc', module: 'fs', method: 'readFile', args: ['path/to/file'], callbackId: 'some-id' }
         */
        console.log("RPC:", e.data);
        const module = (knownModules as any)[e.data.module];
        if (!(module as any)[e.data.method]) {
          sendRpcResponse(e.data, { resultType: 'exception', exception: `Unknown method ${e.data.method}` });
        }
        else {
          try {
            const result = await (module as any)[e.data.method](...e.data.args);
            sendRpcResponse(e.data, { resultType: 'value', result });
          } catch (exn) {
            sendRpcResponse(e.data, { resultType: 'exception', exception: String(exn) });
          }
        }
        return;
      }
      if (e.protocol !== 'pyret') { console.warn("Non-pyret message: ", e); return; }
      let definitionsAtLastRun : boolean | string = false;
      if('repl' === type) {
        definitionsAtLastRun = document.getText();
      }
      let docText = document.getText();
      if(docText === "") {
        const config = vscode.workspace.getConfiguration('pyret-parley');
        let context = config.get('defaultContext');
        docText = `use context ${context}\n\n`;
        const edit = new vscode.WorkspaceEdit();
        edit.insert(document.uri, new vscode.Position(0, 0), docText);
        await vscode.workspace.applyEdit(edit);
        document.save();
      }
      const initialState = {
        definitionsAtLastRun,
        interactionsSinceLastRun: [],
        editorContents: docText,
        replContents: "",
        // Skip the editor's boot warm-start run (events.js reset()). Its only
        // observable effect is a live REPL prompt at boot, and the cpo pane
        // hides the interactions pane anyway -- while the run is in flight the
        // editor looks ready but swallows Run clicks and races typed edits
        // against the contents install. For the repl pane this field is inert:
        // definitionsAtLastRun is the file's text there, and that run always
        // happens.
        warmStart: false,
      };
      switch (e.data.type) {
        case 'pyret-init': {
          console.log("Got init", e);
          pane.webview.postMessage({
            protocol: 'pyret',
            data: {
              type: 'reset',
              state: JSON.stringify(initialState)
            },
          });
          pane.webview.postMessage({
            type: 'gainControl'
          });
          break;
        }
        case 'change': {
          console.log("Got change", e);
          const edit = new vscode.WorkspaceEdit();
          const { from, to, text } = e.data.change;
          const range = new vscode.Range(from.line, from.ch, to.line, to.ch);
          const newText = text.join('\n');
          edit.replace(document.uri, range, newText);
          enqueueEdit(edit, e.state.editorContents);
          break;
        }
        default: console.log("Got a message: ", e);
      }
    });

    updateWebview();

    return {
      pane,
      context,
      document,
      type,
      reset: () => { pane.webview.postMessage({
          protocol: 'pyret',
          data: {
            type: 'reset',
            state: JSON.stringify({
              definitionsAtLastRun: document.getText(),
              interactionsSinceLastRun: [],
              editorContents: document.getText(),
              replContents: "",
            })
          },
        });
      }
    };
}
