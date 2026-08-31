import * as vscode from 'vscode';

/*
 * NOTE(joe Aug '26): this file is only used by CI for now. The issue it solves
 * is flaky CI runs where VScode wasn't consistently getting the .arr file open
 * and the extension loaded correctly, which would just hang.
 *
 * Claude said: The --env=vscode suite intermittently ends with the .arr tab open
 * and no webview ("no editor frame after 120000ms"), and the two explanations
 * are indistinguishable from outside the extension host: either the extension
 * never activated (so no provider was ever registered for pyret-parley.cpo), or
 * it activated and resolveCustomTextEditor never ran / never finished.
 *
 * These markers separate the two. They surface as status bar items rather than
 * notifications or console lines because the harness has to be able to read them
 * from the workbench DOM at the moment it gives up: the extension host runs
 * inside a web worker whose console Playwright cannot see, and toasts both
 * auto-dismiss and sit on top of the UI the other tests click.
 *
 * Off unless BOTH hold: the extension is running in development mode (loaded
 * via extensionDevelopmentPath -- the harness's regime; marketplace installs
 * are Production) AND the workspace sets pyret-parley.diagnostics. That
 * setting is deliberately not contributed in package.json, so it never
 * appears in the Settings UI; the API still reads unregistered keys out of a
 * workspace's settings.json, and only the browser-test fixture workspaces
 * write it.
 */

const PREFIX = 'PYRET-DIAG';

let enabled = false;

/** Call once at the top of activate(), before the first mark(). */
export function initDiagnostics(context: vscode.ExtensionContext): void {
  enabled =
    context.extensionMode === vscode.ExtensionMode.Development &&
    vscode.workspace.getConfiguration('pyret-parley').get<boolean>('diagnostics') === true;
}

/**
 * Record that `stage` was reached, with a wall-clock timestamp. Keeps a
 * reference to each item so it is never garbage collected out of the bar; the
 * harness reads them all at once, so ordering between them is the signal.
 */
const items: vscode.StatusBarItem[] = [];

export function mark(stage: string): void {
  if (!enabled) { return; }
  const text = `${PREFIX} ${stage} t=${Date.now()}`;
  // Also on the console: harmless, and visible if VS Code ever forwards
  // extension-host logs to the parent window.
  console.log(text);
  const item = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, -1000 - items.length);
  item.text = text;
  item.show();
  items.push(item);
}
