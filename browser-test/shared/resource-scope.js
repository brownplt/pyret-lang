/*
 * resource-scope.js -- keep a failing setup() from leaking the things it opened.
 *
 * Every env adapter has the same shape: start a server, launch Chromium, then do
 * fallible work (navigate, wait for the workbench, find the editor frame) before
 * it can hand back a `cleanup`. A throw anywhere in that middle section used to
 * strand the browser and the server: nothing had a handle on them yet, so
 * nothing could close them. Those open handles keep the `node --test` process
 * alive forever, so it never exits and the reporter never flushes its failure
 * summary -- a fast, well-diagnosed setup error becomes a silent 25-minute CI
 * timeout with no output. (That is precisely how the vscode env's boot-timeout
 * error, diagnostics and all, went missing.)
 *
 * So: register each resource the moment it exists, and close the whole scope on
 * the way out of a failure.
 *
 *   const scope = resourceScope();
 *   try {
 *     const server = await open(...);         scope.add(() => server.dispose());
 *     const browser = await launchChromium(); scope.add(() => browser.close());
 *     ... fallible work ...
 *     return { page, frame, cleanup: scope.closeAll };
 *   } catch (e) { await scope.closeAll(); throw e; }
 */
function resourceScope() {
  const closers = [];
  let closed = false;

  async function closeAll() {
    // Idempotent: the scope is both the failure path and the returned `cleanup`,
    // and after() can run after a hook already tore things down.
    if (closed) return;
    closed = true;
    // Reverse order, so the browser goes down before the server it was talking
    // to. One closer failing must not strand the rest -- and on the failure path
    // it must not mask the original error either, so a close failure is reported
    // and swallowed rather than thrown.
    for (const close of closers.reverse()) {
      try {
        await close();
      } catch (e) {
        console.error("cleanup step failed (continuing): " + (e && e.message));
      }
    }
  }

  return {
    add(close) { closers.push(close); },
    closeAll,
  };
}

module.exports = { resourceScope };
