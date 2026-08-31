# VS Code extension end-to-end tests

These tests exercise the Pyret VS Code mode the way a user does — open an `.arr`
tab, hit Run — and check **what actually happened on the filesystem**. The VS
Code filesystem is the interesting part: in the webview the editor is the same
code.pyret.org editor, but file operations are routed over a `postMessage` RPC
to the extension host, which talks to `vscode.workspace.fs`. Path resolution
there is subtly different from a normal disk, and that is what these tests pin
down.

## How it works

We run Mocha **inside the (web) extension host** via
`@vscode/test-web --extensionTestsPath`. That host has the full `vscode` API but
cannot see the webview DOM — which is fine, because we don't need it. Instead:

1. The test opens an `.arr` file as a text editor and invokes
   `pyret-parley.run-file` (the editor-tab Run button). That spins up the REPL
   webview, which auto-runs the program.
2. The program writes a file (via Pyret's `filesystem` module →
   `filesystem-internal` → the extension's `pyret-rpc` `fs.writeFile` →
   `vscode.workspace.fs`).
3. The test polls `vscode.workspace.fs` for that file and asserts on its
   contents.

The workspace filesystem is `@vscode/test-web`'s in-memory provider, seeded
read-only from the mounted fixtures folder and writable as an overlay — so
program writes are visible to the test within the run, with no disk side effects.

No browser DOM, no network: `url-file` mode is `all-local` (see
`fixtures/spell-checker/.vscode/settings.json`).

## Running

```sh
# from vscode/
npm test            # pretest compiles the extension + the test bundle, then runs
npm run test:ts     # the same three tests, on the TypeScript compiler backend
                    # (fixtures/spell-checker-ts sets pyret-parley.compiler: "ts";
                    # requires a code.pyret.org build that ran `make web-ts`)
```

Prerequisites (one-time):

- `code.pyret.org` web assets built and symlinked in as `build/`
  (`ln -s ../code.pyret.org/build build`); the webview loads
  `build/web/js/cpo-main.jarr.js` and `build/web/views/editor.html` from there.
- A Chromium for `@vscode/test-web`: `npx playwright install chromium`.

## What the fixtures model

A minimized version of the Bootstrap "Spell Checker" starter files, which import
a helper library that in turn imports `core.arr` via `url-file`:

```
fixtures/spell-checker/
  ai/         plain.arr, starter-ok.arr, starter-bug.arr   <- the open "tabs"
  libraries/  core.arr, lib-ok.arr, lib-bug.arr            <- the helper library
```

## The finding these tests encode

`url-file(BASE, REL)` in `all-local` mode uses `REL` as the local path, and that
path is resolved **against the directory of the open editor tab**
(`dirname(document.uri)`), not against the directory of the module doing the
import. So a library in `libraries/` that imports its sibling with
`url-file(base, "core.arr")` actually looks for `core.arr` next to the *starter
file's tab* (`ai/`), and fails. The working version has to climb back out with
`url-file(base, "../libraries/core.arr")` — the "`../` nonsense".

The three tests:

| Test | Fixture | Expectation |
| --- | --- | --- |
| working-dir probe | `ai/plain.arr` | **passes** — relative output lands next to the tab (`ai/`), not at the workspace root |
| starter chain (workaround) | `ai/starter-ok.arr` → `lib-ok.arr` (`../libraries/core.arr`) | **passes** — value flows starter → Lib → Core and is written out |
| module-relative sibling | `ai/starter-bug.arr` → `lib-bug.arr` (`core.arr`) | **passes** — the context-aware finder resolves a library's sibling relative to the importing module |

The last test was intentionally red when first added; it went green when the
web module finder became context-aware (a per-module load-path threaded through
`compile-lib`, with path arithmetic in the embedding host — the same fix
described above). Both compiler backends implement these semantics, which is
what `npm run test:ts` pins down.
