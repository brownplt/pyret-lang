# browser-test — code.pyret.org's editor assertions, on embed instances and vscode webviews

NOTE(joe): Originally Claude-generated README here with only minor edits

This directory runs the **same assertions** from `code.pyret.org`'s editor test
suite against the three places the Pyret editor renders. It's a **`node:test`**
suite (no extra test-framework dependency) driven through **one runner**:

```
node run.js --env=cpo|embed|embed-static|vscode|vscode-ovsx [--compiler=pyret|ts] [--grep=<regex>] [--suites=all|check-blocks,errors,...]
```

| `--env` | What it drives |
|---|---|
| `cpo` | the reference — `code.pyret.org`'s `/editor` page (reproduces upstream's outcomes) |
| `embed` | the embed API's embedded instance (`<iframe>` in `/embed/embed1.html`) |
| `embed-static` | the **pyret-embed library** (`embed/dist/pyret.js`) driving the **built** `editor.embed.html` artifact on a plain static server — the npm-consumer flow, no CPO server |
| `vscode` | the `pyret-parley.cpo` webview, in headless VS Code for the Web |
| `vscode-ovsx` | the same webview, but with its assets served the way **Open VSX** serves them to the **GitLab Web IDE** — reproduces issue #21 |

### `vscode-ovsx` — the GitLab Web IDE / Open VSX reproduction

`--env=vscode` serves the webview assets from a local static server with correct
MIME types and no size limit (that's how `@vscode/test-web` works), so it can't
see the way the extension breaks in the GitLab Web IDE. There, GitLab resolves
the webview's resources to Open VSX (`open-vsx.org/vscode/unpkg/...`), which
serves **every** file as `Content-Type: text/plain` + `X-Content-Type-Options:
nosniff` (so `<script src>`/`<link>` are refused execution) and **503s** files
over a ~15 MB cap (so the 37 MB `cpo-main.jarr.js` never loads). That's
[issue #21](https://github.com/jpolitz/pyret-parley-vscode/issues/21).

`vscode-ovsx` reproduces exactly that. `shared/ovsx-server.js` serves the built
`dist/web/build/web` with those hostile semantics; the editor HTML is rendered
the same way `getHtmlForWebview` does, with `BASE_URL`/`PYRET` pointed at it. It
does not boot VS Code — it injects a no-op `acquireVsCodeApi` (so beforePyret
takes the real `window.PYRET_IN_VSCODE` branch, the path issue #21 broke)
and passes `initialState` in the URL hash (so `events.js` self-resets the editor
and gains control locally, giving `editorReady` its non-empty CodeMirror).

Hostile mode is the regression test for #21: it fails if the self-contained
template stops booting under nosniff serving. To separate a real regression
from broken harness plumbing, run **faithful** mode (correct MIME, no cap) —
that should boot and pass just like `vscode.dev`:

```bash
node run.js --env=vscode-ovsx                 # hostile (default): the #21 regression test
OVSX_FAITHFUL=1 node run.js --env=vscode-ovsx  # correct MIME/no cap: plumbing check
```

Env vars: `OVSX_FAITHFUL=1` (correct serving), `OVSX_ASSET_ROOT=<dir>` (override
the served build; defaults to `vscode/dist/web/build/web`), `OVSX_CAP_MB=<n>`
(hostile size cap, default 15). A standalone one-shot check that skips the full
spec suite lives in `smoke-ovsx.js`.

### `embed-static` — pyret-embed driving the built `editor.embed.html` artifact

`--env=embed` drives `/editor#controlled=true` through a running CPO server, so
it never loads `build/web/editor.embed.html`: the file an embedding host
actually deploys, rendered once at build time from `src/web/editor.html` +
`.env.embed` (`BASE_URL="."`, relative asset paths, `POSTMESSAGE_ORIGIN="*"`).
`embed-static` serves `build/web` from a plain correct-MIME static server
(`shared/static-server.js`) and embeds it through the **real pyret-embed
library**: a host page (`pages/embed-static-host.html`, mirroring the package's
own `embed/src/basic.html` examples) imports `embed/dist/pyret.js` and calls
`makeEmbedConfig({ src: "/editor.embed.html", ... })` — the exact flow an npm
consumer of `pyret-embed` runs. It catches breakage that lives only in the
built artifact or the library: template variables mis-rendered at build time,
asset references that resolve at a server root but 404 under relative/static
hosting, and pyret-embed API drift against the editor. Needs the CPO build and
`embed/dist` (`npm ci --ignore-scripts && npx webpack` in `embed/`) — no
server, no `code.pyret.org/node_modules`. `EMBED_STATIC_ROOT=<dir>` overrides
the served build dir.

`--compiler` (default `pyret`) additionally selects which **compiler backend**
the environment boots — the stock Pyret-hosted compiler or the TypeScript port
(code.pyret.org's `?compiler=ts` opt-in). Each env adapter maps it to its own
flavor knob: cpo appends `?compiler=ts` to `/editor`, embed forwards it through
the host page to the iframe URL (as the embed library's `compiler` config
option does), and vscode opens the fixture workspace whose settings set
`pyret-parley.compiler: "ts"`. The suites and assertions are identical in both
configurations; `run-all.sh` runs the full env × compiler matrix.

It is **strictly additive**: nothing under `code.pyret.org/` or `vscode/` is
modified; the upstream test files are read as-is.

## Why this is "the same tests"

All three environments render the **same** CPO editor (`editor.html`: CodeMirror,
`#runButton`, `#output`, `.check-block`, `.testing-summary`, "Looks shipshape").
Only how you *reach* that DOM differs — the editor is the main page (cpo), a child
iframe (embed), or a webview frame (vscode) — and a single `findEditorFrame`
helper (the frame with a `#runButton`) locates it in every case.

Two mechanisms keep the inputs and assertions identical to upstream:

- **Same inputs, zero copying.** `shared/load-cpo-specs.js` `require`s the
  unmodified `code.pyret.org/test/*.js` with the mocha globals and `util.js`
  replaced by *recording shims*, capturing the exact `(program, expected)` tuples
  upstream feeds its assertions — the same check-block table, the same
  error→substring table, the same `.arr` chart/table programs. No spec is copied.

- **Same assertions.** `shared/page-assertions.js` is a line-for-line, in-page
  port of the `util.js` predicates (`checkAllTestsPassed` / `testErrorRendersString`
  / `testRunsAndHasCheckBlocks` / `testRunAndUseRepl` / `checkTableRendersCorrectly`);
  `shared/cpo-assertions.js` orchestrates them exactly as `util.js` does, using
  **`node:assert`** for the content checks. Each function carries its `util.js`
  source reference. Because the port runs in-page via Playwright, it can reach a
  vscode webview (Selenium can't), so the *same* assertion code runs in all three
  environments.

  Running `--env=cpo` reproduces upstream's pass/fail on the real `/editor` page,
  which is what makes the port a trustworthy stand-in for `util.js`. (The port was
  first validated head-to-head against the real Selenium `util.js` suite; see the
  git history of this directory — the `util.js` path and the port produced
  identical results before the harness was consolidated onto the port.)

- **Two failure kinds, kept distinct.** Content checks use `node:assert`, so a
  wrong rendering fails as an **`AssertionError`** (with a value diff — "expected
  `failed`, got `Passed`"). Anything that prevents the test from being conducted —
  a program that wouldn't install, a value that never rendered, the REPL erroring —
  throws a **`ProceduralError`** (`shared/errors.js`). Both fail the test; the
  error class tells you which kind at a glance.

### Two webview details the port handles

- The `pyret-parley.cpo` custom editor starts with the interactions panel
  collapsed (`hideInteractions`), but the CPO editor removes that class on the
  first run (`beforePyret.js:1594`), so the REPL becomes available — which is why
  the REPL-driven suites (type-check, tables) run in the webview too.
- The run mode is sticky (`cpo-main.js`: `currentAction`): after a type-check
  run, the plain Run button keeps running type-checked. Since one editor frame is
  reused across suites, `PA.run()` selects the explicit "Run" dropdown item, which
  resets the mode. (Upstream never sees this — a fresh browser per suite.)

## Layout

One suite is harness-local rather than mirrored from upstream:
`big-programs` (`tests/big-programs.js`) generates large programs — many
top-level functions, many `ask` arms, many `data` variants — and asserts they
compile and pass their check blocks. These pin the compiler's stack behavior in
a real browser (fixed ~1MB stack; the CLI respawns node with `--stack-size=8192`
so it never sees these overflows), at scales chosen to fail on the recursive
ANF formulations the ts compiler replaced (the ~800-function bad-stack.arr
report is the funs shape).

```
run.js                  friendly CLI: --env/--grep -> `node --test ...` + PYRET_ENV
tests/suite.test.js     the node:test entry: boots one env, one test() per spec
envs/
  cpo.js                launch chromium, goto /editor
  embed.js              goto /embed/embed1.html, sendReset, find the iframe
  embed-static.js       pyret-embed (embed/dist) against the built editor.embed.html (no CPO server)
  vscode.js             boot @vscode/test-web, open the custom editor, find the webview
  vscode-ovsx.js        serve the webview via a simulated Open VSX (issue #21 repro)
pages/
  embed-static-host.html  host page embedding the artifact via pyret-embed's makeEmbedConfig
shared/
  static-server.js      plain correct-MIME static server (multi-root)
  ovsx-server.js        static server that mimics Open VSX serving (text/plain+nosniff, size cap)
  load-cpo-specs.js     extract exact specs from code.pyret.org/test/*.js (no copying)
  page-assertions.js    in-page DOM port of util.js predicates (window.PA)
  cpo-assertions.js     node:assert assertions mirroring util.js, per content check
  dispatch.js           map a loaded spec to its assertion
  errors.js             ProceduralError (procedural failures vs content AssertionError)
  playwright-page.js    `page` adapter over a Playwright frame
  find-frame.js         locate the editor frame (the one with #runButton)
  browser.js            launch Chromium (system Chrome or Playwright's bundled one)
vscode/fixture-workspace/test.arr   the .arr the custom editor opens
results/                captured run logs
```

## Running

Prereqs (one-time):

```bash
# code.pyret.org built + deps (build/web/js/cpo-main.jarr, code.pyret.org/node_modules)

# vscode extension built
cd vscode && ln -sf ../code.pyret.org/build build && npm install && npm run compile

# this harness's deps
cd ../browser-test && npm install     # playwright + @vscode/test-web

# Chrome (Playwright drives it via executablePath)
export GOOGLE_CHROME_BINARY=/bin/google-chrome
```

Then:

```bash
# cpo + embed load /editor from the CPO server, so start it first:
#   (cd code.pyret.org && BASE_URL=... PYRET=... PORT=4999 ... node src/run.js &)
BASE_URL=http://localhost:4999 node run.js --env=cpo
BASE_URL=http://localhost:4999 node run.js --env=embed

# vscode needs no CPO server (assets come from the built extension):
node run.js --env=vscode

# embed-static needs no CPO server either (build the library once:
#   cd embed && npm ci --ignore-scripts && npx webpack):
node run.js --env=embed-static

# everything (starts the CPO server if needed):
./run-all.sh
```

### Filtering (local dev)

`--grep` is a regex matched against both suite names and individual test names —
so you can scope to one feature or one env:

```bash
node run.js --env=embed  --grep tables       # the tables suite, in the embed instance
node run.js --env=cpo    --grep 'is-not'      # every is-not* test, in /editor
node run.js --env=vscode --grep field-not-found

npm run embed -- --grep tables                # same, via package scripts
```

Other flags: `--suites=check-blocks,errors,...` (default `all`),
`--reporter=spec|tap|dot|junit` (default `spec`). Or skip the wrapper entirely:
`PYRET_ENV=embed node --test --test-name-pattern=tables tests/suite.test.js`.

## Results

Captured run logs land in `results/` (`run-all.sh` writes one per
env x compiler cell). Current state of the matrix: **236 passing,
0 failing** in all six configurations (cpo / embed / vscode, each on
both the stock and the ts compiler), plus the 3 generated
`big-programs` specs (verified on cpo in both compiler flavors).
