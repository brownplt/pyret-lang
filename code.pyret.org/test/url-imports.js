var tester = require("../test-util/util.js");

// The vscode test below names a remote base that must never be fetched: its
// imports resolve from the fixture workspace on disk. If local resolution
// breaks, the network fallback fails against this address instead of quietly
// loading real files. Bootstrap's own files are tested in its curriculum repo
// (bootstrapworld/curriculum, starter-file-tests/).
var UNREACHABLE = "https://example.invalid/starter-files";

var tests = [];

// Hermetic cases served by the dev server itself (server.js serves test-util/
// statically in development), so they need no outside network. The "/app"
// path segment need not exist; it is there for "../" to consume, the same
// way the starter files' base URLs work. A fixture that url-file imports
// another fixture writes {{FIXTURE_ORIGIN}} for its own origin; both servers
// fill it in when serving .arr files under pyret-programs/url-imports/.
// browser-test serves these fixtures itself (PYRET_FIXTURE_BASE, see its
// run.js) so that the envs which run no CPO server can reach them too; the
// mocha suite has no such server and falls back to BASE_URL, where the dev
// server's test-util mount serves the same tree same-origin.
var base = process.env.PYRET_FIXTURE_BASE || process.env.BASE_URL;
if (base) {
  var localBase = base.replace(/\/+$/, "") + "/pyret-programs/url-imports";
  tests.push(
    { name: "local url import (no external network)",
      program:
        'import url("' + localBase + '/lib/provided.arr") as P\n' +
        'check:\n' +
        '  P.shared-value is "from-url-imports-lib"\n' +
        'end',
      specs: [[["Passed"]]],
      options: { timeout: 60000 } },
    { name: "local url-file with ../ traversal (no external network)",
      program:
        'include url-file("' + localBase + '/app", "../lib/provided.arr")\n' +
        'check:\n' +
        '  shared-value is "from-url-imports-lib"\n' +
        'end',
      specs: [[["Passed"]]],
      options: { timeout: 60000 } },
    { name: "local use context url-file (no external network)",
      program:
        'use context url-file("' + localBase + '/app", "../lib/mini-context.arr")\n' +
        'check:\n' +
        '  context-marker is 42\n' +
        'end',
      specs: [[["Passed"]]],
      options: { timeout: 60000 } },
    // A url-file context and a url-file include, both via "../", where the
    // included module has a url-file context of its own.
    { name: "local included module with its own use context url-file (no external network)",
      program:
        'use context url-file("' + localBase + '/app", "../lib/mini-context.arr")\n' +
        'include url-file("' + localBase + '/app", "../lib/nested-context.arr")\n' +
        'check:\n' +
        '  context-marker is 42\n' +
        '  nested-context-marker is 43\n' +
        'end',
      specs: [[["Passed"], ["Passed"]]],
      options: { timeout: 60000 } }
  );
}

// The vscode environment is the only one whose host implements the filesystem
// RPCs, so it is the only place url-file's LOCAL branch can run. Its fixture
// workspace sets pyret-parley.urlFileMode = "local-if-present" (see
// browser-test/vscode/fixture-workspace/.vscode/settings.json) and mirrors the
// starter-file layout on disk:
//
//   algebra-2/test.arr             <- the open editor tab
//   libraries/core.arr             <- copy of a starter-files core.arr, + one marker
//   libraries/unit-clock-library.arr  <- copy, + one marker, and it carries its
//                                        OWN `use context url-file(..., "core.arr")`
//
// This test checks the two markers, which is what makes the local branch
// observable, and specifically what distinguishes correct load-path tracking
// from the bug it replaced:
//
//   came-from-local-filesystem  -- the tab's own "../libraries/core.arr"
//                                  resolved relative to algebra-2/.
//   core-marker-seen            -- unit-clock-library.arr's bare "core.arr"
//                                  resolved relative to ITS directory
//                                  (libraries/), not the tab's. If that ever
//                                  regresses to the tab's directory,
//                                  algebra-2/core.arr does not exist, the
//                                  import silently falls back to the network,
//                                  and upstream core.arr has no such binding --
//                                  so this fails instead of going green.
if (process.env.PYRET_ENV === "vscode") {
  tests.push(
    { name: "starter-file shape resolves from the workspace, per-module load paths",
      program:
        'use context url-file("' + UNREACHABLE + '/algebra-2", "../libraries/core.arr")\n' +
        'include url-file("' + UNREACHABLE + '/algebra-2", "../libraries/unit-clock-library.arr")\n' +
        'check:\n' +
        '  came-from-local-filesystem is "vscode-fixture-workspace"\n' +
        '  core-marker-seen is "vscode-fixture-workspace"\n' +
        '  deg-to-rad(0) is 0\n' +
        'end',
      specs: [[["Passed"], ["Passed"], ["Passed"]]],
      options: { timeout: 180000 } }
  );
}

describe("url and url-file imports (non-embedded editor)", function() {
  before(tester.setupMulti("url and url-file imports"));
  after(tester.teardownMulti);

  tests.forEach(function(t) {
    tester.testRunsAndHasCheckBlocks(it, t.name, t.program, t.specs, t.options);
  });
});
