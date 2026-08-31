var tester = require("../test-util/util.js");

/*
 * Tests for live curricular files from Bootstrap. It's probably worth
 * revisiting this occasionally to update to newer files. The original version
 * was written in summer 2026 and refers to files from then.
 */

var PINNED = "4aab0a88c78ddc12d279f53199c688c75fd40a5c";
var RAW = "https://raw.githubusercontent.com/bootstrapworld/starter-files/" + PINNED;

var tests = [
  { name: "url import of a pinned raw file",
    program:
      'import url("' + RAW + '/libraries/core.arr") as CORE\n' +
      'check:\n' +
      '  CORE.string-trim("  hi  ") is "hi"\n' +
      'end',
    specs: [[["Passed"]]],
    options: { timeout: 90000 } },

  { name: "import url-file relative to the base URL",
    program:
      'import url-file("' + RAW + '/libraries", "core.arr") as CORE\n' +
      'check:\n' +
      '  CORE.string-trim("  hi  ") is "hi"\n' +
      'end',
    specs: [[["Passed"]]],
    options: { timeout: 90000 } },

  { name: "import url-file with ../ traversal above the base URL",
    program:
      'import url-file("' + RAW + '/algebra-2", "../libraries/core.arr") as CORE\n' +
      'check:\n' +
      '  CORE.string-trim("  hi  ") is "hi"\n' +
      'end',
    specs: [[["Passed"]]],
    options: { timeout: 90000 } },

  { name: "use context url-file",
    program:
      'use context url-file("' + RAW + '/libraries", "core.arr")\n' +
      'check:\n' +
      '  string-trim("  hi  ") is "hi"\n' +
      '  round-digits(3.14159, 2) is 3.14\n' +
      'end',
    specs: [[["Passed"], ["Passed"]]],
    options: { timeout: 90000 } },

  // The shape of the fall2026 Unit Clock starter file: a url-file context
  // and a url-file include, both reaching a sibling directory via "../",
  // where the included library has a url-file context of its own.
  { name: "starter-file shape: use context and include, both via ../",
    program:
      'use context url-file("' + RAW + '/algebra-2", "../libraries/core.arr")\n' +
      'include url-file("' + RAW + '/algebra-2", "../libraries/unit-clock-library.arr")\n' +
      'check:\n' +
      '  deg-to-rad(0) is 0\n' +
      '  rad-to-deg(0) is 0\n' +
      'end',
    specs: [[["Passed"], ["Passed"]]],
    options: { timeout: 180000 } },
];

// Hermetic cases served by the dev server itself (server.js serves test-util/
// statically in development), so they need no outside network. The "/app"
// path segment need not exist; it is there for "../" to consume, the same
// way the starter files' base URLs work.
// browser-test serves these fixtures itself (PYRET_FIXTURE_BASE, see its
// run.js) so that the envs which run no CPO server can reach them too; the
// mocha suite has no such server and falls back to BASE_URL, where the dev
// server's test-util mount serves the same tree same-origin.
var base = process.env.PYRET_FIXTURE_BASE || process.env.BASE_URL;
if (base) {
  var localBase = base.replace(/\/+$/, "") + "/pyret-programs/url-imports";
  tests.push(
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
//   libraries/core.arr             <- copy of the pinned core.arr, + one marker
//   libraries/unit-clock-library.arr  <- copy, + one marker, and it carries its
//                                        OWN `use context url-file(..., "core.arr")`
//
// So the "../ traversal" and "starter-file shape" tests above resolve off disk
// there instead of over the network -- same programs, same assertions,
// different resolution path.
//
// This test adds the two markers, which is what makes the local branch
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
        'use context url-file("' + RAW + '/algebra-2", "../libraries/core.arr")\n' +
        'include url-file("' + RAW + '/algebra-2", "../libraries/unit-clock-library.arr")\n' +
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
