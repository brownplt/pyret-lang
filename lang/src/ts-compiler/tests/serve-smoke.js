// Smoke test client for the Pyret compile server protocol (server.arr +
// server.js / src/ts-compiler/src/server.ts). Driven by serve-test.sh
// (`make ts-serve-test`); also usable by hand.
//
// Usage: node serve-smoke.js <socket-path> <program.arr> <outfile.jarr>
//        node serve-smoke.js <socket-path> --shutdown
//
// Connects over ws+unix, sends a compile request shaped like the npm
// `pyret` client (client-lib.js), logs every message received verbatim,
// and exits 0 on compile-success (after checking the outfile exists),
// 1 on compile-failure, 3 on close-without-result. In --shutdown mode,
// sends the npm client's shutdown command and exits 0 when the server
// closes the connection.

const path = require('path');
const fs = require('fs');
const WebSocket = require(path.join(__dirname, '../../../node_modules/ws'));

const sock = process.argv[2];
// Absolute paths, as the npm client's options carry: the server resolves the
// program's file dependency against base-dir, so a cwd-relative program path
// would double-resolve and "Cannot find import".
const program = process.argv[3] === '--shutdown' ? '--shutdown' : path.resolve(process.argv[3]);
const outfile = process.argv[4] && path.resolve(process.argv[4]);

const langDir = path.resolve(path.join(__dirname, '../../..'));

const pyretOptions = {
  'program': program,
  'outfile': outfile,
  // The npm client sets base-dir to the PROJECT root (the parent of .pyret/,
  // client-lib.js), and relative paths in the require-config resolve against
  // it (make-standalone.ts). standalone-configA.json's entries are relative
  // to lang/, so that is the base dir here -- NOT dirname(program).
  'base-dir': langDir,
  'builtin-js-dir': path.join(langDir, 'src/js/trove'),
  'builtin-arr-dir': path.join(langDir, 'src/arr/trove'),
  'require-config': process.env.SMOKE_REQUIRE_CONFIG || path.join(langDir, 'src/scripts/standalone-configA.json'),
  'deps-file': path.join(langDir, 'build/phaseA/bundled-node-compile-deps.js'),
  'compiled-dir': process.env.SMOKE_COMPILED_DIR || path.join(langDir, 'build/serve-smoke-compiled'),
  'checks': 'none',
  'checks-format': 'text',
  'no-check-mode': true,
  'type-check': false,
  'standalone-file': path.join(langDir, 'src/js/base/handalone.js')
};

const shutdownMode = program === '--shutdown';

const client = new WebSocket('ws+unix://' + sock);
let done = false;

client.on('error', function (err) {
  console.error('CLIENT-ERROR: ' + err);
  process.exit(2);
});

client.on('open', function () {
  if (shutdownMode) {
    // Same message the npm client's --shutdown sends (client-lib.js).
    client.send(JSON.stringify({ command: 'shutdown' }));
    return;
  }
  client.send(JSON.stringify({ command: 'compile', compileOptions: JSON.stringify(pyretOptions) }));
});

client.on('message', function (message) {
  console.log('RAW: ' + message);
  const parsed = JSON.parse(message);
  if (parsed.type === 'compile-failure') {
    done = true;
    process.exit(1);
  } else if (parsed.type === 'compile-success') {
    done = true;
    if (!fs.existsSync(outfile)) {
      console.error('compile-success but outfile missing: ' + outfile);
      process.exit(4);
    }
    console.log('OUTFILE-OK: ' + outfile);
    process.exit(0);
  }
});

client.on('close', function () {
  if (shutdownMode) {
    console.log('SHUTDOWN-SENT');
    process.exit(0);
  }
  if (!done) {
    console.error('CLOSED-WITHOUT-RESULT');
    process.exit(3);
  }
});

setTimeout(function () {
  console.error('TIMEOUT');
  process.exit(5);
}, 600000);
