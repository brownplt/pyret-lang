#!/usr/bin/env node
// Corpus driver for the parity harnesses, in compile-SERVER mode.
//
// The parity corpora (type-check 174, wf 147) used to spawn a fresh CLI per
// program per compiler; the .arr compiler costs ~1.5-2s just to boot, so the
// corpora dominated CI time. This driver connects to an already-running
// compile server (`pyret -serve`, either compiler -- the same protocol the
// npm client speaks) and sends one compile request per program, reconnecting
// per job because the server closes the connection after each one. Warm
// per-program cost drops to ~0.5s (.arr) / ~0.15s (TS).
//
// Note what this changes about WHAT is compared: diagnostics arrive as
// echo-log/echo-err frames rendered by server.arr / server.ts instead of CLI
// stderr, so these corpora now also pin server-mode rendering parity. The
// CLI rendering surface stays covered by parity-test.sh (which still runs
// the CLI per program; only 28 programs, not a time problem).
//
// Usage:
//   node parity-serve-driver.js <socket> <programs-file> <outdir> <suffix> [--type-check]
//
// <programs-file>: one program path per line.
// For each program P writes <outdir>/<base><suffix>.out (concatenated frame
// contents) and appends "<base> <status>" to <outdir>/results<suffix>.txt,
// where status is 0 (compile-success), 1 (compile-failure), or ERR
// (protocol/connection error -- e.g. the server died).

const path = require('path');
const fs = require('fs');
const WebSocket = require(path.join(__dirname, '../../../node_modules/ws'));

const [sock, programsFile, outdir, suffix, ...flags] = process.argv.slice(2);
if (!sock || !programsFile || !outdir || suffix === undefined) {
  console.error('usage: node parity-serve-driver.js <socket> <programs-file> <outdir> <suffix> [--type-check]');
  process.exit(2);
}
const typeCheck = flags.includes('--type-check');
const langDir = path.resolve(path.join(__dirname, '../../..'));

const programs = fs.readFileSync(programsFile, 'utf8').split('\n').filter((l) => l !== '');
const resultsPath = path.join(outdir, 'results' + suffix + '.txt');
fs.writeFileSync(resultsPath, '');

function optionsFor(program, outfile) {
  return {
    'program': path.resolve(program),
    'outfile': outfile,
    // Project root, as the npm client sets it: relative require-config
    // paths resolve against base-dir (make-standalone.ts).
    'base-dir': langDir,
    'builtin-js-dir': path.join(langDir, 'src/js/trove'),
    'builtin-arr-dir': path.join(langDir, 'src/arr/trove'),
    'require-config': path.join(langDir, 'src/scripts/standalone-configA.json'),
    'deps-file': path.join(langDir, 'build/phaseA/bundled-node-compile-deps.js'),
    'compiled-dir': path.join(outdir, 'compiled' + suffix),
    'checks': 'none',
    'checks-format': 'text',
    'no-check-mode': true,
    'type-check': typeCheck,
    'display-progress': false,
    'standalone-file': path.join(langDir, 'src/js/base/handalone.js')
  };
}

function runOne(program) {
  return new Promise((resolve) => {
    const base = path.basename(program, '.arr');
    const outfile = path.join(outdir, base + suffix + '.jarr');
    const textPath = path.join(outdir, base + suffix + '.out');
    let text = '';
    let status = 'ERR';
    let settled = false;
    const client = new WebSocket('ws+unix://' + sock);
    const finish = () => {
      if (settled) { return; }
      settled = true;
      clearTimeout(timer);
      fs.writeFileSync(textPath, text);
      fs.appendFileSync(resultsPath, base + ' ' + status + '\n');
      resolve();
    };
    const timer = setTimeout(() => {
      text += '\nDRIVER-TIMEOUT\n';
      try { client.terminate(); } catch (_e) { /* closing anyway */ }
      finish();
    }, 120000);
    client.on('error', (err) => {
      text += '\nDRIVER-WS-ERROR: ' + err + '\n';
      finish();
    });
    client.on('open', () => {
      client.send(JSON.stringify({ command: 'compile', compileOptions: JSON.stringify(optionsFor(program, outfile)) }));
    });
    client.on('message', (message) => {
      const parsed = JSON.parse(message);
      if (parsed.type === 'echo-log' || parsed.type === 'echo-err') {
        text += parsed.contents;
      } else if (parsed.type === 'compile-success') {
        status = '0';
      } else if (parsed.type === 'compile-failure') {
        status = '1';
      }
    });
    // The server closes the connection when the job is done; that close is
    // the end-of-job signal (results may arrive just before it).
    client.on('close', finish);
  });
}

(async () => {
  for (const p of programs) {
    await runOne(p);
  }
  console.log(suffix + ': ' + programs.length + ' programs driven');
})();
