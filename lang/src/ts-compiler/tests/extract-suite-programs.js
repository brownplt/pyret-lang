#!/usr/bin/env node
// Extract the inline program strings from the in-suite well-formedness /
// compile-error tests into standalone .arr files, for compile-parity runs
// (see wf-parity.sh).
//
// The in-suite files (tests/pyret/tests/test-well-formed.arr,
// tests/pyret/tests/test-compile-errors.arr) hold years of accumulated
// wf/scope-error programs as `run-str("...")` / `cok("...")` string literals.
// Extracting at HARNESS RUNTIME (rather than checking in a generated corpus)
// keeps the parity corpus automatically in sync with the in-suite tests.
//
// Handles the two literal forms those files use: double-quoted strings
// (only \n, \", \\ escapes occur) and ```raw``` multi-line strings.
//
// Each program is written BYTE-EXACT to <outdir>/p<NNN>.arr -- no provenance
// header, because any added line would shift every srcloc in the error
// messages under comparison. Provenance goes to <outdir>/manifest.txt.
//
// Usage: node extract-suite-programs.js <outdir> <file.arr> [<file.arr> ...]

const fs = require('fs');
const path = require('path');

const [outdir, ...files] = process.argv.slice(2);
if (!outdir || files.length === 0) {
  console.error('usage: node extract-suite-programs.js <outdir> <file.arr> ...');
  process.exit(2);
}
fs.mkdirSync(outdir, { recursive: true });

// run-str( or cok( followed by either a "..." literal or a ```...``` literal.
const CALL = /\b(?:run-str|cok)\(\s*(?:"((?:[^"\\]|\\.)*)"|```([\s\S]*?)```)/g;

function unescape(s) {
  return s.replace(/\\(.)/g, (_, c) =>
    c === 'n' ? '\n' : c === 't' ? '\t' : c === 'r' ? '\r' : c);
}

const seen = new Map(); // program text -> first provenance (dedupe)
for (const file of files) {
  const src = fs.readFileSync(file, 'utf8');
  let m;
  while ((m = CALL.exec(src)) !== null) {
    const prog = m[1] !== undefined ? unescape(m[1]) : m[2];
    const line = src.slice(0, m.index).split('\n').length;
    if (!seen.has(prog)) { seen.set(prog, path.basename(file) + ':' + line); }
  }
}

const manifest = [];
let i = 0;
for (const [prog, where] of seen) {
  i += 1;
  const name = 'p' + String(i).padStart(3, '0');
  // Ensure a trailing newline (the in-suite compile path adds nothing, but a
  // final line with no newline renders identically in both compilers anyway;
  // keep the bytes as-is except guaranteeing the file is not empty).
  fs.writeFileSync(path.join(outdir, name + '.arr'), prog);
  manifest.push(name + '  ' + where);
}
fs.writeFileSync(path.join(outdir, 'manifest.txt'), manifest.join('\n') + '\n');
console.log(String(seen.size));
