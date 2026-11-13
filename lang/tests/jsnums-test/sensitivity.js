'use strict';

// Sensitivity check: for each hunk (or named group of hunks) in the diff of
// js-numbers.js between BASE and FIXED, revert that hunk alone against the
// fixed file and count the frozen cases that fail. A fix whose reversion
// fails nothing is not observed by the suite.
//
//   node sensitivity.js --base <git-ref> --fixed <git-ref|path> [--groups groups.json] [--list]
//
// groups.json: { "<name>": { "hunks": [1, 4], "note": "..." }, ... } with
// 1-based ordinals of the zero-context hunks (git diff -U0) between the
// base and fixed files; --list prints them.

const fs = require('fs');
const os = require('os');
const path = require('path');
const { execFileSync } = require('child_process');
const { loadLibrary } = require('./load');
const { runAll } = require('./watchdog');

const REPO = path.join(__dirname, '..', '..', '..');
const FILE = 'lang/src/js/base/js-numbers.js';
const CONFIGS = [28, 30, 26];

function git(...args) { return execFileSync('git', args, { cwd: REPO, encoding: 'utf8', maxBuffer: 1 << 28 }); }

function readRef(ref) {
  if (fs.existsSync(ref)) return fs.readFileSync(ref, 'utf8');
  return git('show', ref + ':' + FILE);
}

function parseHunks(diff) {
  const hunks = [];
  let cur = null;
  for (const line of diff.split('\n')) {
    const m = /^@@ -(\d+)(?:,(\d+))? \+(\d+)(?:,(\d+))? @@(.*)$/.exec(line);
    if (m) { cur = { header: line, oldStart: +m[1], newStart: +m[3], newLen: m[4] === undefined ? 1 : +m[4], lines: [] }; hunks.push(cur); continue; }
    if (!cur) continue;
    if (line.startsWith('\\')) continue;
    if (/^[ +-]/.test(line)) cur.lines.push(line);
  }
  for (const h of hunks) {
    h.post = h.lines.filter(l => l[0] !== '-').map(l => l.slice(1));
    h.pre = h.lines.filter(l => l[0] !== '+').map(l => l.slice(1));
  }
  return hunks;
}

// Revert the given hunks (bottom-up) against the fixed file's lines.
function revert(fixedLines, hunks) {
  const out = fixedLines.slice();
  for (const h of [...hunks].sort((a, b) => b.newStart - a.newStart)) {
    const at = h.newLen === 0 ? h.newStart : h.newStart - 1;
    const have = out.slice(at, at + h.post.length);
    if (have.join('\n') !== h.post.join('\n')) throw new Error('hunk ' + h.header + ' does not match the fixed file');
    out.splice(at, h.post.length, ...h.pre);
  }
  return out;
}

async function runSuite(libPath) {
  for (const bits of CONFIGS) {
    try { loadLibrary(libPath, bits); } catch (e) { return { total: 0, fails: [{ id: 'load', why: 'library failed to load: ' + e.message }], loadError: true }; }
  }
  const all = await runAll(libPath, CONFIGS);
  const fails = [];
  for (const [key, r] of all) if (!r.ok) fails.push({ id: key, why: r.why });
  return { total: all.size, fails };
}

async function main(argv) {
  const opt = {};
  for (let i = 0; i < argv.length; i++) {
    if (argv[i] === '--base') opt.base = argv[++i];
    else if (argv[i] === '--fixed') opt.fixed = argv[++i];
    else if (argv[i] === '--groups') opt.groups = argv[++i];
    else if (argv[i] === '--list') opt.list = true;
  }
  if (!opt.base || !opt.fixed) throw new Error('need --base and --fixed');
  const fixedText = readRef(opt.fixed);
  const tmp = fs.mkdtempSync(path.join(process.env.TMPDIR || os.tmpdir(), 'jsnums-sens-'));
  const fixedPath = path.join(tmp, 'fixed.js');
  fs.writeFileSync(fixedPath, fixedText);
  const basePath = path.join(tmp, 'base.js');
  fs.writeFileSync(basePath, readRef(opt.base));
  // git diff --no-index exits 1 when the files differ
  const diff = require('child_process').spawnSync('git', ['diff', '--no-index', '-U0', '--', basePath, fixedPath], { cwd: REPO, encoding: 'utf8', maxBuffer: 1 << 28 }).stdout;
  const hunks = parseHunks(diff);
  console.log('hunks in diff: ' + hunks.length);
  if (opt.list) { hunks.forEach((h, i) => console.log((i + 1) + ': ' + h.header)); return; }
  const groups = opt.groups ? JSON.parse(fs.readFileSync(opt.groups, 'utf8')) : Object.fromEntries(hunks.map((h, i) => ['hunk-' + (i + 1), { hunks: [i + 1] }]));
  const covered = new Set();
  const baseline = await runSuite(fixedPath);
  console.log('fixed file: ' + baseline.fails.length + ' failures out of ' + baseline.total);
  const baselineIds = new Set(baseline.fails.map(f => f.id));
  console.log('\n| group | hunks | new failures | first failing cases |');
  console.log('|---|---|---:|---|');
  const fixedLines = fixedText.split('\n');
  let gi = 0;
  for (const [name, g] of Object.entries(groups)) {
    const hs = g.hunks.map(i => { covered.add(i); return hunks[i - 1]; });
    const variant = path.join(tmp, 'group-' + (++gi) + '.js');
    fs.writeFileSync(variant, revert(fixedLines, hs).join('\n'));
    const r = await runSuite(variant);
    const fresh = r.fails.filter(f => !baselineIds.has(f.id));
    const ids = fresh.slice(0, 4).map(f => f.id).join(', ') + (fresh.length > 4 ? ', ...' : '');
    console.log('| ' + [name, g.hunks.join(' '), fresh.length, r.loadError ? 'library failed to load' : ids].join(' | ') + ' |');
  }
  const uncovered = hunks.map((h, i) => i + 1).filter(i => !covered.has(i));
  if (uncovered.length) console.log('\nhunks in no group: ' + uncovered.join(' '));
  fs.rmSync(tmp, { recursive: true, force: true });
}

if (require.main === module) main(process.argv.slice(2)).catch(e => { console.error(e); process.exit(1); });
module.exports = { parseHunks, revert };
