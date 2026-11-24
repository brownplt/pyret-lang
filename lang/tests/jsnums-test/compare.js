'use strict';

// Runs the frozen suite against two js-numbers.js files and tabulates,
// per case family, which cases fail on A and pass on B, fail on both,
// or pass on A and fail on B.
//
//   node compare.js <A.js> <B.js> [--details] [--json out.json]

const fs = require('fs');
const { runAll } = require('./watchdog');

const CONFIGS = [28, 30, 26];

async function main(argv) {
  const [a, b] = argv.filter(x => !x.startsWith('--') && !argv[argv.indexOf(x) - 1]?.startsWith('--json'));
  const details = argv.includes('--details');
  const jsonOut = argv.includes('--json') ? argv[argv.indexOf('--json') + 1] : null;
  const ra = await runAll(a, CONFIGS), rb = await runAll(b, CONFIGS);
  const fam = new Map();
  for (const [key, x] of ra) {
    const y = rb.get(key);
    const k = x.op + ' / ' + x.family;
    if (!fam.has(k)) fam.set(k, { op: x.op, family: x.family, total: 0, fixed: [], both: [], regressed: [], pass: 0 });
    const f = fam.get(k);
    f.total++;
    if (!x.ok && y.ok) f.fixed.push({ key, whyA: x.why });
    else if (!x.ok && !y.ok) f.both.push({ key, whyA: x.why, whyB: y.why });
    else if (x.ok && !y.ok) f.regressed.push({ key, why: y.why });
    else f.pass++;
  }
  const rows = [...fam.values()].sort((p, q) => p.op.localeCompare(q.op) || p.family.localeCompare(q.family));
  console.log('| op | family | cases x configs | fail A, pass B | fail both | pass A, fail B |');
  console.log('|---|---|---:|---:|---:|---:|');
  let tf = 0, tb = 0, tr = 0, tt = 0;
  for (const f of rows) {
    tt += f.total;
    if (f.fixed.length + f.both.length + f.regressed.length === 0) continue;
    console.log('| ' + [f.op, f.family, f.total, f.fixed.length, f.both.length, f.regressed.length].join(' | ') + ' |');
    tf += f.fixed.length; tb += f.both.length; tr += f.regressed.length;
  }
  console.log('| **total** | | ' + tt + ' | ' + tf + ' | ' + tb + ' | ' + tr + ' |');
  if (details) {
    for (const f of rows) {
      if (f.both.length) { console.log('\n### fail on both: ' + f.op + ' / ' + f.family); for (const x of f.both.slice(0, 12)) console.log('- ' + x.key + ': A: ' + x.whyA + ' | B: ' + x.whyB); if (f.both.length > 12) console.log('- ... ' + (f.both.length - 12) + ' more'); }
      if (f.regressed.length) { console.log('\n### pass on A, fail on B: ' + f.op + ' / ' + f.family); for (const x of f.regressed.slice(0, 12)) console.log('- ' + x.key + ': ' + x.why); if (f.regressed.length > 12) console.log('- ... ' + (f.regressed.length - 12) + ' more'); }
    }
  }
  if (jsonOut) fs.writeFileSync(jsonOut, JSON.stringify(rows, (k, v) => typeof v === 'bigint' ? v.toString() : v, 1));
}

if (require.main === module) main(process.argv.slice(2)).catch(e => { console.error(e); process.exit(1); });
