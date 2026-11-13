'use strict';

// Consensus and freeze tool. Runs the oracle harnesses on the pending and
// disputed cases of each case file, freezes the cases every oracle agreed
// on (with provenance), and writes the disagreement report.
//
//   node freeze.js [--refreeze] [--report <file>] [case-file ...]

const fs = require('fs');
const path = require('path');
const os = require('os');
const { spawnSync } = require('child_process');
const F = require('../numfmt');
const policy = require('../policy');
const { OPS } = require('../jsops');

const HERE = __dirname;
const CASES_DIR = path.join(HERE, '..', 'cases');
const REPORT = path.join(HERE, 'disagreements.md');

function findJulia() {
  const candidates = [process.env.JULIA, 'julia', path.join(os.homedir(), '.juliaup', 'bin', 'julia')].filter(Boolean);
  for (const c of candidates) {
    const r = spawnSync(c, ['--version'], { encoding: 'utf8' });
    if (r.status === 0) return c;
  }
  return null;
}

const ORACLES = {
  racket: {
    cmd: () => ['racket', [path.join(HERE, 'racket.rkt')]],
    version: () => (spawnSync('racket', ['--version'], { encoding: 'utf8' }).stdout.match(/v([\d.]+ \[\w+\])/) || [])[1],
  },
  julia: {
    cmd: () => { const j = findJulia(); return j && [j, [path.join(HERE, 'julia.jl')]]; },
    version: () => { const j = findJulia(); return j && (spawnSync(j, ['--version'], { encoding: 'utf8' }).stdout.match(/([\d.]+)/) || [])[1]; },
  },
  python: {
    cmd: () => ['python3', [path.join(HERE, 'python.py')]],
    version: () => {
      const v = (spawnSync('python3', ['--version'], { encoding: 'utf8' }).stdout.match(/([\d.]+)/) || [])[1];
      const m = spawnSync('python3', ['-c', 'import mpmath; print(mpmath.__version__)'], { encoding: 'utf8' }).stdout.trim();
      return v && 'Python ' + v + ' mpmath ' + m;
    },
  },
};

function runOracle(name, lines) {
  const spec = ORACLES[name].cmd();
  if (!spec) return null;
  const r = spawnSync(spec[0], spec[1], { input: lines.join('\n') + '\n', encoding: 'utf8', maxBuffer: 1 << 30 });
  if (r.status !== 0) {
    throw new Error(name + ' harness failed (exit ' + r.status + '):\n' + r.stderr);
  }
  const out = new Map();
  for (const line of r.stdout.split('\n')) {
    if (!line) continue;
    const i = line.indexOf('\t');
    out.set(line.slice(0, i), line.slice(i + 1));
  }
  return out;
}

function tsv(c, op, oargs) {
  for (const a of oargs) if (/[\t\n]/.test(a)) throw new Error('tab or newline in arg of ' + c.id);
  return [c.id, op, ...oargs].join('\t');
}

function pushRules(set, rules) { for (const r of rules) set.add(r); }

function consensus(op, c, oargs, raws) {
  const rules = new Set();
  const normalized = {};
  const ran = [];
  for (const [name, raw] of Object.entries(raws)) {
    if (raw === undefined) continue;
    const parsed = policy.parseRaw(raw);
    if (parsed === null) continue;
    ran.push(name);
    if (parsed.harnessError) { normalized[name] = parsed; continue; }
    const n = policy.normalize(op, c.args, parsed);
    normalized[name] = n.result;
    pushRules(rules, n.rules);
  }
  const first = ran.length ? normalized[ran[0]] : null;
  const agree = ran.length >= 2 && ran.every(n => !normalized[n].harnessError && policy.sameResult(normalized[n], first));
  return { ran, normalized, agree, rules };
}

function formatCase(c) {
  return JSON.stringify(c);
}

function writeCaseFile(file, data) {
  const lines = ['{'];
  const keys = Object.keys(data);
  keys.forEach((k, i) => {
    const last = i === keys.length - 1 ? '' : ',';
    if (Array.isArray(data[k]) && data[k].length && typeof data[k][0] === 'object') {
      lines.push('  ' + JSON.stringify(k) + ': [');
      data[k].forEach((c, j) => lines.push('    ' + formatCase(c) + (j === data[k].length - 1 ? '' : ',')));
      lines.push('  ]' + last);
    } else {
      lines.push('  ' + JSON.stringify(k) + ': ' + JSON.stringify(data[k]) + last);
    }
  });
  lines.push('}');
  fs.writeFileSync(file, lines.join('\n') + '\n');
}

function orderedCase(c, keys) {
  const out = {};
  for (const k of keys) if (c[k] !== undefined) out[k] = c[k];
  return out;
}
const FROZEN_KEYS = ['id', 'family', 'args', 'oracle_args', 'expect', 'mode', 'agreed', 'policy', 'policy-override', 'raw', 'note'];
const DISPUTED_KEYS = ['id', 'family', 'args', 'oracle_args', 'raw', 'normalized', 'policy', 'note'];
const PENDING_KEYS = ['id', 'family', 'args', 'note'];

function main(argv) {
  let refreeze = false, report = REPORT;
  const files = [];
  for (let i = 0; i < argv.length; i++) {
    if (argv[i] === '--refreeze') refreeze = true;
    else if (argv[i] === '--report') report = argv[++i];
    else files.push(argv[i]);
  }
  const names = files.length ? files.map(f => path.basename(f)) : fs.readdirSync(CASES_DIR).filter(f => f.endsWith('.json')).sort();
  const versions = {};
  for (const name of Object.keys(ORACLES)) {
    const v = ORACLES[name].version();
    if (v) versions[name] = v;
  }
  const available = Object.keys(versions);
  if (available.length < 2) throw new Error('need at least two oracles installed, found: ' + available.join(', '));
  console.log('oracles: ' + available.map(n => n + ' ' + versions[n]).join(', '));

  const disputes = [];
  const totals = { frozen: 0, disputed: 0, pending: 0 };
  for (const name of names) {
    const file = path.join(CASES_DIR, name);
    const data = JSON.parse(fs.readFileSync(file, 'utf8'));
    const op = data.op;
    if (!OPS[op]) throw new Error(name + ': unknown op ' + op);
    const frozen = refreeze ? [] : (data.cases || []);
    const todo = (refreeze ? (data.cases || []) : []).concat(data.pending || [], data.disputed || []);
    const seen = new Set(frozen.map(c => c.id));
    for (const c of todo) {
      if (seen.has(c.id)) throw new Error(name + ': duplicate id ' + c.id);
      seen.add(c.id);
      if (!Array.isArray(c.args) || c.args.length !== OPS[op].kinds.length) throw new Error(name + ': bad args for ' + c.id);
      OPS[op].kinds.forEach((k, i) => policy.argsOf(op, c.args));
    }
    const pre = new Map();
    const lines = [];
    for (const c of todo) {
      const o = policy.oracleArgs(op, c.args);
      pre.set(c.id, o);
      lines.push(tsv(c, op, o.args));
    }
    const outputs = {};
    if (lines.length) {
      for (const oname of available) {
        const r = runOracle(oname, lines);
        if (r) outputs[oname] = r;
      }
    }
    const newFrozen = [], disputed = [], pending = [];
    for (const c of todo) {
      const o = pre.get(c.id);
      const raws = {};
      for (const oname of Object.keys(outputs)) raws[oname] = outputs[oname].get(c.id);
      const k = consensus(op, c, o.args, raws);
      const rules = new Set(o.rules);
      pushRules(rules, k.rules);
      const base = { id: c.id, family: c.family, args: c.args, note: c.note };
      if (o.args.some((a, i) => a !== c.args[i])) base.oracle_args = o.args;
      if (k.ran.length === 0) { pending.push(orderedCase(base, PENDING_KEYS)); continue; }
      if (k.agree) {
        const e = policy.expectation(op, c.args, k.normalized[k.ran[0]]);
        pushRules(rules, e.rules);
        const fc = Object.assign(base, { expect: e.expect, mode: e.mode });
        const pureOverride = rules.size > 0 && [...rules].every(r => policy.OVERRIDE_RULES.has(r));
        if (pureOverride) {
          // every oracle's raw result was discarded and replaced by the
          // same override regardless of what it said -- there is no
          // agreement to report, only the override and what it threw away.
          fc['policy-override'] = [...rules].sort();
          fc.raw = {}; for (const n of k.ran) fc.raw[n] = raws[n];
        } else {
          fc.agreed = k.ran.slice().sort();
          if (rules.size) { fc.policy = [...rules].sort(); fc.raw = {}; for (const n of k.ran) fc.raw[n] = raws[n]; }
        }
        newFrozen.push(orderedCase(fc, FROZEN_KEYS));
      } else {
        const dc = Object.assign(base, { raw: {}, normalized: {} });
        for (const n of k.ran) { dc.raw[n] = raws[n]; dc.normalized[n] = k.normalized[n]; }
        if (rules.size) dc.policy = [...rules].sort();
        disputed.push(orderedCase(dc, DISPUTED_KEYS));
        disputes.push({ file: name, op, c: dc });
      }
    }
    const out = { op, kinds: OPS[op].kinds };
    if (data.generated) out.generated = data.generated;
    out.oracles = Object.assign({}, data.oracles || {}, versions);
    out.cases = frozen.concat(newFrozen);
    out.disputed = disputed;
    out.pending = pending;
    writeCaseFile(file, out);
    totals.frozen += newFrozen.length; totals.disputed += disputed.length; totals.pending += pending.length;
    console.log(name + ': frozen +' + newFrozen.length + ' (total ' + out.cases.length + '), disputed ' + disputed.length + ', pending ' + pending.length);
  }
  selfCheck();
  writeReport(report, versions, disputes);
  console.log('frozen ' + totals.frozen + ', disputed ' + totals.disputed + ', pending ' + totals.pending);
}

// The policy module converts exact arguments to doubles itself; that
// conversion must agree with the frozen toFixnum consensus.
function selfCheck() {
  const file = path.join(CASES_DIR, 'toFixnum.json');
  if (!fs.existsSync(file)) return;
  const data = JSON.parse(fs.readFileSync(file, 'utf8'));
  for (const c of data.cases) {
    if (!F.isExactLit(c.args[0]) || !c.expect.double) continue;
    const mine = F.doubleToBits(F.exactToDouble(F.parseExact(c.args[0])));
    if (mine !== c.expect.double) throw new Error('numfmt.exactToDouble disagrees with oracle consensus on ' + c.id);
  }
}

function writeReport(file, versions, disputes) {
  const lines = [];
  lines.push('# Oracle disagreements');
  lines.push('');
  lines.push('Generated by oracle/freeze.js. Oracles: ' + Object.entries(versions).map(([n, v]) => n + ' ' + v).join(', ') + '.');
  lines.push('');
  lines.push('Cases below are not frozen; the runner does not see them.');
  lines.push('');
  lines.push('| file | id | args | oracle args | ' + Object.keys(versions).join(' | ') + ' | policy |');
  lines.push('|---|---|---|---|' + Object.keys(versions).map(() => '---').join('|') + '|---|');
  const cell = s => String(s === undefined ? '' : s).replace(/\|/g, '\\|');
  for (const d of disputes) {
    const c = d.c;
    lines.push('| ' + [d.file, c.id, JSON.stringify(c.args), c.oracle_args ? JSON.stringify(c.oracle_args) : '',
      ...Object.keys(versions).map(n => c.raw[n] === undefined ? '(did not run)' : c.raw[n]),
      (c.policy || []).join(' ')].map(cell).join(' | ') + ' |');
  }
  lines.push('');
  lines.push('Total: ' + disputes.length);
  fs.writeFileSync(file, lines.join('\n') + '\n');
}

if (require.main === module) main(process.argv.slice(2));

module.exports = { writeCaseFile, ORACLES };
