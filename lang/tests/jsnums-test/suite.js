'use strict';

const fs = require('fs');
const path = require('path');
const { loadLibrary, JsNumsError } = require('./load');
const { OPS } = require('./jsops');
const F = require('./numfmt');
const policy = require('./policy');

const CASES_DIR = path.join(__dirname, 'cases');
const MAX_FIXNUM = 9e15;

function caseFiles(dir) {
  return fs.readdirSync(dir || CASES_DIR).filter(f => f.endsWith('.json')).sort();
}
function readCaseFile(name, dir) {
  return JSON.parse(fs.readFileSync(path.join(dir || CASES_DIR, name), 'utf8'));
}

function buildInt(L, s) {
  const v = L.fromString(s);
  if (v === false) throw new Error('input construction: fromString rejected ' + s);
  return v;
}

function buildArg(L, kind, s) {
  switch (kind) {
    case 'num': {
      if (F.isRoughLit(s)) return L.makeRoughnum(F.parseRough(s));
      const q = F.parseExact(s);
      let v;
      if (q.d === 1n) v = buildInt(L, s);
      else v = new L.Rational(buildInt(L, q.n.toString()), buildInt(L, q.d.toString()));
      if (String(v) !== s) throw new Error('input construction: ' + s + ' became ' + String(v));
      return v;
    }
    case 'double': return F.parseRough(s);
    case 'str': return s;
    case 'bool': return s === 'true';
    default: throw new Error('unknown kind ' + kind);
  }
}

function errorClass(e) {
  if (e instanceof JsNumsError) {
    if (e.tag === 'div-by-zero') return 'div-by-zero';
    if (e.tag === 'internal-error') return 'internal';
    return 'error';
  }
  return 'crash';
}

function intOf(L, x) {
  if (typeof x === 'number') {
    if (!Number.isInteger(x)) return { n: F.doubleToExact(x).n, ok: false, why: 'non-integer JS number as integer part' };
    return { n: BigInt(x), ok: Math.abs(x) <= MAX_FIXNUM, why: 'unboxed integer beyond fixnum range' };
  }
  if (x instanceof L.BigInteger) {
    const n = BigInt(x.toString());
    return { n, ok: F.bigAbs(n) > BigInt(MAX_FIXNUM), why: 'BigInteger that fits a fixnum' };
  }
  return { n: null, ok: false, why: 'not an integer representation' };
}

// Classify a js-numbers value: kind, exact value or bits, representation,
// and whether the representation is the canonical one for the value.
function classify(L, v, resultType) {
  if (resultType === 'double') {
    if (typeof v !== 'number') return { kind: 'bad', why: 'toFixnum did not return a JS number' };
    return { kind: 'double', bits: F.doubleToBits(v), x: v };
  }
  if (v === false && resultType === 'numOrFalse') return { kind: 'none' };
  if (typeof v === 'boolean') return { kind: 'bool', value: v };
  if (typeof v === 'string') return { kind: 'string', value: v };
  if (Array.isArray(v)) return { kind: 'list', items: v.map(String) };
  if (typeof v === 'number') {
    if (!Number.isInteger(v)) return { kind: 'exact', q: F.doubleToExact(v), repr: 'fixnum', canonical: false, why: 'non-integer JS number as pyretnum' };
    const q = F.exact(BigInt(v));
    return { kind: 'exact', q, repr: 'fixnum', canonical: Math.abs(v) <= MAX_FIXNUM, why: 'unboxed integer beyond fixnum range', negZero: Object.is(v, -0) };
  }
  if (v instanceof L.BigInteger) {
    const i = intOf(L, v);
    return { kind: 'exact', q: F.exact(i.n), repr: 'bigint', canonical: i.ok, why: i.why };
  }
  if (v instanceof L.Rational) {
    const n = intOf(L, v.n), d = intOf(L, v.d);
    if (n.n === null || d.n === null) return { kind: 'bad', why: 'Rational with non-integer parts' };
    if (d.n === 0n) return { kind: 'bad', why: 'Rational with zero denominator' };
    const q = F.exact(n.n, d.n);
    const reduced = q.n === n.n && q.d === d.n;
    const canonical = n.ok && d.ok && reduced && d.n > 1n;
    const why = !n.ok ? 'numerator: ' + n.why : !d.ok ? 'denominator: ' + d.why : !reduced ? 'unreduced or negative denominator' : d.n === 1n ? 'Rational holding an integer' : '';
    return { kind: 'exact', q, repr: 'rational', canonical, why };
  }
  if (v instanceof L.Roughnum) {
    return { kind: 'rough', x: v.n, bits: F.doubleToBits(v.n), canonical: Number.isFinite(v.n), why: 'non-finite roughnum' };
  }
  return { kind: 'bad', why: 'not a pyretnum: ' + String(v) };
}

function evaluate(L, op, args) {
  const row = OPS[op];
  if (!row) throw new Error('unknown op ' + op);
  let built;
  try {
    built = row.kinds.map((k, i) => buildArg(L, k, args[i]));
  } catch (e) {
    return { kind: 'input-error', message: e.message };
  }
  try {
    const v = row.fn(L, ...built);
    return classify(L, v, row.result);
  } catch (e) {
    return { kind: 'error', cls: errorClass(e), message: e && e.message ? e.message : String(e), stack: e && e.stack };
  }
}

function describe(a) {
  switch (a.kind) {
    case 'exact': return F.exactLit(a.q) + ' [' + a.repr + (a.negZero ? ', -0' : '') + (a.canonical ? '' : ', NON-CANONICAL: ' + a.why) + ']';
    case 'rough': return F.roughLit(a.x) + ' [' + a.bits + ']';
    case 'double': return 'double ' + F.roughLit(a.x) + ' [' + a.bits + ']';
    case 'error': return 'error(' + a.cls + '): ' + a.message;
    case 'input-error': return 'input-error: ' + a.message;
    case 'bool': return String(a.value);
    case 'string': return JSON.stringify(a.value);
    case 'list': return JSON.stringify(a.items);
    case 'none': return 'false';
    default: return a.kind + ': ' + a.why;
  }
}
function describeExpect(e) {
  if (e.exact !== undefined) return e.exact;
  if (e.rough !== undefined) return F.roughLit(F.bitsToDouble(e.rough)) + ' [' + e.rough + ']';
  if (e.double !== undefined) return 'double ' + F.roughLit(F.bitsToDouble(e.double)) + ' [' + e.double + ']';
  if (e.error !== undefined) return 'error(' + e.error + ')';
  if (e.bool !== undefined) return String(e.bool);
  if (e.string !== undefined) return JSON.stringify(e.string);
  if (e.none) return 'false';
  return JSON.stringify(e);
}

function checkRepeating(items, q) {
  if (items.length !== 3) return 'expected 3 parts';
  const [whole, pre, rep] = items;
  if (!/^-?\d+$/.test(whole) || !/^\d*$/.test(pre) || !(/^\d+$/.test(rep) || rep === '...')) return 'malformed parts';
  const neg = whole.startsWith('-');
  const w = BigInt(whole.replace('-', ''));
  const preN = pre === '' ? 0n : BigInt(pre);
  if (rep === '...') {
    // expansion cut off at the limit: whole and the digits so far must be the truncated expansion
    const mag = F.exact(F.bigAbs(q.n), q.d);
    const scale = 10n ** BigInt(pre.length);
    const truncated = (mag.n * scale) / mag.d;
    if ((neg !== (q.n < 0n)) || truncated !== w * scale + preN) return 'truncated expansion does not match';
    return null;
  }
  const scale = 10n ** BigInt(pre.length);
  const repN = BigInt(rep), period = 10n ** BigInt(rep.length) - 1n;
  // value = w + (preN + repN/period) / scale
  let v = F.exact(w * scale * period + preN * period + repN, scale * period);
  if (neg) v = F.exact(-v.n, v.d);
  if (v.n !== q.n || v.d !== q.d) return 'reconstructs to ' + F.exactLit(v);
  for (let k = 1; k < rep.length; k++) {
    if (rep.length % k === 0 && rep === rep.slice(0, k).repeat(rep.length / k)) return 'repeating part not minimal';
  }
  if (pre.length > 0 && pre[pre.length - 1] === rep[rep.length - 1]) return 'non-repeating part not minimal';
  return null;
}

function compare(op, args, c, actual) {
  const e = c.expect;
  const mode = c.mode || 'exact';
  if (actual.kind === 'error' || actual.kind === 'input-error' || actual.kind === 'bad') {
    if (e.error !== undefined && actual.kind === 'error' && actual.cls === e.error) return { ok: true };
    return { ok: false, why: 'got ' + describe(actual) };
  }
  if (e.error !== undefined) return { ok: false, why: 'expected error, got ' + describe(actual) };
  if (mode === 'decimal-string') {
    if (actual.kind !== 'string') return { ok: false, why: 'got ' + describe(actual) };
    const d = Number(F.parseExact(args[1]).n);
    const re = d >= 1 ? new RegExp('^-?\\d+\\.\\d{' + d + '}$') : /^-?\d+$/;
    if (!re.test(actual.value)) return { ok: false, why: 'malformed decimal string ' + JSON.stringify(actual.value) };
    const q = F.decimalToExact(actual.value), x = F.parseExact(e.exact);
    if (q.n !== x.n || q.d !== x.d) return { ok: false, why: 'string ' + JSON.stringify(actual.value) + ' is ' + F.exactLit(q) + ', expected ' + e.exact };
    return { ok: true };
  }
  if (mode === 'repeating') {
    if (actual.kind !== 'list') return { ok: false, why: 'got ' + describe(actual) };
    const why = checkRepeating(actual.items, F.parseExact(e.exact));
    return why ? { ok: false, why: why + ': ' + describe(actual) } : { ok: true };
  }
  if (e.exact !== undefined) {
    if (actual.kind !== 'exact') return { ok: false, why: 'got ' + describe(actual) };
    const x = F.parseExact(e.exact);
    if (x.n !== actual.q.n || x.d !== actual.q.d) return { ok: false, why: 'got ' + describe(actual) };
    if (OPS[op].repr) {
      if (actual.repr !== OPS[op].repr) return { ok: false, why: 'expected a ' + OPS[op].repr + ': ' + describe(actual), representation: true };
    } else if (!actual.canonical) return { ok: false, why: 'non-canonical representation: ' + describe(actual), representation: true };
    return { ok: true, negZero: !!actual.negZero };
  }
  if (e.rough !== undefined) {
    if (actual.kind !== 'rough') return { ok: false, why: 'got ' + describe(actual) };
    if (!actual.canonical) return { ok: false, why: 'non-canonical: ' + describe(actual), representation: true };
    if (mode === 'ulp') {
      const ulp = F.ulpDistance(actual.x, F.bitsToDouble(e.rough));
      const limit = policy.ULP_LIMIT[op];
      if (ulp === null) return { ok: false, why: 'got ' + describe(actual) };
      if (ulp > BigInt(limit)) return { ok: false, why: 'off by ' + ulp + ' ulp (limit ' + limit + '): got ' + describe(actual), ulp };
      return { ok: true, ulp };
    }
    if (actual.bits !== e.rough) return { ok: false, why: 'got ' + describe(actual) };
    return { ok: true };
  }
  if (e.double !== undefined) {
    if (actual.kind !== 'double') return { ok: false, why: 'got ' + describe(actual) };
    if (actual.bits !== e.double) return { ok: false, why: 'got ' + describe(actual) };
    return { ok: true };
  }
  if (e.bool !== undefined) {
    return actual.kind === 'bool' && actual.value === e.bool ? { ok: true } : { ok: false, why: 'got ' + describe(actual) };
  }
  if (e.string !== undefined) {
    return actual.kind === 'string' && actual.value === e.string ? { ok: true } : { ok: false, why: 'got ' + describe(actual) };
  }
  if (e.none) {
    return actual.kind === 'none' ? { ok: true } : { ok: false, why: 'got ' + describe(actual) };
  }
  return { ok: false, why: 'unknown expectation ' + JSON.stringify(e) };
}

// Run the frozen cases of one file against a library instance.
function runCases(L, file, filter) {
  const out = [];
  for (const c of file.cases) {
    if (filter && !filter(c)) continue;
    const actual = evaluate(L, file.op, c.args);
    const r = compare(file.op, c.args, c, actual);
    out.push({ id: c.id, family: c.family, args: c.args, expect: c.expect, mode: c.mode, ok: r.ok, why: r.why, ulp: r.ulp, negZero: r.negZero, representation: r.representation, actual });
  }
  return out;
}

function summarize(results) {
  const s = { total: results.length, pass: 0, fail: 0, negZero: 0, maxUlp: 0n, failures: [] };
  for (const r of results) {
    if (r.ok) s.pass++; else { s.fail++; s.failures.push(r); }
    if (r.negZero) s.negZero++;
    if (r.ulp !== undefined && r.ulp > s.maxUlp) s.maxUlp = r.ulp;
  }
  return s;
}

function formatFailure(r) {
  return r.id + ' args=' + JSON.stringify(r.args) + ' expected=' + describeExpect(r.expect) + (r.mode && r.mode !== 'exact' ? ' (' + r.mode + ')' : '') + ' -> ' + r.why;
}

module.exports = {
  CASES_DIR, caseFiles, readCaseFile, buildArg, classify, evaluate, compare, runCases, summarize,
  describe, describeExpect, formatFailure, errorClass, loadLibrary,
};
