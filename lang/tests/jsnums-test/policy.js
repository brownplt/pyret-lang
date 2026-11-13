'use strict';

// Turns oracle consensus into js-numbers' expectation. Every departure from
// the oracles is a named rule; the freeze tool stamps the rule names on the
// case. Spec of record: docs/src/builtin/numbers.scrbl, then Racket.

const F = require('./numfmt');
const { OPS } = require('./jsops');

// Rough results of these ops are compared with an ulp tolerance (libm is
// not correctly rounded). The limit is a human decision; see the README.
const ULP_LIMIT = {
  sin: 1, cos: 1, tan: 1, asin: 1, acos: 1, atan: 1, atan2: 2,
  exp: 1, log: 2, expt: 2, sqrt: 0,
};

// Ops where js-numbers converts the exact side to a double before comparing
// (oracles compare exact against float exactly).
const MIXED_COMPARE_OPS = new Set(['lessThan', 'lessThanOrEqual', 'greaterThan', 'greaterThanOrEqual']);

// Ops where js-numbers checks the divisor before anything else, so a zero
// second argument is a division-by-zero error whatever the oracle did (IEEE
// infinity, NaN, Python ZeroDivisionError, Racket's exact 0 for (/ 0 -0.0),
// a domain error for a non-integer first argument, ...).
const ZERO_DIVISOR_OPS = new Set(['divide', 'remainder', 'makeRational']);
// Integer exponents beyond the fixnum range go to jsbn's bnpExp, which
// refuses exponents above 2^32-1 rather than computing e.g. (-1)^n.
const EXPT_EXPONENT_LIMIT = 9000000000000000n;

// Rules that trigger on (op, args) alone and overwrite an oracle's raw
// result without ever looking at it. For these, every oracle's own
// output gets replaced by the same value regardless of what it actually
// computed, so "all three oracles agreed" would be true by construction,
// not evidence. freeze.js stamps cases these rules touch with
// policy-override instead of policy+agreed (see README.md).
const OVERRIDE_RULES = new Set([
  'zero-divisor', 'rough-zero-exponent-ambiguous', 'trig-overflow-is-error',
  'expt-exponent-limit', 'min-value-tolerance-error',
]);

function numArg(kind, s) {
  if (kind === 'num' || kind === 'double') {
    if (F.isRoughLit(s)) return { rough: F.parseRough(s), lit: s };
    return { exact: F.parseExact(s), lit: s };
  }
  return { other: s };
}
function argsOf(op, args) { return OPS[op].kinds.map((k, i) => numArg(k, args[i])); }
const isExactZero = a => a && a.exact !== undefined && a.exact.n === 0n;
const isExactOne = a => a && a.exact !== undefined && a.exact.n === 1n && a.exact.d === 1n;
const isRoughZero = a => a && a.rough !== undefined && a.rough === 0;
const isZero = a => isExactZero(a) || isRoughZero(a);
const isNegative = a => a && (a.exact !== undefined ? a.exact.n < 0n : a.rough < 0);
const isPositive = a => a && (a.exact !== undefined ? a.exact.n > 0n : a.rough > 0);
const isRough = a => a && a.rough !== undefined;

// Pre-transform: the arguments the oracles evaluate.
function oracleArgs(op, args) {
  const rules = new Set();
  const out = args.slice();
  const A = argsOf(op, args);
  if (MIXED_COMPARE_OPS.has(op) && isRough(A[0]) !== isRough(A[1])) {
    const i = isRough(A[0]) ? 1 : 0;
    out[i] = F.roughLit(F.exactToDouble(A[i].exact));
    rules.add('mixed-compare-via-double');
  }
  return { args: out, rules: [...rules] };
}

// Harness output line -> result object (before policy).
function parseRaw(raw) {
  if (raw === 'skip') return null;
  if (raw === 'none') return { none: true };
  if (raw === 'div-by-zero') return { error: 'div-by-zero' };
  if (raw.startsWith('domain:')) return { error: 'error' };
  if (raw.startsWith('exact:')) {
    const s = raw.slice(6);
    F.parseExact(s);
    return { exact: s };
  }
  if (raw.startsWith('rough:')) {
    const x = F.bitsToDouble(raw.slice(6));
    return Number.isFinite(x) ? { rough: F.doubleToBits(x) } : { nonfinite: F.doubleToBits(x) };
  }
  if (raw === 'bool:true') return { bool: true };
  if (raw === 'bool:false') return { bool: false };
  if (raw.startsWith('string:')) return { string: raw.slice(7) };
  return { harnessError: raw };
}

function identityValue(op, A) {
  switch (op) {
    case 'sin': case 'tan': case 'asin': case 'atan': return isExactZero(A[0]) ? '0' : null;
    case 'cos': case 'exp': return isExactZero(A[0]) ? '1' : null;
    case 'log': case 'acos': return isExactOne(A[0]) ? '0' : null;
    case 'atan2': return isExactZero(A[0]) && isPositive(A[1]) ? '0' : null;
    case 'expt':
      if (isExactZero(A[1]) || isExactOne(A[0])) return '1';
      // expt(0, y) for y > 0 is exact 0 whether y is exact or rough (the
      // x===0 special case hardcodes it). A rough zero exponent is its
      // own case, forced to an error in normalize() above, not here.
      if (isExactZero(A[0]) && isPositive(A[1])) return '0';
      return null;
    default: return null;
  }
}

function roughIs(bits, lit) { return F.bitsToDouble(bits) === Number(lit); }

// Normalize one oracle's result under js-numbers' policy, before consensus.
function normalize(op, args, r) {
  const rules = [];
  if (r === null || r.harnessError) return { result: r, rules };
  const A = argsOf(op, args);
  if (op === 'expt' && isExactZero(A[0]) && isRoughZero(A[1])) {
    // 0^0 is a removable discontinuity: as a rough exponent it could be
    // 0 (giving 1), positive (giving 0), or negative (division by zero).
    // The oracles just apply IEEE pow(0,0)=1; js-numbers raises instead
    // (decided 2026-11-13, Joe Politz with Ben).
    r = { error: 'error' }; rules.push('rough-zero-exponent-ambiguous');
  }
  if (ZERO_DIVISOR_OPS.has(op) && isZero(A[1])) {
    r = { error: 'div-by-zero' }; rules.push('zero-divisor');
  }
  if (r.nonfinite || r.error) {
    if (op === 'expt' && isZero(A[0]) && isNegative(A[1])) {
      r = { error: 'div-by-zero' }; rules.push('zero-divisor');
    } else if (op === 'modulo' && isZero(A[1])) {
      r = { error: 'error' }; rules.push('modulo-zero-is-domain-error');
    } else if (r.nonfinite) {
      if (op === 'toFixnum') r = { double: r.nonfinite };
      else { r = { error: 'error' }; rules.push('no-nonfinite'); }
    }
  }
  if (r.rough && op === 'toFixnum') r = { double: r.rough };
  if (r.rough && op === 'add' && roughIs(r.rough, '0')) {
    const other = isExactZero(A[0]) && isRough(A[1]) ? A[1] : isExactZero(A[1]) && isRough(A[0]) ? A[0] : null;
    if (other && other.rough === 0) { r = { rough: F.doubleToBits(other.rough) }; rules.push('exact-zero-add-identity'); }
  }
  if (r.rough && roughIs(r.rough, '0') &&
      ((op === 'multiply' && (isExactZero(A[0]) || isExactZero(A[1]))) ||
       (op === 'divide' && isExactZero(A[0])))) {
    r = { exact: '0' }; rules.push('exact-zero-annihilates');
  }
  const idv = identityValue(op, A);
  if (idv !== null && r.rough && roughIs(r.rough, idv)) {
    r = { exact: idv }; rules.push('exact-at-identity');
  }
  if ((op === 'sin' || op === 'cos' || op === 'tan') && A[0].exact !== undefined && !Number.isFinite(F.exactToDouble(A[0].exact))) {
    r = { error: 'error' }; rules.push('trig-overflow-is-error');
  }
  if (op === 'expt' && A[0].exact !== undefined && !isExactZero(A[0]) && !isExactOne(A[0]) &&
      A[1].exact !== undefined && A[1].exact.d === 1n && F.bigAbs(A[1].exact.n) > EXPT_EXPONENT_LIMIT) {
    r = { error: 'error' }; rules.push('expt-exponent-limit');
  }
  if (op === 'roughlyEquals' && isRough(A[2]) && A[2].rough === Number.MIN_VALUE && (isRough(A[0]) || isRough(A[1]))) {
    const x = isRough(A[0]) ? A[0].rough : F.exactToDouble(A[0].exact);
    const y = isRough(A[1]) ? A[1].rough : F.exactToDouble(A[1].exact);
    if (Math.abs(x - y) === Number.MIN_VALUE) { r = { error: 'error' }; rules.push('min-value-tolerance-error'); }
  }
  return { result: r, rules };
}

// Post-transform: consensus -> js-numbers expectation and comparison mode.
function expectation(op, args, c) {
  const rules = [];
  const A = argsOf(op, args);
  let e = c;
  if (op === 'equals' && A.some(isRough)) {
    e = { error: 'error' }; rules.push('rough-equals-raises');
  }
  if ((op === 'numerator' || op === 'denominator') && isRough(A[0]) && e.exact) {
    const x = F.exactToDouble(F.parseExact(e.exact));
    if (Number.isFinite(x)) { e = { rough: F.doubleToBits(x) }; rules.push('rough-numden'); }
    else { e = { error: 'error' }; rules.push('rough-numden', 'no-nonfinite'); }
  }
  let mode = 'exact';
  if (op === 'toStringDigits') mode = 'decimal-string';
  else if (op === 'toRepeatingDecimal') mode = 'repeating';
  else if (e.rough && ULP_LIMIT[op] !== undefined) mode = 'ulp';
  return { expect: e, mode, rules };
}

function sameResult(a, b) {
  const ka = Object.keys(a).sort().join(), kb = Object.keys(b).sort().join();
  if (ka !== kb) return false;
  return Object.keys(a).every(k => a[k] === b[k]);
}

module.exports = {
  ULP_LIMIT, OVERRIDE_RULES, oracleArgs, parseRaw, normalize, expectation, sameResult, argsOf,
};
