'use strict';

const EXACT_RE = /^([+-]?\d+)(?:\/(\d+))?$/;
const ROUGH_RE = /^~(inf|-inf|nan|[+-]?(?:\d+\.?\d*|\.\d+)(?:[eE][+-]?\d+)?)$/;

function bigAbs(x) { return x < 0n ? -x : x; }
function bigGcd(a, b) {
  a = bigAbs(a); b = bigAbs(b);
  while (b !== 0n) { const t = a % b; a = b; b = t; }
  return a;
}
function bitLength(b) { return b === 0n ? 0 : bigAbs(b).toString(2).length; }

function exact(n, d) {
  if (d === undefined) d = 1n;
  if (d === 0n) throw new Error('exact: zero denominator');
  if (d < 0n) { n = -n; d = -d; }
  const g = bigGcd(n, d);
  return { n: g === 0n ? 0n : n / g, d: g === 0n ? 1n : d / g };
}

function isExactLit(s) { return EXACT_RE.test(s); }
function isRoughLit(s) { return ROUGH_RE.test(s); }

function parseExact(s) {
  const m = EXACT_RE.exec(s);
  if (!m) throw new Error('bad exact literal ' + JSON.stringify(s));
  const q = exact(BigInt(m[1]), m[2] === undefined ? 1n : BigInt(m[2]));
  if (exactLit(q) !== s.replace(/^\+/, '')) throw new Error('non-canonical exact literal ' + JSON.stringify(s));
  return q;
}

function exactLit(q) { return q.d === 1n ? q.n.toString() : q.n.toString() + '/' + q.d.toString(); }

function parseRough(s) {
  const m = ROUGH_RE.exec(s);
  if (!m) throw new Error('bad rough literal ' + JSON.stringify(s));
  const r = m[1];
  if (r === 'inf') return Infinity;
  if (r === '-inf') return -Infinity;
  if (r === 'nan') return NaN;
  return Number(r);
}

function roughLit(x) {
  if (Number.isNaN(x)) return '~nan';
  if (x === Infinity) return '~inf';
  if (x === -Infinity) return '~-inf';
  let s = Object.is(x, -0) ? '-0' : String(x);
  if (!/[.e]/.test(s)) s += '.0';
  return '~' + s;
}

function doubleToBits(x) {
  if (Number.isNaN(x)) return '7ff8000000000000';
  const dv = new DataView(new ArrayBuffer(8));
  dv.setFloat64(0, x);
  return dv.getBigUint64(0).toString(16).padStart(16, '0');
}

function bitsToDouble(h) {
  if (!/^[0-9a-f]{16}$/.test(h)) throw new Error('bad bits ' + h);
  const dv = new DataView(new ArrayBuffer(8));
  dv.setBigUint64(0, BigInt('0x' + h));
  return dv.getFloat64(0);
}

function doubleToExact(x) {
  if (!Number.isFinite(x)) throw new Error('doubleToExact: non-finite');
  const bits = BigInt('0x' + doubleToBits(x));
  const sign = (bits >> 63n) & 1n;
  const expo = Number((bits >> 52n) & 0x7ffn);
  const frac = bits & 0xfffffffffffffn;
  let m, e;
  if (expo === 0) { m = frac; e = -1074; } else { m = frac | 0x10000000000000n; e = expo - 1075; }
  if (sign === 1n) m = -m;
  return e >= 0 ? exact(m * (1n << BigInt(e))) : exact(m, 1n << BigInt(-e));
}

function exactToDouble(q) {
  let n = q.n, d = q.d;
  if (n === 0n) return 0;
  const neg = n < 0n;
  if (neg) n = -n;
  const shift = 55 - (bitLength(n) - bitLength(d));
  let num = n, den = d;
  if (shift > 0) num = n << BigInt(shift); else if (shift < 0) den = d << BigInt(-shift);
  const quo = num / den, rem = num % den;
  const e2 = bitLength(quo) - 1 - shift;
  if (e2 > 1023) return neg ? -Infinity : Infinity;
  if (e2 < -1075) return neg ? -0 : 0;
  const keep = Math.min(53, 1075 + e2);
  const drop = BigInt(bitLength(quo) - keep);
  let q2 = quo >> drop;
  const low = quo - (q2 << drop);
  const half = 1n << (drop - 1n);
  if (low > half || (low === half && (rem !== 0n || (q2 & 1n) === 1n))) q2 += 1n;
  if (q2 === 0n) return neg ? -0 : 0;
  let res = Number(q2);
  let s = e2 - keep + 1;
  while (s > 0) { res *= 2; s -= 1; }
  while (s < 0) { res *= 0.5; s += 1; }
  return neg ? -res : res;
}

function decimalToExact(s) {
  const m = /^([+-]?)(\d*)(?:\.(\d*))?(?:[eE]([+-]?\d+))?$/.exec(s);
  if (!m || (m[2] === '' && (m[3] === undefined || m[3] === ''))) throw new Error('bad decimal ' + s);
  const frac = m[3] || '';
  let n = BigInt((m[2] || '0') + frac);
  let e = (m[4] ? parseInt(m[4], 10) : 0) - frac.length;
  if (m[1] === '-') n = -n;
  return e >= 0 ? exact(n * 10n ** BigInt(e)) : exact(n, 10n ** BigInt(-e));
}

function ordinal(x) {
  const b = BigInt('0x' + doubleToBits(x));
  return (b >> 63n) === 1n ? -(b & 0x7fffffffffffffffn) : b;
}
function ulpDistance(a, b) {
  if (!Number.isFinite(a) || !Number.isFinite(b)) return (Object.is(a, b) ? 0n : null);
  return bigAbs(ordinal(a) - ordinal(b));
}

function integerRoot(n, k) {
  if (n < 0n) throw new Error('integerRoot: negative');
  if (n < 2n) return n;
  let x = BigInt(Math.floor(Math.pow(Number(n), 1 / k))) + 1n;
  while (x ** k > n) x -= 1n;
  while ((x + 1n) ** k <= n) x += 1n;
  return x;
}

module.exports = {
  EXACT_RE, ROUGH_RE, bigAbs, bigGcd, bitLength, exact, isExactLit, isRoughLit,
  parseExact, exactLit, parseRough, roughLit, doubleToBits, bitsToDouble,
  doubleToExact, exactToDouble, decimalToExact, ulpDistance, integerRoot,
};
