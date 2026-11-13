'use strict';

// Hand-written checks that no oracle can express: environment selection,
// export surface, resource guards, and the fixnum bound. Kept short on
// purpose; everything with a numeric answer belongs in cases/.

const test = require('node:test');
const assert = require('node:assert/strict');
const { loadLibrary, loadModule, JsNumsError } = require('./load');
const { evaluateInWorker } = require('./watchdog');

const libPath = process.env.JSNUMS_PATH || undefined;

test('jsbn uses 28-bit digits under this Node (navigator without a string appName)', () => {
  const { digitBits } = loadLibrary(libPath, undefined);
  assert.equal(digitBits, 28);
});

test('fake navigators select the 26-, 28- and 30-bit configurations', () => {
  for (const bits of [26, 28, 30]) assert.equal(loadLibrary(libPath, bits).digitBits, bits);
});

test('the wrapped module exports every name MakeNumberLibrary provides', () => {
  const mod = loadModule(libPath);
  const lib = mod.MakeNumberLibrary({});
  // FloatPoint and Complex are FIXME-marked aliases of Roughnum, not part of the surface
  const missing = Object.keys(lib).filter(k => !(k in mod) && k !== 'FloatPoint' && k !== 'Complex');
  assert.deepEqual(missing, []);
});

test('fixnum bound is +/-9e15: 9e15 is unboxed, 9e15+1 is a BigInteger', () => {
  const { lib } = loadLibrary(libPath, 28);
  assert.equal(lib.MAX_FIXNUM, 9e15);
  assert.equal(lib.MIN_FIXNUM, -9e15);
  assert.equal(typeof lib.fromString('9000000000000000'), 'number');
  assert.equal(typeof lib.fromString('-9000000000000000'), 'number');
  assert.ok(lib.fromString('9000000000000001') instanceof lib.BigInteger);
  assert.ok(lib.fromString('-9000000000000001') instanceof lib.BigInteger);
});

test('expt refuses exponents beyond 2^32-1 instead of running out of memory', async () => {
  // run in a worker: a library without the guard never returns
  for (const args of [['7', '1' + '0'.repeat(36789)], ['3/2', '4294967296'], ['2', '4294967296'], ['-7/3', '-4294967296']]) {
    const r = await evaluateInWorker(libPath, 28, 'expt', args);
    assert.equal(r.kind, 'error', 'expt(' + args.join(', ') + '): ' + r.text);
    assert.match(r.message, /too large/);
  }
});

test('errbacks are used for domain errors on BigInteger methods', () => {
  const { lib } = loadLibrary(libPath, 28);
  assert.throws(() => lib.makeBignum('-1').log(), e => e instanceof JsNumsError && e.tag === 'log-non-positive');
  assert.throws(() => lib.makeBignum('2').asin(), e => e instanceof JsNumsError && e.tag === 'domain-error');
  assert.throws(() => lib.makeBignum('-2').acos(), e => e instanceof JsNumsError && e.tag === 'domain-error');
});

test('BigIntegers from different parse paths are structurally equal', () => {
  const { lib } = loadLibrary(libPath, 28);
  assert.deepEqual(lib.fromString('1e30'), lib.makeBignum('1e30'));
  assert.deepEqual(lib.makeBignum('1e3'), lib.makeBignum('1000'));
  assert.deepEqual(lib.fromString('1e309'), lib.makeBignum('1' + '0'.repeat(309)));
});

test('sqrt of an exact non-square beyond double range is not a false exact integer', () => {
  const { lib } = loadLibrary(libPath, 28);
  const n = lib.fromString('1' + '0'.repeat(400) + '1');
  let r;
  try { r = lib.sqrt(n); } catch (e) { assert.ok(e instanceof JsNumsError); return; }
  assert.ok(!lib.isInteger(r), 'sqrt(10^400+1) returned exact integer ' + r);
});
